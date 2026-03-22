{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Fuzzer.Gen where

import Control.Monad
import qualified Data.Map as M
import Data.Word
import Effectful
import Effectful.Labeled
import qualified Effectful.Labeled.Reader as LR
import Effectful.NonDet
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Fuzzer.GenUtils
import Fuzzer.Grammar
import Fuzzer.RNG

-- | We keep track of declared variables through their types,
-- to be able to pick them when possible/needed during program generation.
type DeclaredVars = M.Map JType [String]

type VarScope = [DeclaredVars]

type GenEffects =
  '[ RNG, -- Randomness, for choosing generators
     Reader (Maybe JType), -- Keeps track of current type when generating expressions
     State VarScope, -- Keeps track of declared variables, can then be used in generation
     State Integer, -- Used for fresh identifiers
     ExprDepth,
     StmtDepth,
     NonDet -- Allows for generators to fail, which allows a generator to back track and try another one
   ]

type Gen a = Eff GenEffects a

maxExprDepth :: Integer
maxExprDepth = 3

maxStmtDepth :: Integer
maxStmtDepth = 1

runGen :: Word64 -> Gen a -> Either CallStack a
runGen seed =
  runPureEff
    . runNonDet OnEmptyRollback
    . LR.runReader @"StmtFuel" maxStmtDepth
    . LR.runReader @"ExprFuel" maxExprDepth
    . evalState 0
    . evalState [M.empty]
    . runReader Nothing
    . runRNG seed

withScope :: (State VarScope :> es) => Eff es a -> Eff es a
withScope gen =
  do
    modify @VarScope (M.empty :)
    res <- gen
    modify @VarScope tail
    return res

-- | Declares a new variable with given name and type in the current scope
declareVar :: (State VarScope :> es) => String -> JType -> Eff es ()
declareVar varName varType =
  do
    scopes <- get
    case scopes of
      [] -> error "declareVar: Tried to declare variable in an empty scope"
      (currScope : rest) -> put (M.insertWith (++) varType [varName] currScope : rest)

-- | Get all variables of a given type in the current scope.
getVars :: (State VarScope :> es) => JType -> Eff es [String]
getVars jtyp =
  do
    scopes <- get @VarScope
    when (null scopes) (error "getVars: Tried to get variables of an empty scope")
    return $ join $ M.findWithDefault [] jtyp <$> scopes

-- | Get the type of the expression we are currently generating.
-- NOTE: Partial function.
getType :: (Reader (Maybe JType) :> es) => Eff es JType
getType =
  ask
    >>= \case
      Nothing -> error "getType: No type has been set"
      Just t -> return t

freshIdent :: (State Integer :> es) => Eff es Integer
freshIdent =
  do
    curr <- get
    put (curr + 1)
    return curr

freshVar :: (State Integer :> es) => Eff es String
freshVar = ((++) "v_" . show) <$> freshIdent

allVars :: (State VarScope :> es) => Eff es [(String, JType)]
allVars = go [JInt, JLong, JFloat, JDouble]
  where
    go [] = return []
    go (t : typs) =
      do
        rest <- go typs
        vars <- getVars t
        return $ ((,t) <$> vars) ++ rest

withType :: (Reader (Maybe JType) :> es) => JType -> Eff es a -> Eff es a
withType typ = local (const $ Just typ)

guardType :: (Reader (Maybe JType) :> es, NonDet :> es) => JType -> Eff es ()
guardType wantedType =
  do
    t <- getType
    when (t /= wantedType) empty

program :: Word64 -> String -> String -> Either String JProgram
program seed className methodName =
  let prog =
        runGen seed $
          (method methodName)
            >>= return . JProgram seed className
   in case prog of
        Left _ -> Left "Failed to generate program"
        Right p -> Right p

method :: String -> Gen JMethod
method methodName =
  do
    paramCount <- randR (1, 4)
    params <-
      sequence $
        replicate paramCount $
          do
            varName <- freshVar
            varType <- genArithmeticType
            return (varName, varType)
    mapM_ (uncurry declareVar) params
    stmtCount <- randR (2, 5)
    methodBody <-
      putStmtFuel maxStmtDepth $
        sequence $
          replicate stmtCount stmt
    -- Initial design choice: Calculate checksum of all variables?
    let retType = JDouble
    ret <- withType retType $ mkReturn
    return $ JMethod methodName retType params methodBody ret

-- | Creates the return statement of a method.
-- Since our verification condition is highly dependent on comparing return values,
-- this is similar to a checksum, summing all variables and returning it as a value.
mkReturn :: Gen JExpr
mkReturn =
  do
    vars <- allVars
    retType <- getType
    go vars retType
  where
    go [] _ = return $ JConst JDouble (DoubleLit 0)
    go [(varName, varType)] retType =
      return $
        if varType == retType
          then JVariable varType varName
          else JConv retType (JVariable varType varName)
    go ((varName, varType) : rest) retType =
      do
        let term =
              if varType == retType
                then JVariable varType varName
                else JConv retType (JVariable varType varName)
        bop <- binOp
        r <- go rest retType
        return $ JBin bop term r

stmt :: Gen JStmt
stmt =
  putExprFuel maxExprDepth $
    weightedM
      [ (declare, 0.5),
        (assign, 0.3),
        (ifElseStmt, 0.1),
        (ifStmt, 0.1)
      ]

ifStmt :: Gen JStmt
ifStmt =
  do
    boolGuard <- withType JBool $ logicalExpr
    ifBody <- blockStmt
    return $ JIf boolGuard ifBody Nothing

ifElseStmt :: Gen JStmt
ifElseStmt =
  do
    boolGuard <- withType JBool $ boolExpr
    ifBody <- blockStmt
    elseBody <- blockStmt
    return $ JIf boolGuard ifBody (Just elseBody)

blockStmt :: Gen JStmt
blockStmt =
  withStmtFuel $
    withScope $
      do
        stmtCount <- randR (1, 4)
        JBlock <$> sequence (replicate stmtCount stmt)

-- | Generates a boolean expression
boolExpr :: Gen JExpr
boolExpr =
  guardType JBool
    >> weightedM
      [ (logicalConst, 0.5),
        (logicalExpr, 0.3),
        (compExpr, 0.2)
      ]

-- | Generates a boolean constant True or False.
logicalConst :: Gen JExpr
logicalConst =
  guardType JBool
    >> (JConst JBool . BoolLit) <$> choose [True, False]

-- | Generates a logical expression between booleans
-- e.g. True && (x > 1)
logicalExpr :: Gen JExpr
logicalExpr =
  withExprFuel $
    do
      guardType JBool
      e1 <- withType JBool boolExpr
      e2 <- withType JBool boolExpr
      op <- choose [JLAnd, JLOr, JEQ, JNE]
      return $ JBin op e1 e2

-- | Generates comparisons between arithmetic expressions.
-- e.g. (1 + 2) > 8
compExpr :: Gen JExpr
compExpr =
  withExprFuel $
    do
      guardType JBool
      typ <- genArithmeticType
      e1 <- withType typ expr
      e2 <- withType typ expr
      op <- choose [JEQ, JNE, JLT, JGT, JGTEQ, JLTEQ]
      return $ JBin op e1 e2

-- | Weighted choise of arithmetic types (Int, Long, Float, Double)
genArithmeticType :: Gen JType
genArithmeticType =
  weighted
    [ (JInt, 0.40),
      (JLong, 0.35),
      (JFloat, 0.15),
      (JDouble, 0.1)
    ]

declare :: Gen JStmt
declare =
  do
    varName <- freshVar
    varTyp <- genArithmeticType
    varExpr <- withType varTyp $ expr
    declareVar varName varTyp
    return $ JDecl varTyp varName varExpr

assign :: Gen JStmt
assign =
  do
    vars <- allVars
    when (null vars) empty
    (varName, varType) <- choose vars
    varExpr <- withType varType expr
    return $ JAssign varName varExpr

expr :: Gen JExpr
expr =
  weightedM
    [ (var, 0.15),
      (constExpr, 0.40),
      (arithmeticExpr, 0.30)
    ]

var :: Gen JExpr
var =
  do
    jtyp <- getType
    vars <- getVars jtyp
    chosenVar <-
      if null vars
        then empty
        else choose vars
    return $ JVariable jtyp chosenVar

-- | Generates a constant expression
-- NOTE: Reader env has to contain a type for the constant to be generated
-- TODO: Add weighted choice for more interesting values, e.g. those close to MIN/MAX
constExpr :: Gen JExpr
constExpr =
  do
    jtyp <- ask
    case jtyp of
      Just JInt ->
        do
          -- Integers in Java are 32 bit
          lit <- IntLit <$> randR (-2 ^ 31, 2 ^ 31 - 1)
          return $ JConst JInt lit
      Just JLong ->
        do
          -- Longs in Java are 64 bit
          lit <- LongLit <$> randR (-2 ^ 63, 2 ^ 63 - 1)
          return $ JConst JLong lit
      Just JFloat ->
        do
          -- Floating point values are generated in binary representation
          lit <- FloatLit <$> randR (-100.0, 100.0)
          return $ JConst JFloat lit
      Just JDouble ->
        do
          -- Floating point values are generated in binary representation
          lit <- DoubleLit <$> randR (-100.0, 100.0)
          return $ JConst JDouble lit
      Just JBool -> empty -- constExpr does not generate booleans
      Nothing -> error "constExpr: Tried to generate a constant without specified type"

arithmeticExpr :: Gen JExpr
arithmeticExpr =
  withExprFuel $
    do
      bop <- binOp
      e1 <- expr
      e2 <- expr
      return $ JBin bop e1 e2

-- | Pick a binary operation which fits with the current type in the Reader env
binOp :: Gen BOp
binOp =
  ask
    >>= \case
      Just JInt -> choose [JAdd, JSub, JMul, JDiv, JAnd, JOr, JLShift, JRShift]
      Just JLong -> choose [JAdd, JSub, JMul, JDiv, JAnd, JOr, JLShift, JRShift]
      Just JFloat -> choose [JAdd, JSub, JMul, JDiv]
      Just JDouble -> choose [JAdd, JSub, JMul, JDiv]
      Nothing -> error "binOp: Tried to generate a binary op without specified type"
