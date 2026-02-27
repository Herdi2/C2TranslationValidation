module Fuzzer.Grammar where

import Data.Word
import Prettyprinter

-- https://docs.oracle.com/javase/specs/jls/se25/html/jls-19.html
-- A grammar of the subset of Java that we allow

data JType
  = JInt
  | JLong
  | JFloat
  | JDouble
  | JBool
  -- JVar, "var" keyword, think C++ auto
  deriving (Show, Ord, Eq)

instance Pretty JType where
  pretty JInt = pretty "int"
  pretty JLong = pretty "long"
  pretty JFloat = pretty "float"
  pretty JDouble = pretty "double"
  pretty JBool = pretty "bool"

-- | Program <className> <compiled method>
data JProgram = JProgram Word64 String JMethod deriving (Show)

instance Pretty JProgram where
  pretty (JProgram seed className method@(JMethod methodName methodType params _ _)) =
    pretty "// Generated with seed"
      <+> pretty seed
      <> line
      <> pretty "class"
      <+> pretty className
      <+> lbrace
      <> line
      <> indent
        4
        ( pretty "public static void main(String[] args)"
            <+> lbrace
            <> line
            <> indent
              4
              ( pretty methodType
                  <+> pretty "checksum"
                  <+> equals
                  <+> pretty methodName
                  <> encloseSep
                    lparen
                    rparen
                    (comma <> space)
                    ( ( \(_, varType) ->
                          pretty $
                            case varType of
                              JInt -> IntLit 0
                              JLong -> LongLit 0
                              JFloat -> FloatLit 0
                              JDouble -> DoubleLit 0
                      )
                        <$> params
                    )
                  <> semi
                  <> line
                  <> pretty "System.out.println(checksum)"
                  <> semi
              )
            <> line
            <> rbrace
        )
      <> line
      <> line
      <> indent 4 (pretty method)
      <> line
      <> rbrace

-- | Method <methodName> <retType> <parameters> <body> <return expr>
data JMethod = JMethod String JType [(String, JType)] [JStmt] JExpr deriving (Show, Eq)

instance Pretty JMethod where
  pretty (JMethod methodName retType params body ret) =
    pretty "static"
      <+> pretty retType
      <+> pretty methodName
      <> encloseSep lparen rparen (comma <> space) ((\(varName, varType) -> pretty varType <+> pretty varName) <$> params)
      <+> lbrace
      <> line
      <> indent 4 (align (vsep $ pretty <$> body))
      <> line
      <> indent 4 (align $ pretty "return" <+> pretty ret <> semi)
      <> line
      <> rbrace

data JStmt
  = JDecl JType String JExpr
  | JAssign String JExpr
  | JIf JExpr JStmt (Maybe JStmt)
  | JBlock [JStmt]
  deriving (Show, Eq)

instance Pretty JStmt where
  pretty (JDecl varTyp varName expr) =
    pretty varTyp <+> pretty varName <+> equals <+> pretty expr <> semi
  pretty (JAssign varName expr) =
    pretty varName <+> equals <+> pretty expr <> semi
  pretty (JIf bExpr ifBody Nothing) =
    pretty "if"
      <> parens (pretty bExpr)
      <> line
      <> pretty ifBody
  pretty (JIf bExpr ifBody (Just elseBody)) =
    pretty "if"
      <> parens (pretty bExpr)
      <> line
      <> pretty ifBody
      <> pretty "else"
      <> pretty elseBody
  pretty (JBlock stmts) =
    lbrace
      <> line
      <> indent 4 (align $ vsep $ pretty <$> stmts)
      <> line
      <> rbrace

data JExpr
  = JVariable JType String
  | JConv JType JExpr
  | JConst JType Lit
  | JBin BOp JExpr JExpr
  deriving (Show, Eq)

instance Pretty JExpr where
  pretty (JVariable _varTyp varName) = pretty varName
  pretty (JConv varTyp expr) = parens (pretty varTyp) <+> pretty expr
  pretty (JConst _litTyp literal) = pretty literal
  pretty (JBin binop expr1 expr2) = parens (pretty expr1) <+> pretty binop <+> parens (pretty expr2)

data Lit
  = IntLit Integer
  | LongLit Integer
  | FloatLit Float
  | DoubleLit Double
  deriving (Show, Eq)

instance Pretty Lit where
  pretty (IntLit i) = pretty i
  pretty (LongLit l) = pretty l <> pretty "l"
  pretty (FloatLit f) = pretty f <> pretty "f"
  pretty (DoubleLit d) = pretty d

data BOp
  = -- Arithmetic operations
    JAdd
  | JSub
  | JMul
  | JDiv
  | JAnd
  | JOr
  | JLShift
  | JRShift
  | -- Logical operations
    JLAnd
  | JLOr
  | JEQ
  | JNE
  | JLT
  | JGT
  | JGTEQ
  | JLTEQ
  deriving (Show, Eq)

instance Pretty BOp where
  pretty (JAdd) = pretty "+"
  pretty (JSub) = pretty "-"
  pretty (JMul) = pretty "*"
  pretty (JDiv) = pretty "/"
  pretty (JAnd) = pretty "&"
  pretty (JOr) = pretty "|"
  pretty (JLShift) = pretty "<<"
  pretty (JRShift) = pretty ">>"
  pretty (JLAnd) = pretty "&&"
  pretty (JLOr) = pretty "||"
  pretty (JEQ) = pretty "=="
  pretty (JNE) = pretty "!="
  pretty (JLT) = pretty "<"
  pretty (JGT) = pretty ">"
  pretty (JGTEQ) = pretty ">="
  pretty (JLTEQ) = pretty "<="
