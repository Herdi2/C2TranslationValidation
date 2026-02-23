module Fuzzer.Grammar where

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
  deriving (Show, Eq)

instance Pretty JType where
  pretty JInt = pretty "int"
  pretty JLong = pretty "long"
  pretty JFloat = pretty "float"
  pretty JDouble = pretty "double"
  pretty JBool = pretty "bool"

-- Method <methodName> <retType> <parameters> <body>
data JMethod = JMethod String JType [(JType, String)] [JStmt] deriving (Show, Eq)

instance Pretty JMethod where
  pretty (JMethod methodName retType params body) =
    pretty retType <+> pretty methodName
      <> encloseSep lparen rparen (comma <> space) ((\(varTyp, varName) -> pretty varTyp <+> pretty varName) <$> params)
        <+> lbrace
      <> line
      <> indent 4 (align (vsep $ pretty <$> body))
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
  pretty (JBin binop expr1 expr2) = pretty expr1 <+> pretty binop <+> pretty expr2

data Lit
  = IntLit Integer
  | LongLit Integer
  | FloatLit Integer
  | DoubleLit Integer
  deriving (Show, Eq)

instance Pretty Lit where
  pretty (IntLit i) = pretty i
  pretty (LongLit l) = pretty l
  pretty (FloatLit f) = pretty f
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
