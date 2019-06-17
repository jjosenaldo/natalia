module Statements.Grammar where

-- natalia's modules
import Expressions.Grammar
import Lexical.Lexemes
import TypeValue.TypeValue

data Statement = 
    CONSStatementVarInit VarInit   |
    CONSStatementPrint Print       |
    CONSStatementReturn Return     |                  
    CONSStatementBlock Block       |
    CONSStatementIf If             |
    CONSStatementIfElse IfElse     |
    CONSStatementWhile While
    deriving (Eq, Show)

data Return =
    CONSReturn Exp
    deriving (Eq, Show)

data Print = 
    CONSPrint Exp
    deriving (Eq, Show)

data VarInit = 
    CONSVarInit Type String Exp
    deriving (Eq, Show)

data Block = 
    CONSBlock [Statement]
    deriving (Eq, Show)

data If = 
    CONSIf Exp Block
    deriving (Eq, Show)

data IfElse = 
    CONSIfElse Exp Block Block
    deriving (Eq, Show)

data While =
    CONSWhile Exp Block
    deriving (Eq, Show)


getBlockStatements :: Block -> [Statement]
getBlockStatements (CONSBlock ss) = ss

-- data VarAssign = 
--     CONSVarAssign Id Expression
--     deriving (Eq, Show)