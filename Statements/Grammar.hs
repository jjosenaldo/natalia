module Statements.Grammar where

-- natalia's modules
import Expressions.Grammar
import Lexical.Lexemes
import TypeValue.TypeValue

data Statement = 
    CONSStatementVarInit VarInit                            |
    CONSStatementPrint Print                                
    deriving (Eq, Show)

data Print = 
    CONSPrint Exp
    deriving (Eq, Show)

data VarInit = 
    CONSVarInit Type String Exp
    deriving (Eq, Show)

-- data VarAssign = 
--     CONSVarAssign Id Expression
--     deriving (Eq, Show)