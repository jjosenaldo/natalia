module Statements.Grammar where

-- natalia's modules
import Expressions.Grammar
import Lexical.Lexemes
import TypeValue.TypeValue

data Statement = 
    CONSStatementVarInit VarInit        |
    CONSStatementPrint Print            |
    CONSStatementReturn Return          |                  
    CONSStatementBlock Block
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


-- FIELD FUNCTIONS FOR BLOCKS ---------------------------------------------------------------------

getBlockStatements :: Block -> [Statement]
getBlockStatements (CONSBlock ss) = ss

-- FIELD FUNCTIONS FOR STATEMENTS ---------------------------------------------------------------------

getStatementReturn :: Statement -> Return
getStatementReturn (CONSStatementReturn r) = r
getStatementReturn s = error ("The statement" ++ (show s) ++  "is not a return!")

getStatementPrint :: Statement -> Print
getStatementPrint (CONSStatementPrint r) = r
getStatementPrint s = error ("The statement" ++ (show s) ++  "is not a print!")

-- FIELD FUNCTIONS FOR RETURNS ---------------------------------------------------------------------

getReturnExp :: Return -> Exp 
getReturnExp (CONSReturn expr ) = expr 

-- FIELD FUNCTIONS FOR PRINTS ---------------------------------------------------------------------

getPrintExp :: Print -> Exp 
getPrintExp (CONSPrint expr ) = expr 