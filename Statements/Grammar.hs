module Statements.Grammar where

-- natalia's modules
import Expressions.Grammar
import Lexical.Lexemes
import TypeValue.TypeValue

data Statement = 
    CONSStatementVarInit VarInit       | -- SYNT
    CONSStatementPrint Print           | -- SYNT, SEM 
    CONSStatementReturn Return         | -- SYNT                   
    CONSStatementBlock Block           | -- SYNT, SEM
    CONSStatementIf If                 | -- SYNT, SEM 
    CONSStatementIfElse IfElse         | -- SYNT 
    CONSStatementWhile While           | -- SYNT, 
    CONSStatementProcCall ProcCall     | -- SYNT
    CONSStatementAssignment Assignment
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

data ProcCall = 
    CONSProcCall String [Exp]
    deriving (Eq, Show)

data Assignment = 
    CONSAssignment LValue Exp
    deriving (Eq, Show)

getBlockStatements :: Block -> [Statement]
getBlockStatements (CONSBlock ss) = ss

-- FIELD FUNCTIONS FOR STATEMENTS ---------------------------------------------------------------------

getStatementReturn :: Statement -> Maybe Return
getStatementReturn (CONSStatementReturn r) = Just r
getStatementReturn s = Nothing --fail ("The statement" ++ (show s) ++  "is not a return!")

getStatementPrint :: Statement -> Maybe Print
getStatementPrint (CONSStatementPrint r) = Just r
getStatementPrint s = Nothing --fail ("The statement" ++ (show s) ++  "is not a print!")

getStatementVarInit :: Statement -> Maybe VarInit
getStatementVarInit (CONSStatementVarInit r) = Just r
getStatementVarInit s = Nothing --fail ("The statement" ++ (show s) ++  "is not a VarInit!")

getStatementIfElse :: Statement -> Maybe IfElse
getStatementIfElse (CONSStatementIfElse r) = Just r
getStatementIfElse s = Nothing -- fail ("The statement" ++ (show s) ++  "is not a IfElse!")

getStatementIf :: Statement -> Maybe If
getStatementIf (CONSStatementIf r) = Just r
getStatementIf s = Nothing -- fail ("The statement" ++ (show s) ++  "is not a If!")

getStatementBlock :: Statement -> Maybe Block
getStatementBlock (CONSStatementBlock r) = Just r
getStatementBlock s = Nothing -- fail ("The statement" ++ (show s) ++  "is not a Block!")

-- FIELD FUNCTIONS FOR RETURNS ---------------------------------------------------------------------

getReturnExp :: Return -> Exp 
getReturnExp (CONSReturn expr ) = expr 

-- FIELD FUNCTIONS FOR PRINTS ---------------------------------------------------------------------

getPrintExp :: Print -> Exp 
getPrintExp (CONSPrint expr ) = expr 

-- FIELD FUNCTIONS FOR IFELSES -----------------------------------------------------------------------

getIfElseExp :: IfElse -> Exp 
getIfElseExp (CONSIfElse ex _ _ ) = ex 

getIfElseBlock1 :: IfElse -> Block 
getIfElseBlock1 (CONSIfElse _ blk1 _) = blk1

getIfElseBlock2 :: IfElse -> Block 
getIfElseBlock2 (CONSIfElse _ _ blk2) = blk2

-- FIELD FUNCTIONS FOR IFS -----------------------------------------------------------------------

getIfExp :: If -> Exp 
getIfExp (CONSIf ex  _ ) = ex 

getIfBlock :: If -> Block 
getIfBlock (CONSIf _ blk1 ) = blk1


-- FIELD FUNCTIONS FOR BLOCKS -----------------------------------------------------------------------

getBlockStatementList :: Block -> [Statement]
getBlockStatementList (CONSBlock l) = l

-- FIELD FUNCTIONS FOR VAR INITS -----------------------------------------------------------------------

getVarInitType :: VarInit -> Type
getVarInitType (CONSVarInit t _ _) = t

getVarInitName :: VarInit -> String
getVarInitName (CONSVarInit _ n _) = n 

getVarInitExp :: VarInit -> Exp
getVarInitExp (CONSVarInit _ _ expr) = expr 
