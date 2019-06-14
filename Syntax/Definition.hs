module Syntax.Definition where

-- natalia's modules
import Lexical.Lexemes
import Memory.Memory
import TypeValue.TypeValue

data Block = 
    CONSBlock [Statement]
    deriving (Eq, Show)

data Statement = 
    CONSStatementVarInit VarInit                            |
    CONSStatementVarAssign VarAssign                        |
    CONSStatementPrint Print                                |
    CONSStatementBlock Block                                |
    CONSStatementIf Expression Block                        |
    CONSStatementIfElse Expression Block Block              |
    CONSStatementWhile Expression Block
    deriving (Eq, Show)

data Print = 
    CONSPrint Expression
    deriving (Eq, Show)

data VarInit = 
    CONSVarInit Type Id Expression
    deriving (Eq, Show)

data VarAssign = 
    CONSVarAssign Id Expression
    deriving (Eq, Show)

data UnOperator = 
    CONSTokenUnOperator Token
    deriving (Eq, Show)

data BinOperator =
    CONSTokenBinOperator Token 
    deriving (Eq, Show)

data Id = 
    CONSTokenId Token -- Id
    deriving (Eq, Show)

data Expression = 
    CONSValue Value Type | -- literals
    CONSId Id Type |
    CONSUnOperation UnOperator Expression Type |
    CONSBinOperation BinOperator Expression Expression Type 
    deriving (Eq, Show)

getSyntacticalUnitPos :: Expression -> (Int, Int)
getSyntacticalUnitPos (CONSValue x _) = getPosValue x 

getTypeOfExpression :: Expression -> Type
getTypeOfExpression (CONSValue _ x) = x
getTypeOfExpression (CONSId _ x) = x
getTypeOfExpression (CONSUnOperation _ _ x) = x
getTypeOfExpression (CONSBinOperation _ _ _ x) = x

getBlockStatements :: Block -> [Statement]
getBlockStatements (CONSBlock x) = x

-- TODO: this function should search in the memory for the local variable.
-- | Returns the type of a local variable.
getTypeOfLocalVar :: String -- ^ the name of the local variable
                  -> Type -- ^ the type of the local variable
getTypeOfLocalVar idName = NatInt


getUnOperatorExpectedType :: UnOperator -> Type 
getUnOperatorExpectedType (CONSTokenUnOperator (Negation p)) = NatBool 
getUnOperatorExpectedType (CONSTokenUnOperator (Minus p)) = NatDouble 

getUnOperatorReturnType :: UnOperator -> Type -> Type
getUnOperatorReturnType (CONSTokenUnOperator (Negation p)) NatBool = 
    NatBool
getUnOperatorReturnType (CONSTokenUnOperator (Negation p)) other = 
    error ("ERROR at " ++ show(p) ++ ": the ! operator expects a " ++  (getNameOfType NatBool) ++ " but you passed a " ++ (getNameOfType other))

getUnOperatorReturnType (CONSTokenUnOperator (Minus p)) NatDouble = 
    NatDouble
getUnOperatorReturnType (CONSTokenUnOperator (Minus p)) NatInt = 
    NatInt
getUnOperatorReturnType (CONSTokenUnOperator (Minus p)) other = 
    error ("ERROR at " ++ show(p) ++ ": the unary - operator expects a " ++  (getNameOfType NatDouble) ++ " or a " ++ (getNameOfType NatInt) ++ " but you passed a " ++ (getNameOfType other))

getBinOperatorReturnType :: BinOperator -> Type -> Type -> Type 
getBinOperatorReturnType (CONSTokenBinOperator (Times p)) NatInt NatInt = NatInt
getBinOperatorReturnType (CONSTokenBinOperator (Times p)) NatDouble NatInt = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Times p)) NatInt NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Times p)) NatDouble NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Times p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator * expects two " ++ (getNameOfType NatDouble))

getBinOperatorReturnType (CONSTokenBinOperator (Div p)) NatInt NatInt = NatInt
getBinOperatorReturnType (CONSTokenBinOperator (Div p)) NatDouble NatInt = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Div p)) NatInt NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Div p)) NatDouble NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Div p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator / expects two " ++ (getNameOfType NatDouble))

getBinOperatorReturnType (CONSTokenBinOperator (Mod p)) NatInt NatInt = 
    error ("ERROR at " ++ show(p) ++ ": the operator % expects two " ++ (getNameOfType NatInt))

getBinOperatorExpectedSecondType :: BinOperator -> Type -> Type
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Times p)) NatInt = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Times p)) NatDouble = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Times p)) _ = 
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator * must be a " ++ (getNameOfType NatInt) ++ " or a " ++ (getNameOfType NatDouble))

getBinOperatorExpectedSecondType (CONSTokenBinOperator (Div p)) NatInt = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Div p)) NatDouble = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Div p)) _ = 
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator / must be a " ++ (getNameOfType NatInt) ++ " or a " ++ (getNameOfType NatDouble))

getBinOperatorExpectedSecondType (CONSTokenBinOperator (Mod p)) NatInt = NatInt
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Mod p)) _ = 
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator % must be a " ++ (getNameOfType NatInt))