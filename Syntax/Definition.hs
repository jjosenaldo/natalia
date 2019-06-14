module Syntax.Definition where

-- natalia's modules
import Lexical.Lexemes
import TypeValue.TypeValue

data UnOperation = 
    CONSTokenUnOperation Token
    deriving (Eq, Show)

data Id = 
    CONSTokenId Token -- Id
    deriving (Eq, Show)

data Expression = 
    CONSValue Value Type | -- literals
    CONSId Id Type |
    CONSUnOperation UnOperation Expression Type 
    deriving (Eq, Show)

getSyntacticalUnitPos :: Expression -> (Int, Int)
getSyntacticalUnitPos (CONSValue x _) = getPosValue x 

getTypeOfExpression :: Expression -> Type
getTypeOfExpression (CONSValue _ x) = x
getTypeOfExpression (CONSId _ x) = x
getTypeOfExpression (CONSUnOperation _ _ x) = x

-- TODO: this function should search in the memory for the local variable.
-- | Returns the type of a local variable.
getTypeOfLocalVar :: String -- ^ the name of the local variable
                  -> Type -- ^ the type of the local variable
getTypeOfLocalVar idName = NatInt


getUnOperationExpectedType :: UnOperation -> Type 
getUnOperationExpectedType (CONSTokenUnOperation (Negation p)) = NatBool 
getUnOperationExpectedType (CONSTokenUnOperation (Minus p)) = NatDouble 

getUnOperationReturnType :: UnOperation -> Type -> Type
getUnOperationReturnType (CONSTokenUnOperation (Negation p)) NatBool = 
    NatBool
getUnOperationReturnType (CONSTokenUnOperation (Negation p)) other = 
    error ("ERROR at " ++ show(p) ++ ": the ! operator expects a " ++  (getNameOfType NatBool) ++ " but you passed a " ++ (getNameOfType other))

getUnOperationReturnType (CONSTokenUnOperation (Minus p)) NatDouble = 
    NatDouble
getUnOperationReturnType (CONSTokenUnOperation (Minus p)) NatInt = 
    NatInt
getUnOperationReturnType (CONSTokenUnOperation (Minus p)) other = 
    error ("ERROR at " ++ show(p) ++ ": the unary - operator expects a " ++  (getNameOfType NatDouble) ++ " or a " ++ (getNameOfType NatInt) ++ " but you passed a " ++ (getNameOfType other))

