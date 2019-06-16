module Expressions.Operations where

-- natalia's modules
import Expressions.Grammar
import Lexical.Lexemes
import TypeValue.TypeValue

-- Haskell modules
import Text.Parsec

-- TODO
getReturnTypeOfBinOp :: BinOp -> Type -> Type -> Type 
getReturnTypeOfBinOp _ _ _ = NatNothing

getReturnTypeOfUnOp :: UnOp -> Type -> Type 

-- Returns NatBool
getReturnTypeOfUnOp (CONSUnOp (Negation p)) NatBool = 
    NatBool
getReturnTypeOfUnOp (CONSUnOp (Negation p)) other = 
    error ("ERROR at " ++ show(p) ++ ": the ! operator expects a " ++  (getNameOfType NatBool) ++ " but you passed a " ++ (getNameOfType other))

-- Returns NatDouble or NatInt
getReturnTypeOfUnOp (CONSUnOp (Minus p)) NatDouble = 
    NatDouble
getReturnTypeOfUnOp (CONSUnOp (Minus p)) NatInt = 
    NatInt
getReturnTypeOfUnOp (CONSUnOp (Minus p)) other = 
    error ("ERROR at " ++ show(p) ++ ": the unary - operator expects a " ++  (getNameOfType NatDouble) ++ " or a " ++ (getNameOfType NatInt) ++ " but you passed a " ++ (getNameOfType other))

getReturnTypeOfUnOp (CONSUnOp (Uppersand p)) t =
    NatPointer t 