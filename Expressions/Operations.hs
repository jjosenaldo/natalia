module Expressions.Operations where

-- natalia's modules
import Expressions.Grammar
import Lexical.Lexemes
import TypeValue.TypeValue

-- Haskell modules
import Text.Parsec

-- TODO
getReturnTypeOfBinOp :: BinOp -> Type -> Type -> Type 
getReturnTypeOfBinOp (CONSBinOp (Times p)) NatInt NatInt = NatInt
getReturnTypeOfBinOp (CONSBinOp (Times p)) NatDouble NatInt = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Times p)) NatInt NatDouble = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Times p)) NatDouble NatDouble = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Times p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator * expects two " ++ (getNameOfType NatDouble))

getReturnTypeOfBinOp (CONSBinOp (Div p)) NatInt NatInt = NatInt
getReturnTypeOfBinOp (CONSBinOp (Div p)) NatDouble NatInt = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Div p)) NatInt NatDouble = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Div p)) NatDouble NatDouble = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Div p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator / expects two " ++ (getNameOfType NatDouble))

getReturnTypeOfBinOp (CONSBinOp (Minus p)) NatInt NatInt = NatInt
getReturnTypeOfBinOp (CONSBinOp (Minus p)) NatDouble NatInt = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Minus p)) NatInt NatDouble = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Minus p)) NatDouble NatDouble = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Minus p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator - expects two " ++ (getNameOfType NatInt))

getReturnTypeOfBinOp (CONSBinOp (Plus p)) NatInt NatInt = NatInt
getReturnTypeOfBinOp (CONSBinOp (Plus p)) NatDouble NatInt = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Plus p)) NatInt NatDouble = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Plus p)) NatDouble NatDouble = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Plus p)) NatString NatString = NatString
getReturnTypeOfBinOp (CONSBinOp (Plus p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator + expects two " ++ (getNameOfType NatInt) ++ " or " ++ (getNameOfType NatString))

getReturnTypeOfBinOp (CONSBinOp (LessThan p)) NatInt NatInt = NatBool
getReturnTypeOfBinOp (CONSBinOp (LessThan p)) NatDouble NatInt = NatBool
getReturnTypeOfBinOp (CONSBinOp (LessThan p)) NatInt NatDouble = NatBool
getReturnTypeOfBinOp (CONSBinOp (LessThan p)) NatDouble NatDouble = NatBool
getReturnTypeOfBinOp (CONSBinOp (LessThan p)) (NatSet _) (NatSet _) = NatBool
getReturnTypeOfBinOp (CONSBinOp (LessThan p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator < expects two " ++ (getNameOfType NatDouble) ++ " or two " ++ (getNameOfType (NatSet NatGenType)))

getReturnTypeOfBinOp (CONSBinOp (LessEquals p)) NatInt NatInt = NatBool
getReturnTypeOfBinOp (CONSBinOp (LessEquals p)) NatDouble NatInt = NatBool
getReturnTypeOfBinOp (CONSBinOp (LessEquals p)) NatInt NatDouble = NatBool
getReturnTypeOfBinOp (CONSBinOp (LessEquals p)) NatDouble NatDouble = NatBool
getReturnTypeOfBinOp (CONSBinOp (LessEquals p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator <= expects two " ++ (getNameOfType NatDouble) ++ " or two " ++ (getNameOfType (NatSet NatGenType)))

getReturnTypeOfBinOp (CONSBinOp (GreaterThan p)) NatInt NatInt = NatBool
getReturnTypeOfBinOp (CONSBinOp (GreaterThan p)) NatDouble NatInt = NatBool
getReturnTypeOfBinOp (CONSBinOp (GreaterThan p)) NatInt NatDouble = NatBool
getReturnTypeOfBinOp (CONSBinOp (GreaterThan p)) NatDouble NatDouble = NatBool
getReturnTypeOfBinOp (CONSBinOp (GreaterThan p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator > expects two " ++ (getNameOfType NatDouble) ++ " or two " ++ (getNameOfType (NatSet NatGenType)))

getReturnTypeOfBinOp (CONSBinOp (GreaterEquals p)) NatInt NatInt = NatBool
getReturnTypeOfBinOp (CONSBinOp (GreaterEquals p)) NatDouble NatInt = NatBool
getReturnTypeOfBinOp (CONSBinOp (GreaterEquals p)) NatInt NatDouble = NatBool
getReturnTypeOfBinOp (CONSBinOp (GreaterEquals p)) NatDouble NatDouble = NatBool
getReturnTypeOfBinOp (CONSBinOp (GreaterEquals p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator >= expects two " ++ (getNameOfType NatDouble) ++ " or two " ++ (getNameOfType (NatSet NatGenType)))

getReturnTypeOfBinOp (CONSBinOp (And p)) NatBool NatBool = NatBool
getReturnTypeOfBinOp (CONSBinOp (And p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator && expects two " ++ (getNameOfType NatBool) )

getReturnTypeOfBinOp (CONSBinOp (Or p)) NatBool NatBool = NatBool
getReturnTypeOfBinOp (CONSBinOp (Or p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator && expects two " ++ (getNameOfType NatBool) )

getReturnTypeOfBinOp (CONSBinOp (Equals p)) _ _ = NatBool

getReturnTypeOfBinOp (CONSBinOp (Different p)) _ _ = NatBool

getReturnTypeOfBinOp (CONSBinOp (Expo p)) NatInt NatInt = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Expo p)) NatDouble NatInt = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Expo p)) NatInt NatDouble = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Expo p)) NatDouble NatDouble = NatDouble
getReturnTypeOfBinOp (CONSBinOp (Expo p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator ^ expects two " ++ (getNameOfType NatDouble))

getReturnTypeOfBinOp (CONSBinOp (Mod p)) NatInt NatInt = 
    error ("ERROR at " ++ show(p) ++ ": the operator % expects two " ++ (getNameOfType NatInt))

getReturnTypeOfUnOp :: UnOp -> Type -> Type 

getReturnTypeOfUnOp (CONSUnOp (Negation p)) NatBool = 
    NatBool
getReturnTypeOfUnOp (CONSUnOp (Negation p)) other = 
    error ("ERROR at " ++ show(p) ++ ": the ! operator expects a " ++  (getNameOfType NatBool) ++ " but you passed a " ++ (getNameOfType other))

getReturnTypeOfUnOp (CONSUnOp (Minus p)) NatDouble = 
    NatDouble
getReturnTypeOfUnOp (CONSUnOp (Minus p)) NatInt = 
    NatInt
getReturnTypeOfUnOp (CONSUnOp (Minus p)) other = 
    error ("ERROR at " ++ show(p) ++ ": the unary - operator expects a " ++  (getNameOfType NatDouble) ++ " or a " ++ (getNameOfType NatInt) ++ " but you passed a " ++ (getNameOfType other))

getReturnTypeOfUnOp (CONSUnOp (Uppersand p)) t =
    NatPointer t 