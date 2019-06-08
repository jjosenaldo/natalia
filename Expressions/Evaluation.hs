module Expressions.Evaluation where

import Lexical.Lexemes

-- | Implementation of binary operations
binary_eval :: Token -- ^ first operand
            -> Token -- ^ operator
            -> Token -- ^ second operand
            -> Token -- ^ result of the operation

-- Operator +
binary_eval (Int x p) (Plus _) (Int y _) = Int (x + y) p
binary_eval (Int x p) (Plus _) (Double y _) = Double ((fromIntegral x) + y) p
binary_eval (Double x p) (Plus _) (Int y _) = Double (x + (fromIntegral y)) p
binary_eval (Double x p) (Plus _) (Double y _) = Double (x + y) p
binary_eval _ (Plus p) _ = error ("ERROR at " ++ show(p) ++ ": the + operator expects two numbers.")

-- Operator - (binary)
binary_eval (Int x p) (Minus _) (Int y _) = Int (x - y) p
binary_eval (Int x p) (Minus _) (Double y _) = Double ((fromIntegral x) - y) p
binary_eval (Double x p) (Minus _) (Int y _) = Double (x - (fromIntegral y)) p
binary_eval (Double x p) (Minus _) (Double y _) = Double (x - y) p
binary_eval _ (Minus p) _ = error ("ERROR at " ++ show(p) ++ ": the binary - operator expects two numbers.")

-- Operator %
binary_eval (Int x _) (Mod _) (Int 0 p) = error ("ERROR at " ++ show(p) ++ ": division by zero.")
binary_eval (Int x p) (Mod _) (Int y _) = Int (rem x y) p
binary_eval _ (Mod p) _ = error ("ERROR at " ++ show(p) ++ ": the % operator expects two ints.")

-- Operator ^
binary_eval (Int 0 _) (Expo p) (Int 0 _) = error ("ERROR at " ++ show(p) ++ ": 0^0 is undefined.")
binary_eval (Int 0 _) (Expo p) (Double 0.0 _) = error ("ERROR at " ++ show(p) ++ ": 0^0 is undefined.")
binary_eval (Double 0.0 _) (Expo p) (Double 0.0 _) = error ("ERROR at " ++ show(p) ++ ": 0^0 is undefined.")
binary_eval (Double 0.0 _) (Expo p) (Int 0 _) = error ("ERROR at " ++ show(p) ++ ": 0^0 is undefined.")
binary_eval (Int x p) (Expo _) (Int y _) = 
    if y > 0 then Int (x ^ y) p
    else Double ((fromIntegral x) ** (fromIntegral y)) p
binary_eval (Double x p) (Expo _) (Int y _) = Double (x ** fromIntegral(y)) p
binary_eval (Int x p) (Expo _) (Double y _) = Double (fromIntegral(x) ** y) p
binary_eval (Double x p) (Expo _) (Double y _) = Double (x ** y) p
binary_eval _ (Expo p) _ = error ("ERROR at " ++ show(p) ++ ": the ^ operator expects two numbers.")

-- Operator /   
binary_eval _ (Div _) (Int 0 p) = error ("ERROR at " ++ show(p) ++ ": division by 0.")
binary_eval _ (Div _) (Double 0.0 p) = error ("ERROR at " ++ show(p) ++ ": division by 0.")
binary_eval (Int x p) (Div _) (Int y _) = 
    if (rem x y == 0) then Int (div x y) p
    else Double ((fromIntegral x) / (fromIntegral y)) p


-- | Implementation of binary operations
unary_eval :: Token -- ^ operator
            -> Token -- ^ operand
            -> Token -- ^ result of the operation

-- Operator - (unary)
unary_eval (Minus p) (Int x _) = Int (-x) p
unary_eval (Minus p) (Double x _) = Double (-x) p
unary_eval (Minus p) _ = error ("ERROR at " ++ show(p) ++ ": the unary - operator expects a number.")