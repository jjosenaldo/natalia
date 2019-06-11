module Expressions.Evaluation where

import Lexical.Lexemes
import Memory.Memory
import Types.Types

-- | Implementation of binary operations
binary_eval :: Value -- ^ first operand
            -> Token -- ^ operator
            -> Value -- ^ second operand
            -> Value -- ^ result of the operation


--  OPERATORS WITH NUMERIC RESULT --------------------------------------------------------------------------------------
-- Operator +
binary_eval (ConsNatInt x) (Plus _) (ConsNatInt y) = ConsNatInt (x + y)
binary_eval (ConsNatInt x) (Plus _) (ConsNatDouble y) = ConsNatDouble ((fromIntegral x) + y)
binary_eval (ConsNatDouble x) (Plus _) (ConsNatInt y) = ConsNatDouble (x + (fromIntegral y))
binary_eval (ConsNatDouble x) (Plus _) (ConsNatDouble y) = ConsNatDouble (x + y)
binary_eval _ (Plus p) _ = error ("ERROR at " ++ show(p) ++ ": the + operator expects two numbers.")

-- Operator - (binary)
binary_eval (ConsNatInt x) (Minus _) (ConsNatInt y) = ConsNatInt (x - y)
binary_eval (ConsNatInt x) (Minus _) (ConsNatDouble y) = ConsNatDouble ((fromIntegral x) - y)
binary_eval (ConsNatDouble x) (Minus _) (ConsNatInt y) = ConsNatDouble (x - (fromIntegral y))
binary_eval (ConsNatDouble x) (Minus _) (ConsNatDouble y) = ConsNatDouble (x - y)
binary_eval _ (Minus p) _ = error ("ERROR at " ++ show(p) ++ ": the binary - operator expects two numbers.")

-- Operator %
binary_eval (ConsNatInt x) (Mod p) (ConsNatInt 0) = error ("ERROR at " ++ show(p) ++ ": division by zero.")
binary_eval (ConsNatInt x) (Mod _) (ConsNatInt y) = ConsNatInt (rem x y)
binary_eval _ (Mod p) _ = error ("ERROR at " ++ show(p) ++ ": the % operator expects two ints.")

-- Operator ^
binary_eval (ConsNatInt 0) (Expo p) (ConsNatInt 0) = error ("ERROR at " ++ show(p) ++ ": 0^0 is undefined.")
binary_eval (ConsNatInt 0) (Expo p) (ConsNatDouble 0.0) = error ("ERROR at " ++ show(p) ++ ": 0^0 is undefined.")
binary_eval (ConsNatDouble 0.0) (Expo p) (ConsNatDouble 0.0) = error ("ERROR at " ++ show(p) ++ ": 0^0 is undefined.")
binary_eval (ConsNatDouble 0.0) (Expo p) (ConsNatInt 0) = error ("ERROR at " ++ show(p) ++ ": 0^0 is undefined.")
binary_eval (ConsNatInt x) (Expo _) (ConsNatInt y) 
    | y > 0 = ConsNatInt (x ^ y)
    | otherwise = ConsNatDouble ((fromIntegral x) ** (fromIntegral y)) 
binary_eval (ConsNatDouble x) (Expo _) (ConsNatInt y) = ConsNatDouble (x ** fromIntegral(y))
binary_eval (ConsNatInt x) (Expo _) (ConsNatDouble y) = ConsNatDouble (fromIntegral(x) ** y) 
binary_eval (ConsNatDouble x) (Expo _) (ConsNatDouble y) = ConsNatDouble (x ** y) 
binary_eval _ (Expo p) _ = error ("ERROR at " ++ show(p) ++ ": the ^ operator expects two numbers.")

-- Operator /   
binary_eval _ (Div p) (ConsNatInt 0) = error ("ERROR at " ++ show(p) ++ ": division by 0.")
binary_eval _ (Div p) (ConsNatDouble 0.0) = error ("ERROR at " ++ show(p) ++ ": division by 0.")
binary_eval (ConsNatInt x) (Div _) (ConsNatInt y )  
    | (rem x y) == 0 = ConsNatInt (div x y)
    | otherwise = ConsNatDouble ((fromIntegral x) / (fromIntegral y))

-- Operator *
binary_eval (ConsNatInt x) (Times p) (ConsNatInt y) = ConsNatInt(x*y)
binary_eval (ConsNatDouble x) (Times p) (ConsNatInt y) = ConsNatDouble(x*fromIntegral(y))
binary_eval (ConsNatInt x) (Times p) (ConsNatDouble y) = ConsNatDouble(fromIntegral(x)*y)
binary_eval (ConsNatDouble x) (Times p) (ConsNatDouble y) = ConsNatDouble(x*y)
binary_eval _ (Times p) _ = error ("ERROR at " ++ show(p) ++ ": the * operator expects two numbers.")

--  OPERATORS WITH BOOLEAN RESULT --------------------------------------------------------------------------------------

binary_eval (ConsNatBool x) (And p) (ConsNatBool y) = ConsNatBool (x && y)
binary_eval _ (And p) _ = error ("ERROR at " ++ show(p) ++ ": the && operator expects two booleans.")

-- | Implementation of binary operations
unary_eval :: Token -- ^ operator
            -> Value -- ^ operand
            -> Value -- ^ result of the operation

-- Operator - (unary)
unary_eval (Minus p) (ConsNatInt x) = ConsNatInt (-x) 
unary_eval (Minus p) (ConsNatDouble x) = ConsNatDouble (-x) 
unary_eval (Minus p) _ = error ("ERROR at " ++ show(p) ++ ": the unary - operator expects a number.")

unary_eval (Negation p) (ConsNatBool x) = ConsNatBool (not(x)) 
unary_eval (Negation p) _ = error ("ERROR at " ++ show(p) ++ ": the unary ! operator expects a boolean.")