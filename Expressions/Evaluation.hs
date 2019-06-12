module Expressions.Evaluation where

import Lexical.Lexemes
import Memory.Memory
import TypeValue.TypeValue
import Types.Types

-- | Implementation of binary operations
binary_eval :: Value -- ^ first operand
            -> Token -- ^ operator
            -> Value -- ^ second operand
            -> Value -- ^ result of the operation

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates list = remDups list []

remDups :: (Eq a) => [a] -> [a] -> [a]
remDups [] _ = []
remDups (x:xs) list2
    | (x `elem` list2) = remDups xs list2
    | otherwise = x : remDups xs (x:list2)



--  OPERATORS WITH NUMERIC RESULT --------------------------------------------------------------------------------------
-- Operator +
binary_eval (ConsNatInt x) (Plus _) (ConsNatInt y) = ConsNatInt (x + y)
binary_eval (ConsNatInt x) (Plus _) (ConsNatDouble y) = ConsNatDouble ((fromIntegral x) + y)
binary_eval (ConsNatDouble x) (Plus _) (ConsNatInt y) = ConsNatDouble (x + (fromIntegral y))
binary_eval (ConsNatDouble x) (Plus _) (ConsNatDouble y) = ConsNatDouble (x + y)
binary_eval (ConsNatString x) (Plus _) (ConsNatString y) = ConsNatString (x ++ y)
binary_eval (ConsNatSet type1 x) (Plus _) (ConsNatSet type2 y)  
        | checkCompatibleTypes type1 type2 = ConsNatSet type1 (removeDuplicates (x ++ y))
        | checkCompatibleTypes type2 type1 = ConsNatSet type2 (removeDuplicates (x ++ y))
        | otherwise = error ("ERROR : You can't unite a set of " ++ show(type1) ++ " with a set of " ++
                             show(type2))

binary_eval _ (Plus p) _ = error ("ERROR at " ++ show(p) ++ ": the + operator expects two numbers.")

-- Operator - (binary)
binary_eval (ConsNatInt x) (Minus _) (ConsNatInt y) = ConsNatInt (x - y)
binary_eval (ConsNatInt x) (Minus _) (ConsNatDouble y) = ConsNatDouble ((fromIntegral x) - y)
binary_eval (ConsNatDouble x) (Minus _) (ConsNatInt y) = ConsNatDouble (x - (fromIntegral y))
binary_eval (ConsNatDouble x) (Minus _) (ConsNatDouble y) = ConsNatDouble (x - y)
binary_eval (ConsNatSet type1 x) (Minus _) (ConsNatSet type2 y)
        | checkCompatibleTypes type1 type2 = ConsNatSet type1 (remDups x y)
        | checkCompatibleTypes type2 type1 = ConsNatSet type2 (remDups x y)
        | otherwise = error ("ERROR : You can't operator a set of " ++ show(type1) ++ " with a set of " ++
                             show(type2))

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

-- Operator &&
binary_eval (ConsNatBool x) (And p) (ConsNatBool y) = ConsNatBool (x && y)
binary_eval _ (And p) _ = error ("ERROR at " ++ show(p) ++ ": the && operator expects two booleans.")

-- Operator ||
binary_eval (ConsNatBool x) (Or p) (ConsNatBool y) = ConsNatBool (x || y)
binary_eval _ (Or p) _ = error ("ERROR at " ++ show(p) ++ ": the || operator expects two booleans.")

-- Operator <
binary_eval (ConsNatInt x) (LessThan p) (ConsNatInt y) = ConsNatBool (x < y)
binary_eval (ConsNatDouble x) (LessThan p) (ConsNatDouble y) = ConsNatBool (x < y)
binary_eval (ConsNatInt x) (LessThan p) (ConsNatDouble y) = 
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binary_eval (ConsNatDouble x) (LessThan p) (ConsNatInt y) =
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binary_eval _ (LessThan p) _ = error ("ERROR at " ++ show(p) ++ ": the < operator expects two numbers.")

-- Operator >
binary_eval (ConsNatInt x) (GreaterThan p) (ConsNatInt y) = ConsNatBool (x > y)
binary_eval (ConsNatDouble x) (GreaterThan p) (ConsNatDouble y) = ConsNatBool (x > y)
binary_eval (ConsNatInt x) (GreaterThan p) (ConsNatDouble y) = 
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binary_eval (ConsNatDouble x) (GreaterThan p) (ConsNatInt y) =
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binary_eval _ (GreaterThan p) _ = error ("ERROR at " ++ show(p) ++ ": the > operator expects two numbers.")

-- Operator <=
binary_eval (ConsNatInt x) (LessEquals p) (ConsNatInt y) = ConsNatBool (x <= y)
binary_eval (ConsNatDouble x) (LessEquals p) (ConsNatDouble y) = ConsNatBool (x <= y)
binary_eval (ConsNatInt x) (LessEquals p) (ConsNatDouble y) = 
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binary_eval (ConsNatDouble x) (LessEquals p) (ConsNatInt y) =
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binary_eval _ (LessEquals p) _ = error ("ERROR at " ++ show(p) ++ ": the <= operator expects two numbers.")

-- Operator >=
binary_eval (ConsNatInt x) (GreaterEquals p) (ConsNatInt y) = ConsNatBool (x >= y)
binary_eval (ConsNatDouble x) (GreaterEquals p) (ConsNatDouble y) = ConsNatBool (x >= y)
binary_eval (ConsNatInt x) (GreaterEquals p) (ConsNatDouble y) = 
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binary_eval (ConsNatDouble x) (GreaterEquals p) (ConsNatInt y) =
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binary_eval _ (GreaterEquals p) _ = error ("ERROR at " ++ show(p) ++ ": the >= operator expects two numbers.")

-- Operator ==
binary_eval (ConsNatInt x) (Equals p) (ConsNatInt y) = ConsNatBool (x == y)
binary_eval (ConsNatDouble x) (Equals p) (ConsNatDouble y) = ConsNatBool (x == y)
binary_eval (ConsNatInt x) (Equals p) (ConsNatDouble y) = 
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binary_eval (ConsNatDouble x) (Equals p) (ConsNatInt y) =
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binary_eval _ (Equals p) _ = error ("ERROR at " ++ show(p) ++ ": the == operator expects two numbers.")

-- Operator !=
binary_eval (ConsNatInt x) (Difference p) (ConsNatInt y) = ConsNatBool (x /= y)
binary_eval (ConsNatDouble x) (Difference p) (ConsNatDouble y) = ConsNatBool (x /= y)
binary_eval (ConsNatInt x) (Difference p) (ConsNatDouble y) = 
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binary_eval (ConsNatDouble x) (Difference p) (ConsNatInt y) =
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binary_eval _ (Difference p) _ = error ("ERROR at " ++ show(p) ++ ": the != operator expects two numbers.")


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