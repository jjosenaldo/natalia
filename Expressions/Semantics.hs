module Expressions.Semantics where 

-- natalia's modules
import Expressions.Grammar
import Expressions.Typing
import Lexical.Lexemes
import Memory.Memory
import PredefBlocks.Grammar
import Program.Grammar
import Program.Parser
import Statements.Grammar
import Types.Types
import TypeValue.TypeValue
import Value.Value

-- Haskell's modules
import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Text.Parsec
import Data.List

-- | Implementation of binary operations
binaryEval :: Value -- ^ first operand
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

subList :: Eq a => [a] -> [a] -> Bool
subList [] [] = True
subList _ []    = False
subList [] _    = True
subList (x:xs) (y:ys) 
    | x == y    = subList xs ys   
    | otherwise = subList (x:xs) ys


--  OPERATORS WITH NUMERIC RESULT --------------------------------------------------------------------------------------
-- Operator +
binaryEval (ConsNatInt x) (Plus _) (ConsNatInt y) = ConsNatInt (x + y)
binaryEval (ConsNatInt x) (Plus _) (ConsNatDouble y) = ConsNatDouble ((fromIntegral x) + y)
binaryEval (ConsNatDouble x) (Plus _) (ConsNatInt y) = ConsNatDouble (x + (fromIntegral y))
binaryEval (ConsNatDouble x) (Plus _) (ConsNatDouble y) = ConsNatDouble (x + y)
binaryEval (ConsNatString x) (Plus _) (ConsNatString y) = ConsNatString (x ++ y)
binaryEval (ConsNatSet type1 x) (Plus _) (ConsNatSet type2 y)  
        | checkCompatibleTypes type1 type2 = ConsNatSet type1 (removeDuplicates (x ++ y))
        | checkCompatibleTypes type2 type1 = ConsNatSet type2 (removeDuplicates (x ++ y))
        | otherwise = error ("ERROR : You can't unite a set of " ++ show(type1) ++ " with a set of " ++
                             show(type2))

binaryEval _ (Plus p) _ = error ("ERROR at " ++ show(p) ++ ": the + operator expects two numbers.")

-- Operator - (binary)
binaryEval (ConsNatInt x) (Minus _) (ConsNatInt y) = ConsNatInt (x - y)
binaryEval (ConsNatInt x) (Minus _) (ConsNatDouble y) = ConsNatDouble ((fromIntegral x) - y)
binaryEval (ConsNatDouble x) (Minus _) (ConsNatInt y) = ConsNatDouble (x - (fromIntegral y))
binaryEval (ConsNatDouble x) (Minus _) (ConsNatDouble y) = ConsNatDouble (x - y)
binaryEval (ConsNatSet type1 x) (Minus _) (ConsNatSet type2 y)
        | checkCompatibleTypes type1 type2 = ConsNatSet type1 (remDups x y)
        | checkCompatibleTypes type2 type1 = ConsNatSet type2 (remDups x y)
        | otherwise = error ("ERROR : You can't operator a set of " ++ show(type1) ++ " with a set of " ++
                             show(type2))

binaryEval _ (Minus p) _ = error ("ERROR at " ++ show(p) ++ ": the binary - operator expects two numbers.")

-- Operator %
binaryEval (ConsNatInt x) (Mod p) (ConsNatInt 0) = error ("ERROR at " ++ show(p) ++ ": division by zero.")
binaryEval (ConsNatInt x) (Mod _) (ConsNatInt y) = ConsNatInt (rem x y)
binaryEval _ (Mod p) _ = error ("ERROR at " ++ show(p) ++ ": the % operator expects two ints.")

-- Operator ^
binaryEval (ConsNatInt 0) (Expo p) (ConsNatInt 0) = error ("ERROR at " ++ show(p) ++ ": 0^0 is undefined.")
binaryEval (ConsNatInt 0) (Expo p) (ConsNatDouble 0.0) = error ("ERROR at " ++ show(p) ++ ": 0^0 is undefined.")
binaryEval (ConsNatDouble 0.0) (Expo p) (ConsNatDouble 0.0) = error ("ERROR at " ++ show(p) ++ ": 0^0 is undefined.")
binaryEval (ConsNatDouble 0.0) (Expo p) (ConsNatInt 0) = error ("ERROR at " ++ show(p) ++ ": 0^0 is undefined.")
binaryEval (ConsNatInt x) (Expo _) (ConsNatInt y) 
    | y > 0 = ConsNatInt (x ^ y)
    | otherwise = ConsNatDouble ((fromIntegral x) ** (fromIntegral y)) 
binaryEval (ConsNatDouble x) (Expo _) (ConsNatInt y) = ConsNatDouble (x ** fromIntegral(y))
binaryEval (ConsNatInt x) (Expo _) (ConsNatDouble y) = ConsNatDouble (fromIntegral(x) ** y) 
binaryEval (ConsNatDouble x) (Expo _) (ConsNatDouble y) = ConsNatDouble (x ** y) 
binaryEval _ (Expo p) _ = error ("ERROR at " ++ show(p) ++ ": the ^ operator expects two numbers.")

-- Operator /   
binaryEval _ (Div p) (ConsNatInt 0) = error ("ERROR at " ++ show(p) ++ ": division by 0.")
binaryEval _ (Div p) (ConsNatDouble 0.0) = error ("ERROR at " ++ show(p) ++ ": division by 0.")
binaryEval (ConsNatInt x) (Div _) (ConsNatInt y )  
    | (rem x y) == 0 = ConsNatInt (div x y)
    | otherwise = ConsNatDouble ((fromIntegral x) / (fromIntegral y))

-- Operator *
binaryEval (ConsNatInt x) (Times p) (ConsNatInt y) = ConsNatInt(x*y)
binaryEval (ConsNatDouble x) (Times p) (ConsNatInt y) = ConsNatDouble(x*fromIntegral(y))
binaryEval (ConsNatInt x) (Times p) (ConsNatDouble y) = ConsNatDouble(fromIntegral(x)*y)
binaryEval (ConsNatDouble x) (Times p) (ConsNatDouble y) = ConsNatDouble(x*y)
binaryEval (ConsNatSet type1 x) (Times _) (ConsNatSet type2 y)
        | checkCompatibleTypes type1 type2 = ConsNatSet type1 (intersect x y)
        | checkCompatibleTypes type2 type1 = ConsNatSet type2 (intersect x y)
        | otherwise = error ("ERROR : You can't intersect a set of " ++ show(type1) ++ " with a set of " ++
                             show(type2))
binaryEval _ (Times p) _ = error ("ERROR at " ++ show(p) ++ ": the * operator expects two numbers.")

--  OPERATORS WITH BOOLEAN RESULT --------------------------------------------------------------------------------------


-- Operator ?
-- binaryEval (ConsNatSet type1 x) (In _) (ConsNatSet type2 y)  
--     | checkCompatibleTypes type1 type2 = ConsNatBool (subList x y)
--     | checkCompatibleTypes type2 type1 = ConsNatBool (subList x y)
--     | otherwise = error ("ERROR : You can't operator a set of " ++ show(type1) ++ " with a set of " ++
--                             show(type2))

-- Operator &&
binaryEval (ConsNatBool x) (And p) (ConsNatBool y) = ConsNatBool (x && y)
binaryEval _ (And p) _ = error ("ERROR at " ++ show(p) ++ ": the && operator expects two booleans.")

-- Operator ||
binaryEval (ConsNatBool x) (Or p) (ConsNatBool y) = ConsNatBool (x || y)
binaryEval _ (Or p) _ = error ("ERROR at " ++ show(p) ++ ": the || operator expects two booleans.")

-- Operator <
binaryEval (ConsNatInt x) (LessThan p) (ConsNatInt y) = ConsNatBool (x < y)
binaryEval (ConsNatDouble x) (LessThan p) (ConsNatDouble y) = ConsNatBool (x < y)
binaryEval (ConsNatInt x) (LessThan p) (ConsNatDouble y) = 
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binaryEval (ConsNatDouble x) (LessThan p) (ConsNatInt y) =
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binaryEval (ConsNatSet type1 x) (LessThan _) (ConsNatSet type2 y)  
    | checkCompatibleTypes type1 type2 = ConsNatBool (subList x y)
    | checkCompatibleTypes type2 type1 = ConsNatBool (subList x y)
    | otherwise = error ("ERROR : You can't operator a set of " ++ show(type1) ++ " with a set of " ++
                            show(type2))
binaryEval _ (LessThan p) _ = error ("ERROR at " ++ show(p) ++ ": the < operator expects two numbers.")

-- Operator >
binaryEval (ConsNatInt x) (GreaterThan p) (ConsNatInt y) = ConsNatBool (x > y)
binaryEval (ConsNatDouble x) (GreaterThan p) (ConsNatDouble y) = ConsNatBool (x > y)
binaryEval (ConsNatInt x) (GreaterThan p) (ConsNatDouble y) = 
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binaryEval (ConsNatDouble x) (GreaterThan p) (ConsNatInt y) =
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binaryEval _ (GreaterThan p) _ = error ("ERROR at " ++ show(p) ++ ": the > operator expects two numbers.")

-- Operator <=
binaryEval (ConsNatInt x) (LessEquals p) (ConsNatInt y) = ConsNatBool (x <= y)
binaryEval (ConsNatDouble x) (LessEquals p) (ConsNatDouble y) = ConsNatBool (x <= y)
binaryEval (ConsNatInt x) (LessEquals p) (ConsNatDouble y) = 
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binaryEval (ConsNatDouble x) (LessEquals p) (ConsNatInt y) =
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binaryEval _ (LessEquals p) _ = error ("ERROR at " ++ show(p) ++ ": the <= operator expects two numbers.")

-- Operator >=
binaryEval (ConsNatInt x) (GreaterEquals p) (ConsNatInt y) = ConsNatBool (x >= y)
binaryEval (ConsNatDouble x) (GreaterEquals p) (ConsNatDouble y) = ConsNatBool (x >= y)
binaryEval (ConsNatInt x) (GreaterEquals p) (ConsNatDouble y) = 
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binaryEval (ConsNatDouble x) (GreaterEquals p) (ConsNatInt y) =
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binaryEval _ (GreaterEquals p) _ = error ("ERROR at " ++ show(p) ++ ": the >= operator expects two numbers.")

-- Operator ==
binaryEval (ConsNatInt x) (Equals p) (ConsNatInt y) = ConsNatBool (x == y)
binaryEval (ConsNatDouble x) (Equals p) (ConsNatDouble y) = ConsNatBool (x == y)
binaryEval (ConsNatInt x) (Equals p) (ConsNatDouble y) = 
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binaryEval (ConsNatDouble x) (Equals p) (ConsNatInt y) =
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binaryEval _ (Equals p) _ = error ("ERROR at " ++ show(p) ++ ": the == operator expects two numbers.")

-- Operator !=
binaryEval (ConsNatInt x) (Different p) (ConsNatInt y) = ConsNatBool (x /= y)
binaryEval (ConsNatDouble x) (Different p) (ConsNatDouble y) = ConsNatBool (x /= y)
binaryEval (ConsNatInt x) (Different p) (ConsNatDouble y) = 
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binaryEval (ConsNatDouble x) (Different p) (ConsNatInt y) =
    error ("ERROR at " ++ show(p) ++ ": comparison between two different types.")
binaryEval _ (Different p) _ = error ("ERROR at " ++ show(p) ++ ": the != operator expects two numbers.")

-- | Implementation of binary operations
unaryEval :: Token -- ^ operator
            -> Value -- ^ operand
            -> Value -- ^ result of the operation   

-- Operator - (unary)
unaryEval (Minus p) (ConsNatInt x) = ConsNatInt (-x) 
unaryEval (Minus p) (ConsNatDouble x) = ConsNatDouble (-x) 
unaryEval (Minus p) _ = error ("ERROR at " ++ show(p) ++ ": the unary - operator expects a number.")

unaryEval (Negation p) (ConsNatBool x) = ConsNatBool (not(x)) 
unaryEval (Negation p) _ = error ("ERROR at " ++ show(p) ++ ": the unary ! operator expects a boolean.")


playMyExp :: Exp -> ParsecT [Token] [MemoryCell] IO (Value)
playMyExp expr = 
    do 
        
        mem <- getState -- [MemoryCell]
        let newExp = setExpType mem expr -- Exp
        val <- playExp newExp
        return $ val

playExp :: Exp -> ParsecT [Token] [MemoryCell] IO (Value)
playExp expr = (try (playExpBinEval expr)) <|> 
        (try (playExpUnEval expr) ) <|> (playExpLit expr) 

playExpLit expr = 
    do 
        let tok = getExpLitToken expr -- Token
        let val = getValueFromToken tok -- Value
        return $ val 

playExpBinEval (CONSExpBin t (CONSBinOp binOp) expr1 expr2) = 
    do 
        res1 <- (playExp expr1)
        res2 <- (playExp expr2)
        let val = (binaryEval (res1) (binOp) (res2))
        return $ val
        
playExpBinEval _ = 
    do 
        fail ("error")

playExpUnEval (CONSExpUn t (CONSUnOp unOp) exp) =
    do 
        res <- (playExp exp)
        let val = (unaryEval (unOp) res)
        return $ val

playExpUnEval _ =
    do
        fail ("error")   

--TODO - semantics to assignment
-- playExpAssignEval (CONSExpAssign t (CONSLValueId str) exp) =