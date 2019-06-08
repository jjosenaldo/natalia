module Expressions.Evaluation where

import Lexical.Lexemes

binary_eval :: Token -> Token -> Token -> Token
binary_eval (Int x p) (Plus _) (Int y _) = Int (x + y) p
binary_eval (Int x p) (Plus _) (Double y _) = Double ((fromIntegral x) + y) p
binary_eval (Double x p) (Plus _) (Int y _) = Double (x + (fromIntegral y)) p
binary_eval (Double x p) (Plus _) (Double y _) = Double (x + y) p
binary_eval _ (Plus p) _ = error ("ERROR at" ++ show(p) ++ ": the + operator expects two numbers.")

binary_eval (Int x p) (Minus _) (Int y _) = Int (x - y) p
binary_eval (Int x p) (Minus _) (Double y _) = Double ((fromIntegral x) - y) p
binary_eval (Double x p) (Minus _) (Int y _) = Double (x - (fromIntegral y)) p
binary_eval (Double x p) (Minus _) (Double y _) = Double (x - y) p
binary_eval _ (Minus p) _ = error ("ERROR at" ++ show(p) ++ ": the binary - operator expects two numbers.")

binary_eval (Int x _) (Mod _) (Int 0 p) = error ("ERROR at " ++ show(p) ++ ": division by zero.")
binary_eval (Int x p) (Mod _) (Int y _) = Int (rem x y) p
binary_eval _ (Mod p) _ = error ("ERROR at" ++ show(p) ++ ": the % operator expects two ints.")

unary_eval :: Token -> Token -> Token
unary_eval (Minus p) (Int x _) = Int (-x) p
unary_eval (Minus p) (Double x _) = Double (-x) p
unary_eval (Minus p) _ = error ("ERROR at" ++ show(p) ++ ": the unary - operator expects a number.")