module Expressions.Evaluation where

import Lexical.Lexemes

binary_eval :: Token -> Token -> Token -> Token
binary_eval (Int x p) (Plus _) (Int y _) = Int (x + y) p
binary_eval (Int x p) (Mod _) (Int y _) = Int (rem x y) p

unary_eval :: Token -> Token -> Token
unary_eval (Minus p) (Int x _) = Int (-x) p