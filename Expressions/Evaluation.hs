module Expressions.Evaluation where

import Lexical.Lexemes

eval :: Token -> Token -> Token -> Token
eval (Int x p) (Plus _) (Int y _) = Int (rem x y) p
eval (Int x p) (Mod _) (Int y _) = Int (rem x y) p