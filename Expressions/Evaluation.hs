module Expressions.Evaluation where

import Lexical.Lexemes

eval :: Token -> Token -> Token -> Token
eval (Int x p) (Plus _) (Int y _) = Int (x + y) p