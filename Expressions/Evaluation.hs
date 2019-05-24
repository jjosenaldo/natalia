module Expressions.Evaluation where

import Lexical.Lexemes

eval :: Token -> Token -> Token -> Token
eval (Int x ) (Plus ) (Int y) = Int (x + y)