module Syntax.Definition where

-- natalia's modules
import Lexical.Lexemes
import TypeValue.TypeValue

data Id = 
    CONSToken Token -- Id
    deriving (Eq, Show)

data Expression = 
    CONSValue Value | -- literals
    CONSId Id 
    deriving (Eq, Show)

getSyntacticalUnitPos :: Expression -> (Int, Int)
getSyntacticalUnitPos (CONSValue x) = getPosValue x 