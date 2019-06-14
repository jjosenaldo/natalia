module Syntax.Definition where

-- natalia's modules
import Lexical.Lexemes
import TypeValue.TypeValue

data Expression = 
    CONSValue Value -- Bool x p
    deriving (Eq, Show)

getSyntacticalUnitPos :: Expression -> (Int, Int)
getSyntacticalUnitPos (CONSValue x) = getPosValue x -- CONSExpression Token