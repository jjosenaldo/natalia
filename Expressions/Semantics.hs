module Expressions.Semantics where 

-- natalia's modules
import Expressions.Grammar
import Lexical.Lexemes
import Memory.Memory
import TypeValue.TypeValue

-- Haskell modules
playExpression :: [MemoryCell] -> Exp -> ([MemoryCell], Value)
playExpression memory (CONSExpLit t (Int x p)) = (memory, ConsNatInt x)