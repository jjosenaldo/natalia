module Expressions.Semantics where 

-- natalia's modules
import Expressions.Grammar
import Lexical.Lexemes
import Memory.Memory
import TypeValue.TypeValue

-- Haskell modules
playExpression :: [MemoryCell] -> Exp -> ([MemoryCell], Value)

-- semantics to literals
playExpression memory (CONSExpLit t (Int x p)) = (memory, ConsNatInt x)
playExpression memory (CONSExpLit t (Double x p)) = (memory, ConsNatDouble x)
playExpression memory (CONSExpLit t (String x p)) = (memory, ConsNatString x)
playExpression memory (CONSExpLit t (Bool x p)) = (memory, ConsNatBool x)
playExpression memory (CONSExpLit t (Null p)) = (memory, ConsNatNull )

