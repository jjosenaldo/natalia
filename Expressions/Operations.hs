module Expressions.Operations where

-- natalia's modules
import Expressions.Grammar
import TypeValue.TypeValue

-- Haskell modules
import Text.Parsec

-- TODO
getReturnTypeOfBinOp :: BinOp -> Type -> Type -> Type 
getReturnTypeOfBinOp _ _ _ = NatNothing

-- TODO
getReturnTypeOfUnOp :: UnOp -> Type -> Type 
getReturnTypeOfUnOp _ _ = NatNothing
