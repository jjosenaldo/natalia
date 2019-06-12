module Types.Typedef where

import TypeValue.TypeValue

data Typedef = 
    ConsTypedef String Type  |
    StructDef String [(Type, String)] deriving (Show, Eq)
