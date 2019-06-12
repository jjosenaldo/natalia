module Types.Typedef where

import TypeValue.TypeValue

data Typedef = 
    Typedef String Type  |
    StructDef String [(Type, String)] deriving (Show, Eq)
