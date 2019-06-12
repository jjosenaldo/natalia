module TypeValue.TypeValue where

data Type = 
    NatGenType |
    NatInt | 
    NatBool | 
    NatString | 
    NatDouble | 
    NatSet Type | 
    NatStruct String [(String, Type)] |
    NatArray Type deriving (Show, Eq)

data Value = 
    ConsNatGenType |
    ConsNatInt Integer | 
    ConsNatBool Bool | 
    ConsNatString String | 
    ConsNatDouble Double | 
    ConsNatSet Type [Value] | 
    ConsNatStruct String [(String, Value)] deriving (Show, Eq)