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

getTypeFromValue::Value -> Type
getTypeFromValue (ConsNatInt _) = NatInt 
getTypeFromValue (ConsNatBool _) = NatBool 
getTypeFromValue (ConsNatString _) = NatString 
getTypeFromValue (ConsNatDouble _) = NatDouble 
getTypeFromValue (ConsNatSet tp _) = NatSet tp
getTypeFromValue (ConsNatStruct str l) = NatStruct str (zip (fst (unzip l)) (map getTypeFromValue (snd (unzip l))))