module TypeValue.TypeValue where

data Type = 
    NatGenType                              |
    NatInt                                  | 
    NatBool                                 | 
    NatString                               | 
    NatDouble                               | 
    NatSet Type                             | 
    NatStruct String                        |
    NatArray Type deriving (Show, Eq)

-- SPOILER DA BRANCH TYPEDEF 
-- MemoryCell = ... | Typedef Typedef
-- Typedef    = Def String Type
-- Def "SetDeInteiros" (ConsNatSet NatInt)
--
{- 
@typedef{
    rational_t {
        int num;
        int den;
    }
    setDeInteiros {int}

}
@main{
    rational_t rat = setDeInteiros{num = 1, den = 2};
}
-}

data Value = 
    ConsNatGenType                          |
    ConsNatInt Integer                      | 
    ConsNatBool Bool                        | 
    ConsNatString String                    | 
    ConsNatDouble Double                    | 
    ConsNatSet Type [Value]                 | 
    ConsNatStruct String [Value]            |
    ConsNatArray Type [Value] deriving (Show, Eq)



getIntFromNatInt (ConsNatInt x) = x
getIntFromNatInt v = error ("Trying to fetch Int from " ++ (show (getTypeFromValue v)))

getTypeFromValue::Value -> Type
getTypeFromValue (ConsNatInt _) = NatInt 
getTypeFromValue (ConsNatBool _) = NatBool 
getTypeFromValue (ConsNatString _) = NatString 
getTypeFromValue (ConsNatDouble _) = NatDouble 
getTypeFromValue (ConsNatSet tp _) = NatSet tp
--getTypeFromValue (ConsNatStruct str l) = NatStruct str (zip (fst (unzip l)) (map getTypeFromValue (snd (unzip l))))
getTypeFromValue (ConsNatArray tp _) = NatArray tp

arrayAccess :: Value -> Value -> Value
arrayAccess (ConsNatArray _ v) (ConsNatInt x) = v!!(fromIntegral x)
arrayAccess v _ = error("ERROR Operator [] can only be used to access array positions, given: " ++ show(getTypeFromValue v))