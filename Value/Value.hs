module Value.Value where 

-- natalia's modules
import Lexical.Lexemes
import TypeValue.TypeValue

getValueFromToken :: Token -> Value
getValueFromToken (Int x p) = ConsNatInt x 
getValueFromToken (String x p) = ConsNatString x
getValueFromToken (Double x p) = ConsNatDouble x
getValueFromToken (Bool x p) = ConsNatBool x
getValueFromToken (Null p) = ConsNatNull

isValueString :: Value -> Bool
isValueString (ConsNatString s) = True
isValueString _ = False

isValueInteger :: Value -> Bool
isValueInteger (ConsNatInt s) = True
isValueInteger _ = False

isValueBool :: Value -> Bool
isValueBool (ConsNatBool s) = True
isValueBool _ = False

isValueDouble :: Value -> Bool
isValueDouble (ConsNatDouble s) = True
isValueDouble _ = False


getValueAsString :: Value -> String
getValueAsString (ConsNatString s) = s 

getValueAsInteger :: Value -> Integer 
getValueAsInteger (ConsNatInt s) = s 

getValueAsBool :: Value -> Bool 
getValueAsBool (ConsNatBool s) = s 

getValueAsDouble :: Value -> Double 
getValueAsDouble (ConsNatDouble s) = s

-- getValueAsSet :: Value -> Bool 
-- getValueAsSet (ConsNatBool s) = s

-- ConsNatSet Type [Value]                 | 
-- ConsNatStruct String [(String, Value)]  |
-- ConsNatArray Type [Value]               |
-- ConsNatNull