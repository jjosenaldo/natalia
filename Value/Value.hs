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

getValueAsString :: Value -> String
getValueAsString (ConsNatString s) = s 