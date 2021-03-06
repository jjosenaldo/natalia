module Types.Types where

import Lexical.Lexemes
import TypeValue.TypeValue

checkCompatibleTypes :: Type 
                     -> Type
                     -> Bool
checkCompatibleTypes _ NatGenType = True
checkCompatibleTypes NatInt NatInt = True
checkCompatibleTypes NatDouble NatInt = True
checkCompatibleTypes NatDouble NatDouble = True
checkCompatibleTypes NatBool NatBool = True
checkCompatibleTypes NatString NatString = True
checkCompatibleTypes (NatStruct str1) (NatStruct str2) = str1 == str2 
checkCompatibleTypes (NatSet t1) (NatSet t2) = checkCompatibleTypes t1 t2
checkCompatibleTypes (NatArray t1) (NatArray t2) = checkCompatibleTypes t1 t2
checkCompatibleTypes (NatStruct str) (NatNull) = True
checkCompatibleTypes _ _ = False

getTypeFromTypeToken :: Token -> Type
getTypeFromTypeToken (Type str _) 
    | str == "int" = NatInt
    | str == "string" = NatString
    | str == "double" = NatDouble
    | str == "bool" = NatBool
    | otherwise = error ("Not supported type: " ++ str)