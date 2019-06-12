module Types.Types where

import Lexical.Lexemes
import TypeValue.TypeValue

getTypeFromValue::Value -> Type
getTypeFromValue (ConsNatInt _) = NatInt 
getTypeFromValue (ConsNatBool _) = NatBool 
getTypeFromValue (ConsNatString _) = NatString 
getTypeFromValue (ConsNatDouble _) = NatDouble 
getTypeFromValue (ConsNatSet tp _) = NatSet tp
getTypeFromValue (ConsNatStruct str l) = NatStruct str (zip (fst (unzip l)) (map getTypeFromValue (snd (unzip l))))

checkCompatibleTypes :: Type 
                     -> Type
                     -> Bool
checkCompatibleTypes NatGenType _ = True
checkCompatibleTypes NatInt NatInt = True
checkCompatibleTypes NatDouble NatInt = True
checkCompatibleTypes NatDouble NatDouble = True
checkCompatibleTypes NatBool NatBool = True
checkCompatibleTypes NatString NatString = True
checkCompatibleTypes (NatStruct str1 l1) (NatStruct str2 l2) = str1 == str2 
checkCompatibleTypes (NatSet t1) (NatSet t2) = checkCompatibleTypes t1 t2
checkCompatibleTypes _ _ = False

getTypeFromTypeToken :: Token -> Type
getTypeFromTypeToken (Type str _) 
    | str == "int" = NatInt
    | str == "string" = NatString
    | str == "double" = NatDouble
    | str == "bool" = NatBool
    | otherwise = error ("Not supported type: " ++ str)