module Types.Types where

import Lexical.Lexemes
import TypeValue.TypeValue

checkCompatibleTypes :: Type 
                     -> Type
                     -> Bool
checkCompatibleTypes _ NatGenType = True -- this doesn't make sense. I hope it will be removed in the future.
checkCompatibleTypes NatGenType _  = True
checkCompatibleTypes NatInt NatInt = True
checkCompatibleTypes NatDouble NatInt = True
checkCompatibleTypes NatDouble NatDouble = True
checkCompatibleTypes NatBool NatBool = True
checkCompatibleTypes NatString NatString = True
checkCompatibleTypes (NatStruct str1) (NatStruct str2) = str1 == str2 
checkCompatibleTypes (NatSet t1) (NatSet t2) = checkCompatibleTypes t1 t2
checkCompatibleTypes (NatArray t1) (NatArray t2) = checkCompatibleTypes t1 t2
checkCompatibleTypes (NatStruct str) (NatNull) = True
checkCompatibleTypes (NatPointer t1) (NatPointer t2) = checkCompatibleTypes t1 t2
checkCompatibleTypes NatNull NatNull = True
checkCompatibleTypes NatNothing _ = False
checkCompatibleTypes _ _ = False

isOfStruct :: Type -> Bool
isOfStruct(NatStruct _) = True
isOfStruct (NatSet t) = isOfStruct t
isOfStruct (NatPointer t) = isOfStruct t
isOfStruct (NatArray t) = isOfStruct t
isOfStruct _ = False

getTypeFromTypeToken :: Token -> Type
getTypeFromTypeToken (Type str _) 
    | str == "int" = NatInt
    | str == "string" = NatString
    | str == "double" = NatDouble
    | str == "bool" = NatBool
    | otherwise = error ("Not supported type: " ++ str)


getNameOfType :: Type -> String
getNameOfType NatInt         =  "int"  
getNameOfType NatBool        =  "bool"
getNameOfType NatString      =  "string"
getNameOfType NatDouble      =  "double"
getNameOfType NatNothing     =  "NatNothing"
getNameOfType (NatSet t)     =  "{" ++ (getNameOfType t) ++ "}"
getNameOfType (NatStruct t)  =  t
getNameOfType (NatArray t)   =  "[" ++ (getNameOfType t) ++ "]"
getNameOfType NatNull        =  "Null"  
getNameOfType NatGenType     =  "NatGenType"
getNameOfType (NatPointer t) =  "*" ++ (getNameOfType t) ++ "*" 