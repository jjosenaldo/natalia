module Types.Types where

import Lexical.Lexemes

--                                                                              name
data Type = NatInt | NatBool | NatString | NatDouble | NatSet Type | NatStruct String [(String, Type)] deriving (Show, Eq)
data Value = ConsNatInt Integer | ConsNatBool Bool | ConsNatString String | ConsNatDouble Double | ConsNatSet Type [Value] | ConsNatStruct String [(String, Value)] deriving (Show, Eq)

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
checkCompatibleTypes NatInt NatInt = True
checkCompatibleTypes NatDouble NatInt = True
checkCompatibleTypes NatDouble NatDouble = True
checkCompatibleTypes NatBool NatBool = True
checkCompatibleTypes NatString NatString = True
checkCompatibleTypes (NatStruct str1 l1) (NatStruct str2 l2) = str1 == str2 
checkCompatibleTypes (NatSet t1) (NatSet t2) = checkCompatibleTypes t1 t2

getTypeFromTypeToken (Type str _) 
    | str == "int" = NatInt
    | str == "bool" = NatBool
    | otherwise = error ("Not supported type: " ++ str)

-- | Checks if the types involved in an assignment are compatible
attr_compatible_types :: Token -- ^ the type of the variable 
                      -> Token -- ^ the type of the value being assigned
                      -> Bool -- ^ True if the types are compatible, False otherwise
attr_compatible_types (Type "int" _) (Type "int" _) = True
attr_compatible_types (Type "double" _) (Type "int" _) = True
attr_compatible_types (Type "double" _) (Type "double" _) = True
attr_compatible_types (Type "bool" _) (Type "bool" _)  = True
attr_compatible_types _ _  = False

-- | Gets the type of a variable based on its name
var_type_from_name :: Token -- ^ the name of the variable
                   -> [(Token, Token)] -- ^ the table of symbols
                   -> Token -- ^ the type of the variable
var_type_from_name _ [] = error "variable not found"
var_type_from_name (Id id1 p1) ((Id id2 _, Int v2 _):t) = 
    if id1 == id2 then Type "int" p1
    else var_type_from_name (Id id1 p1) t

var_type_from_name (Id id1 p1) ((Id id2 _, Double v2 _):t) = 
    if id1 == id2 then Type "double" p1
    else var_type_from_name (Id id1 p1) t

var_type_from_name (Id id1 p1) ((Id id2 _, Bool v2 _):t) = 
    if id1 == id2 then Type "bool" p1
    else var_type_from_name (Id id1 p1) t

-- | Gets the type of a value
get_value_type :: Token -- ^ the value 
               -> Token -- ^ the type whose the value belongs to
get_value_type (Int x p) = Type "int" p
get_value_type (Double x p) = Type "double" p
get_value_type (Bool x p) = Type "bool" p