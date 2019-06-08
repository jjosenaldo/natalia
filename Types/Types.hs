module Types.Types where

import Lexical.Lexemes


-- checks if two VALUES are of compatible types, or if a type and a value are compatible
is_compatible :: Token -> Token -> Bool
is_compatible (Int _ _) (Int _ _) = True
is_compatible (Type x _) (Int _ _) 
    | x == "int" = True
    | otherwise = False
is_compatible _ _ = False

attr_compatible_types :: Token -> Token -> Bool
attr_compatible_types (Type "int" _) (Type "int" _) = True
attr_compatible_types (Type "double" _) (Type "int" _) = True
attr_compatible_types (Type "double" _) (Type "double" _) = True
attr_compatible_types _ _  = False

var_type_from_name :: Token -> [(Token, Token)] -> Token
var_type_from_name _ [] = error "variable not found"
var_type_from_name (Id id1 p1) ((Id id2 _, Int v2 _):t) = 
    if id1 == id2 then Type "int" p1
    else var_type_from_name (Id id1 p1) t

var_type_from_name (Id id1 p1) ((Id id2 _, Double v2 _):t) = 
    if id1 == id2 then Type "double" p1
    else var_type_from_name (Id id1 p1) t

get_value_type :: Token -> Token
get_value_type (Int x p) = Type "int" p
get_value_type (Double x p) = Type "double" p