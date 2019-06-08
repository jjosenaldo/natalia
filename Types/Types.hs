module Types.Types where

import Lexical.Lexemes

-- | Checks if the types involved in an assignment are compatible
attr_compatible_types :: Token -- ^ the type of the variable 
                      -> Token -- ^ the type of the value being assigned
                      -> Bool -- ^ True if the types are compatible, False otherwise
attr_compatible_types (Type "int" _) (Type "int" _) = True
attr_compatible_types (Type "double" _) (Type "int" _) = True
attr_compatible_types (Type "double" _) (Type "double" _) = True
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

-- | Gets the type of a value
get_value_type :: Token -- ^ the value 
               -> Token -- ^ the type whose the value belongs to
get_value_type (Int x p) = Type "int" p
get_value_type (Double x p) = Type "double" p