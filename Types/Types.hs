module Types.Types where

import Lexical.Lexemes

get_value :: Token -> [(Token, Token)] -> Token
get_value _ [] = error "getValue error: variable not found"
get_value (Id id1) ((Id id2, value):t) = if id1 == id2 then value
                                                else get_value (Id id1) t

-- checks if two VALUES are of compatible types, or if a type and a value are compatible
is_compatible :: Token -> Token -> Bool
is_compatible (Int _ ) (Int _ ) = True
is_compatible (Type x) (Int _ ) 
    | x == "int" = True
    | otherwise = False
is_compatible _ _ = False