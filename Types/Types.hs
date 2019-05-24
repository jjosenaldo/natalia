module Types.Types where

import Lexical.Lexemes

get_value :: Token -> [(Token, Token)] -> Token
get_value _ [] = error "variable not found"
get_value (Id id1 p1) ((Id id2 _, value):t) = if id1 == id2 then value
                                             else get_value (Id id1 p1) t

-- checks if two VALUES are of compatible types, or if a type and a value are compatible
is_compatible :: Token -> Token -> Bool
is_compatible (Int _ _) (Int _ _) = True
is_compatible (Type x _) (Int _ _) 
    | x == "int" = True
    | otherwise = False
is_compatible _ _ = False