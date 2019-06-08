module Memory.Memory where

import Lexical.Lexemes

-- functions for the table of symbols
symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (Id id1 p1, v1) ((Id id2 p2, v2):t) = 
    if id1 == id2 then (Id id1 p2, v1) : t
    else (Id id2 p2, v2) : symtable_update (Id id1 p1, v1) t