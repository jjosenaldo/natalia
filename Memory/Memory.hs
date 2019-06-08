module Memory.Memory where

import Lexical.Lexemes

-- | Inserts a variable in the table of symbols
symtable_insert :: (Token,Token) -- ^ the variable to be inserted
                -> [(Token,Token)] -- ^ the memory before the insertion
                -> [(Token,Token)] -- ^ the memory after the insertion
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

-- | Updates the value of a variable in the table of symbols
symtable_update :: (Token,Token) -- ^ the variable with its new value
                -> [(Token,Token)] -- ^ the memory before the update
                -> [(Token,Token)] -- ^ the memory after the update
symtable_update (Id id1 p1, _) [] = fail ("ERROR on the update of the variable" ++ id1 ++ " at " ++ (show p1) ++ ": it is not present in the memory.")
symtable_update (Id id1 p1, v1) ((Id id2 p2, v2):t) = 
    if id1 == id2 then (Id id1 p2, v1) : t
    else (Id id2 p2, v2) : symtable_update (Id id1 p1, v1) t