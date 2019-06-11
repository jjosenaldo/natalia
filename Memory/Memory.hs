module Memory.Memory where

import Lexical.Lexemes


-- THESE FUNCTIONS ARE HERE JUST FOR 'BACKWARDS COMPATIBILITY', THEY'LL BE REMOVED SOON!

-- FUNCTIONS TO BE REPLACED:


-- -- | Inserts a variable in the table of symbols
-- symtable_insert :: (Token,Token) -- ^ the variable to be inserted
--                 -> [(Token,Token)] -- ^ the memory before the insertion
--                 -> [(Token,Token)] -- ^ the memory after the insertion
-- symtable_insert symbol []  = [symbol]
-- symtable_insert symbol symtable = symtable ++ [symbol]

-- -- | Updates the value of a variable in the table of symbols
-- symtable_update :: (Token,Token) -- ^ the variable with its new value
--                 -> [(Token,Token)] -- ^ the memory before the update
--                 -> [(Token,Token)] -- ^ the memory after the update
-- symtable_update (Id id1 p1, _) [] = fail ("ERROR on the update of the variable " ++ id1 ++ " at " ++ (show p1) ++ ": it is not present in the memory.")
-- symtable_update (Id id1 p1, v1) ((Id id2 p2, v2):t) = 
--     if id1 == id2 then (Id id1 p2, v1) : t
--     else (Id id2 p2, v2) : symtable_update (Id id1 p1, v1) t

-- -- | Gets the value of a variable in the table of symbols
-- symtable_get :: Token -- ^ the name of the variable to be searched
--              -> [(Token,Token)] -- ^ the memory 
--              -> Token -- ^ the value of the variable
-- symtable_get (Id name p) [] = error ("ERROR when getting the value of the variable " ++ name ++ " at " ++ (show p) ++ ": it is not in the memory.")

-- symtable_get (Id name p) ((Id id _, v):t) = 
--     if id == name then v
--     else symtable_get (Id name p) t

-- symtable_has_variable :: Token -- ^ the name of the variable to be searched
--                       -> [(Token, Token)] -- ^ the memory
--                       -> Bool -- True if the variable is in the memory, False otherwise
-- symtable_has_variable _ [] = False
-- symtable_has_variable (Id name p) ((Id id _, v):t) 
--     | name == id = True
--     | otherwise = symtable_has_variable (Id name p) t

-- BELOW IS THE CONTENT OF MemoryTest.hs

data Variable = ConstructVariable String Value Bool | ConstructConstantVariable String Value Bool deriving (Show, Eq)
data Parameter = ConsParameter String Type deriving (Show, Eq)

--                                  id                return                     name
data Subprogram = ConstructFunction String [Parameter] Type | ConstructProcedure String [Parameter] deriving (Show, Eq)
data MemoryCell = Variable Variable | Subprogram Subprogram deriving (Show, Eq)

-- functions to access important fields
getId (Variable (ConstructVariable x _)) = x
getId (Variable (ConstructConstantVariable x _)) = x
getId (Subprogram (ConstructFunction x _ _)) = x
getId (Subprogram (ConstructProcedure x _)) = x

getValue (Variable (ConstructVariable _ val)) = val
getValue (Variable (ConstructConstantVariable _ val)) = val

isVariable :: MemoryCell -> Bool
isVariable (Variable v) = True
isVariable _ = False

isConstantVariable (Variable (ConstructConstantVariable c _)) = True
isConstantVariable _ = False

memory_insert :: MemoryCell -- ^ the variable to be inserted
                -> [MemoryCell] -- ^ the memory before the insertion
                -> [MemoryCell] -- ^ the memory after the insertion
memory_insert symbol []  = [symbol]
memory_insert symbol memory = memory ++ [symbol]

-- | Updates the value of a variable in the table of symbols
memory_update :: MemoryCell -- ^ the variable with its new value
                -> [MemoryCell] -- ^ the memory before the update
                -> [MemoryCell] -- ^ the memory after the update
memory_update (Variable v) [] = error ("ERROR on the update of the variable " ++ (getId (Variable v)) ++ ": it is not present in the memory.")
memory_update (Variable (ConstructConstantVariable _ _)) m = error ("ERROR impossible to change value of a constant variable")
memory_update (Variable v1) (v2:t) = 
    if (isVariable v2) && ((getId (Variable v1)) == (getId v2)) then (Variable v1) : t
    else v2 : memory_update (Variable v1) t

memory_update (Subprogram s) mem = error ("ERROR you can't update the value of subprogram in memory")

-- | Gets the value of a variable in the table of symbols
memory_get :: String -- ^ the name of the memory cell to be searched
             -> (Int, Int) -- ^ the (line, column) where the variable was used in program
             -> [MemoryCell] -- ^ the memory 
             -> MemoryCell -- ^ the value of the memory cell
memory_get name p [] = error ("ERROR when fetching name " ++ name ++ " at "++show(p)++": it is not in the memory.")
memory_get name p (cell:m) = 
    if (getId cell) == name then cell
    else memory_get name p m

memory_has_name :: String -- ^ the name of the variable or subprogram to be searched
    -> [MemoryCell] -- ^ the memory
    -> Bool -- True if the variable is in the memory, False otherwise
memory_has_name _ [] = False
memory_has_name str (v:t) 
    | str == getId v = True
    | otherwise = memory_has_name str t

memory_delete :: String -- ^ the name of the variable 
        -> [MemoryCell] -- ^ the memory before removal
        -> [MemoryCell] -- ^ the memory after removal
memory_delete name (v:m) = 
    if (getId v) == name then
        if (isVariable v) then m
        else error ("ERROR you can't delete a subprogram")
    else (v : (memory_delete name m))