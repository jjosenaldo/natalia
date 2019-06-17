module Memory.Memory where

import Lexical.Lexemes
import Types.Types
import Types.Typedef
import TypeValue.TypeValue

data Variable = 
    ConstructVariable String Value Bool | 
    ConstructConstantVariable String Value Bool deriving (Show, Eq)
data Parameter = ConsParameter String Type deriving (Show, Eq)

--                                  id                return                     name
data Subprogram = ConstructFunction String [Parameter] Type | ConstructProcedure String [Parameter] deriving (Show, Eq)

data MemoryCell = 
    Variable Variable | 
    Subprogram Subprogram |
    Typedef Typedef deriving (Show, Eq)

-- functions to access important fields
getId (Variable (ConstructVariable x _ _)) = x
getId (Variable (ConstructConstantVariable x _ _)) = x
getId (Subprogram (ConstructFunction x _ _)) = x
getId (Subprogram (ConstructProcedure x _)) = x
getId (Typedef (ConsTypedef x _)) = x
getId (Typedef (StructDef x _)) = x

getMemoryCellType (Typedef x) = x  

setValue::MemoryCell -> Value -> MemoryCell
setValue (Variable (ConstructVariable name v1 isGlobal)) v2
    | checkCompatibleTypes (getTypeFromValue v1) (getTypeFromValue v2) = Variable (ConstructVariable name v2 isGlobal)
    | otherwise = error ("ERROR type mismatch in variable " ++ name)

getValue::MemoryCell -> (Int, Int) -> Value
getValue (Variable (ConstructVariable _ val _)) _ = val
getValue (Variable (ConstructConstantVariable _ val _)) _ = val
getValue c p = error ("ERROR "++show(p)++" you can't get value from a " ++ show(c))

isVariable :: MemoryCell -> Bool
isVariable (Variable v) = True
isVariable _ = False

isConstantVariable (Variable (ConstructConstantVariable c _ _)) = True
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
memory_update (Variable (ConstructConstantVariable _ _ _)) m = error ("ERROR impossible to change value of a constant variable")
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


-- Receives a variable, a list of NatInts (representing indexes) and a value, returns the same variable with new value setted
setValueArray :: MemoryCell -> [Value] -> Value -> MemoryCell
setValueArray (Variable (ConstructVariable name val isGlobal)) [] newVal = 
    if checkCompatibleTypes t1 t2 then Variable (ConstructVariable name newVal isGlobal)
    else error ("ERROR type mismatch, trying to insert "++ show(t1) ++ " in " ++ show(t2))
    where 
        t1 = getTypeFromValue val
        t2 = getTypeFromValue newVal

setValueArray (Variable (ConstructVariable name val isGlobal)) list newVal = Variable (ConstructVariable name (setValueArray' val list newVal) isGlobal)

-- Auxiliar function, that receives an array, a list of NatInt (representing indexes), and a new value, and sets the value correspondingly
setValueArray' :: Value -> [Value] -> Value -> Value
setValueArray' val [] newVal = 
    if checkCompatibleTypes t1 t2 then newVal
    else error ("ERROR type mismatch, trying to insert " ++ show(t1) ++ " in " ++ show(t2))
    where 
        t1 = getTypeFromValue val
        t2 = getTypeFromValue newVal
setValueArray' (ConsNatArray t arr) (h:list) newVal = ConsNatArray t ((take index arr) ++ [setValueArray' (arrayAccess (ConsNatArray t arr) h) list newVal] ++ (drop (index+1) arr))
        where
            index = fromIntegral (getIntFromNatInt h)