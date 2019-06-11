module Memory.Memory where

import Lexical.Lexemes
--                                                                              name
data Type = NatInt | NatBool | NatString | NatDouble | NatSet Type | NatStruct String [(String, Type)] deriving (Show, Eq)

--                                  id     value
data Variable = ConstructVariable String Value | ConstructConstantVariable String Value deriving (Show, Eq)
data Parameter = ConsParameter String Type deriving (Show, Eq)

--                                  id                return                     name
data Subprogram = ConstructFunction String [Parameter] Type | ConstructProcedure String [Parameter] deriving (Show, Eq)
data MemoryCell = Variable Variable | Subprogram Subprogram deriving (Show, Eq)

--CONSTANTS FOR TESTING!!!! THESE SHOULD NOT BE AT MASTER...
-- funx = Subprogram (ConstructFunction "soma" [ConsParameter "a" NatInt, ConsParameter "b" NatInt] NatInt)
-- procx = Subprogram (ConstructProcedure "nada" [])
-- varx = Variable (ConstructVariable "x" (ConsNatInt 2))
-- structx = Variable (ConstructVariable "a" (ConsNatStruct "rational_t" [("num", ConsNatInt 1), ("den", ConsNatInt 2), ("set_to_test", ConsNatSet NatInt [ConsNatInt 1, ConsNatInt 2, ConsNatInt 3])]))
-- mem = memory_insert structx (memory_insert procx (memory_insert funx (memory_insert varx [])))


-- Functions to access important fields of memory cells
getId (Variable (ConstructVariable x _)) = x
getId (Variable (ConstructConstantVariable x _)) = x
getId (Subprogram (ConstructFunction x _ _)) = x
getId (Subprogram (ConstructProcedure x _)) = x

getTypeFromValue (ConsNatInt _) = NatInt 
getTypeFromValue (ConsNatBool _) = NatBool 
getTypeFromValue (ConsNatString _) = NatString 
getTypeFromValue (ConsNatDouble _) = NatDouble 
getTypeFromValue (ConsNatSet tp _) = NatSet tp
getTypeFromValue (ConsNatStruct str l) = NatStruct str (zip (fst (unzip l)) (map getTypeFromValue (snd (unzip l))))

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