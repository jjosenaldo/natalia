--import Lexical.Lexemes

data Type = NatInt | NatBool | NatString | NatDouble | NatSet Type | NatStruct [(String, Type)] deriving (Show, Eq)
data TypeValue = ConsNatInt Int | ConsNatBool Bool | ConsNatString String | ConsNatDouble Double | ConsNatSet Type [TypeValue] | ConsNatStruct Type [(String, (Type, TypeValue))] deriving (Show, Eq)

--                                  id     value
data Variable = ConstructVariable String TypeValue | ConstructConstantVariable String TypeValue deriving (Show, Eq)
data Parameter = ConsParameter String Type deriving (Show, Eq)

--                                  id                return                     name
data Subprogram = ConstructFunction String [Parameter] Type | ConstructProcedure String [Parameter] deriving (Show, Eq)
data MemoryCell = Variable Variable | Subprogram Subprogram deriving (Show, Eq)

--CONSTANTS FOR TESTING!!!! THESE SHOULD NOT BE AT MASTER...
funx = Subprogram (ConstructFunction "soma" [ConsParameter "a" NatInt, ConsParameter "b" NatInt] NatInt)
procx = Subprogram (ConstructProcedure "nada" [])
varx = Variable (ConstructVariable "x" (ConsNatInt 2))
mem = memory_insert varx (memory_insert funx (memory_insert procx []))

-- Functions to access important fields of memory cells
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
memory_update (Variable v1) (v2:t) = 
    if (isVariable v2) && (id (Variable v1)) == (id v2) then (Variable v1) : t
    else v2 : memory_update (Variable v1) t

memory_update (Subprogram s) mem = error ("ERROR you can't update the value of subprogram in memory")

-- | Gets the value of a variable in the table of symbols
memory_get :: String -- ^ the name of the memory cell to be searched
             -> (Int, Int)
             -> [MemoryCell] -- ^ the memory 
             -> MemoryCell -- ^ the value of the memory cell
memory_get name p [] = error ("ERROR when fetching name " ++ name ++ " at "++show(p)++": it is not in the memory.")
memory_get name p (cell:m) = 
    if (getId cell) == name then cell
    else memory_get name p m