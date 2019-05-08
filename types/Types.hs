module Types where

data TypeValue = TypeValue {tvType :: String, tvValue :: Int }deriving Show 
data Scope = Scope {scopeScope :: [String]} deriving Show 
data MemoryCell = MemoryCell {cellName :: String, cellTv :: TypeValue, cellScope :: Scope} deriving Show 
data Memory = Memory {memCells :: [MemoryCell]} deriving Show 

add_var_to_memory :: Memory -> MemoryCell -> Memory
add_var_to_memory (Memory (cells)) cell = Memory (cell : cells)

aux_update_var_in_memory :: [MemoryCell] -> MemoryCell -> [MemoryCell]
aux_update_var_in_memory [] _ = []
aux_update_var_in_memory (firstCell : cells) newCell
    | cellName firstCell == cellName newCell = newCell : cells
    | otherwise = firstCell : aux_update_var_in_memory cells newCell

update_var_in_memory :: Memory -> MemoryCell -> Memory
update_var_in_memory (Memory cells) cell = Memory (aux_update_var_in_memory cells cell)