data Memory = Memory {mem_cells :: [Variable]} deriving Show
data Variable = Variable {var_id :: VarId, var_type :: VarType, var_value :: VarValue, var_scope :: VarScope} deriving Show
data VarId = VarId String (Int,Int) deriving Show
data VarType = VarType String (Int,Int) deriving Show
data VarValue = ValueInt VarType Int (Int,Int) | ValueDouble Double (Int,Int) | ValueSet Set (Int, Int) | ValueBool Bool (Int, Int) deriving Show
data VarScope = VarScope String (Int,Int) deriving Show

id_name :: VarId -> String
id_name (VarId name _ ) = name

data Set = Set  [VarValue] (Int,Int) deriving Show 