module Program.ProgramState where

--import Memory.Memory
--                                  global        local
data ProgramState = CONSState String Int [MemoryCell] [MemoryCell] deriving (Eq, Show)
getStateGlobalMemory (CONSState _ _ m _) = m
getStateLocalMemory (CONSState _ _ _ m) = m
getStateLevel (CONSState _ l _ _) = l
getStateActiveSubprogram (CONSState a _ _ _) = a