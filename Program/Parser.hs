module Program.Parser where

-- natalia's modules
import PredefBlocks.Grammar
import PredefBlocks.Parser
import Program.Grammar
import Memory.Memory
import Types.Typedef

--                                  global        local
data State = CONSState String Int [MemoryCell] [MemoryCell] deriving (Eq, Show)
getStateGlobalMemory (CONSState _ _ m _) = m
getStateLocalMemory (CONSState _ _ _ m) = m
getStateLevel (CONSState _ l _ _) = l
getStateActiveSubprogram (CONSState a _ _ _) = a

populateTypedefs :: [Typedef] -> State -> State
populateTypedefs [] s = s
populateTypedefs (h:tdList) (CONSState a l glbMem locMem) = populateTypedefs tdList (CONSState a l (glbMem++[Typedef h]) locMem)

populateGlobals :: [Initialization] -> State -> State
populateGlobals [] s = s
populateGlobals (h:tdList) (CONSState a l glbMem locMem) = populateGlobals tdList (CONSState a l (glbMem++[var]) locMem)
    where
        val = playExp (getInitializationExp h)
        id = getInitializationId h
        var = Variable (ConstructVariable val id True)

populateSubprograms :: [Subprogram] -> State -> State
populateSubprograms [] s = s
populateSubprograms (h:tdList) (CONSState a l glbMem locMem) = populateSubprograms tdList (CONSState a l (glbMem++[Subprogram h]) locMem)

-- Haskell's modules
-- import Control.Monad.IO.Class
-- import System.Environment
-- import System.IO.Unsafe
import Text.Parsec

_program =
    do
        typedefsBlock <- _typedefsBlock
        globalsBlock <- _globalsBlock
        subprogramsBlock <- _subprogramsBlock

        updateState(populateTypedefs (getTypedefs typedefsBlock))
        updateState(populateTypedefs (getInitializations globalsBlock))
        updateState(populateTypedefs (getSubprograms subprogramsBlock))

        s <- getState

        liftIO(print(s))

        mainBlock <- _mainBlock
        return (CONSProgram typedefsBlock globalsBlock subprogramsBlock mainBlock)