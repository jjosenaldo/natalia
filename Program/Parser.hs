module Program.Parser where

-- natalia's modules
import PredefBlocks.Grammar
import PredefBlocks.Parser
import Program.Grammar
import Program.ProgramState
import Memory.Memory
import Types.Typedef
import Lexical.Lexemes
import Expressions.Semantics

-- Haskell's modules
-- import Control.Monad.IO.Class
-- import System.Environment
-- import System.IO.Unsafe
import Text.Parsec

populateTypedefs :: [Typedef] -> ProgramState -> ProgramState
populateTypedefs [] s = s
populateTypedefs (h:tdList) (CONSState a l glbMem locMem) = populateTypedefs tdList (CONSState a l (glbMem++[Typedef h]) locMem)

populateGlobals [] s = 
    do
        return (s)
populateGlobals (h:tdList) (CONSState a l glbMem locMem) =
    do
        val <- playExp (getInitializationExp h)
        let id = getInitializationId h
        let var = Variable (ConstructVariable id val True)
        return (populateGlobals tdList (CONSState a l (glbMem++[var]) locMem))

populateSubprograms :: [Subprogram] -> ProgramState -> ProgramState
populateSubprograms [] s = s
populateSubprograms (h:tdList) (CONSState a l glbMem locMem) = populateSubprograms tdList (CONSState a l (glbMem++[Subprogram h]) locMem)



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