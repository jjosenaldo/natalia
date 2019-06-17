module Program.Parser where

-- natalia's modules
import PredefBlocks.Grammar
import PredefBlocks.Parser
import Program.Grammar
import Memory.Memory
import Types.Typedef
import Types.Types
import TypeValue.TypeValue
import Lexical.Lexemes
import Expressions.Semantics

-- Haskell's modules
import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Text.Parsec

populateTypedefs :: [Typedef] -> ProgramState -> ProgramState
populateTypedefs [] s = s
populateTypedefs (h:tdList) (CONSState a l glbMem locMem) = populateTypedefs tdList (CONSState a l (glbMem++[Typedef h]) locMem)

globalMemoryInsert :: MemoryCell -> ProgramState -> ProgramState
globalMemoryInsert var (CONSState a b glb c) = CONSState a b (glb++[var]) c 

convertToVars ((CONSInitialization a b c):inits) = 
    do
        val <- playExp c
        if checkCompatibleTypes a (getTypeFromValue val) then 
            do
                let var = Variable (ConstructVariable b val True "global" 0)
                preResult <- convertToVars inits

                return  (    [var]   ++   preResult   )
        else error("Incompatible types at global declarations")
convertToVars [] =
    do
        return ([])

calculateAndPopulateGlobals ((CONSInitialization a b c):inits) =
    do
        val <- playExp c
        if checkCompatibleTypes a (getTypeFromValue val) then 
            do
                let var = Variable (ConstructVariable b val True "global" 0)
                updateState(globalMemoryInsert var)

                preResult <- calculateAndPopulateGlobals inits
                return (preResult)
        else error("Incompatible types at global declarations")

calculateAndPopulateGlobals [] = 
    do
        return ([])




populateGlobals :: [MemoryCell] -> ProgramState -> ProgramState
populateGlobals [] s = s
populateGlobals (h:tdList) (CONSState a l glbMem locMem) = populateGlobals tdList (CONSState a l (glbMem++[h]) locMem)

populateSubprograms :: [Subprogram] -> ProgramState -> ProgramState
populateSubprograms [] s = s
populateSubprograms (h:tdList) (CONSState a l glbMem locMem) = populateSubprograms tdList (CONSState a l (glbMem++[Subprogram h]) locMem)



_program =
    do
        typedefsBlock <- _typedefsBlock
        globalsBlock <- _globalsBlock
        subprogramsBlock <- _subprogramsBlock

        
        updateState(populateTypedefs (getTypedefs typedefsBlock))

        aux <- convertToVars (getInitializations globalsBlock)

        updateState(populateGlobals aux)

        updateState(populateSubprograms (getSubprograms subprogramsBlock))

        s <- getState

        liftIO(print("PRE MEMORY"))
        liftIO(print(s))

        mainBlock <- _mainBlock
        return (CONSProgram typedefsBlock globalsBlock subprogramsBlock mainBlock)