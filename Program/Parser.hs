module Program.Parser where

-- natalia's modules
import PredefBlocks.Grammar
import PredefBlocks.Parser
import Program.Grammar

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
        mainBlock <- _mainBlock
        return (CONSProgram typedefsBlock globalsBlock subprogramsBlock mainBlock)