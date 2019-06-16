module Program.Parser where

import PredefBlocks.Grammar
import PredefBlocks.Parser
import Program.Grammar

_program =
    do
        --typedefsBlock <- _typedefsBlock
        globalsBlock <- _globalsBlock
        subprogramsBlock <- _subprogramsBlock
        mainBlock <- _mainBlock
        return (CONSProgram (CONSTypedefsBlock []) globalsBlock subprogramsBlock (CONSMainBlock []))