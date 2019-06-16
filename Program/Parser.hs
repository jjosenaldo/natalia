module Program.Parser where

import Program.Grammar
import SubprogramsBlock.Parser
import GlobalsBlock.Parser

_program =
    do
        --typedefsBlock <- _typedefsBlock
        globalsBlock <- _globalsBlock
        subprogramsBlock <- _subprogramsBlock
        --mainBlock <- _mainBlock
        return (CONSProgram (CONSTypedefsBlock []) globalsBlock subprogramsBlock (CONSMainBlock []))