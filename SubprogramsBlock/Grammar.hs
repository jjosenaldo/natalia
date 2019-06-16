module SubprogramsBlock.Grammar where

import TypeValue.TypeValue
import Statements.Grammar 
import TypeValue.TypeValue

data SubprogramsBlock = 
    CONSSubprogramsBlock [Subprogram]
    deriving (Eq, Show)

data Subprogram =
    CONSProcedure String [(Type, String)] [Statement] |
    CONSFunction String [(Type, String)] Type [Statement] deriving (Eq, Show)