module SubprogramsBlock.Grammar where

    
data SubprogramsBlock = 
    CONSSubprogramBlock [Subprogram]
    deriving (Eq, Show)

data Subprogram =
    CONSProcedure String [(Type, String)] [Statement]
    CONSFunction String [(Type, String)] deriving (Eq, Show)