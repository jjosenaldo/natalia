module PredefBlocks.Grammar where

-- natalia's modules
import Expressions.Grammar
import Statements.Grammar
import TypeValue.TypeValue

-- Haskell modules

data SubprogramsBlock = 
    CONSSubprogramsBlock [Subprogram]
    deriving (Eq, Show)

data Subprogram =
    CONSProcedure String [(Type, String)] [Statement] |
    CONSFunction String [(Type, String)] Type [Statement] 
    deriving (Eq, Show)

data GlobalsBlock = 
    CONSGlobalsBlock [Initialization] 
    deriving (Eq, Show)

data Initialization = 
    CONSInitialization Type String Exp 
    deriving (Eq, Show)

data TypedefsBlock = 
    CONSTypedefsBlock [Typedef] 
    deriving (Eq, Show)

data Typedef = 
    CONSTypedef Type String 
    deriving (Eq, Show)
    
data MainBlock = 
    CONSMainBlock [Statement] 
    deriving (Eq, Show)
    