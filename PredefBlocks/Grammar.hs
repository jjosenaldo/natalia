module PredefBlocks.Grammar where

-- natalia's modules
import Expressions.Grammar
import Statements.Grammar
import TypeValue.TypeValue
import Types.Typedef

-- Haskell modules

data SubprogramsBlock = 
    CONSSubprogramsBlock [Subprogram]
    deriving (Eq, Show)

getSubprograms (CONSSubprogramsBlock t) = t

data Subprogram =
    CONSProcedure String [(Type, String)] [Statement] |
    CONSFunction String [(Type, String)] Type [Statement] 
    deriving (Eq, Show)

data GlobalsBlock = 
    CONSGlobalsBlock [Initialization] 
    deriving (Eq, Show)

getInitializations (CONSGlobalsBlock t) = t

data Initialization = 
    CONSInitialization Type String Exp 
    deriving (Eq, Show)

getInitializationType (CONSInitialization a b c) = a
getInitializationId (CONSInitialization a b c) = b
getInitializationExp (CONSInitialization a b c) = c

data TypedefsBlock = 
    CONSTypedefsBlock [Typedef] 
    deriving (Eq, Show)

getTypedefs (CONSTypedefsBlock t) = t

-- data Typedef = 
--     CONSTypedef Type String 
--     deriving (Eq, Show)
    
data MainBlock = 
    CONSMainBlock [Statement] 
    deriving (Eq, Show)
    
getMainBlockStatements :: MainBlock -> [Statement]
getMainBlockStatements (CONSMainBlock ss) = ss