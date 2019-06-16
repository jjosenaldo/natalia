module Program.Grammar where

-- natalia's modules
import PredefBlocks.Grammar
import TypeValue.TypeValue

data Program = 
    CONSProgram TypedefsBlock GlobalsBlock SubprogramsBlock MainBlock 
    deriving (Eq, Show)