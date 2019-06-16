module Program.Grammar where
import Statements.Grammar
import GlobalsBlock.Grammar
import SubprogramsBlock.Grammar
import TypeValue.TypeValue

data Program = CONSProgram TypedefsBlock GlobalsBlock SubprogramsBlock MainBlock deriving (Eq, Show)

data TypedefsBlock = CONSTypedefsBlock [Typedef] deriving (Eq, Show)

data Typedef = CONSTypedef Type String deriving (Eq, Show)

data MainBlock = CONSMainBlock [Statement] deriving (Eq, Show)
