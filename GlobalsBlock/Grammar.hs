module GlobalsBlock.Grammar where

import TypeValue.TypeValue
import Expressions.Parser
import Expressions.Grammar

data GlobalsBlock = CONSGlobalsBlock [Initialization] deriving (Eq, Show)

data Initialization = CONSInitialization Type String Exp deriving (Eq, Show)