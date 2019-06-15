module NewExpressions.Grammar where

-- natalia's modules
import Lexical.Lexemes

-- Haskell modules

data Type = 
    NatNothing
    deriving (Eq, Show)

data Exp = 
    CONSExpLit Type Token | 
    CONSExpBin Type BinOp Exp Exp | 
    CONSExpUn Type UnOp Exp 
    deriving (Eq, Show)

data BinOp = 
    CONSBinOp Token
    deriving (Eq, Show)

data UnOp = 
    CONSUnOp Token
    deriving (Eq, Show)