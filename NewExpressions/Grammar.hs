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
    CONSExpUn Type UnOp Exp |
    CONSExpLocalVar Type String | 
    CONSExpAssign Type LValue Exp 
    deriving (Eq, Show)

data LValue = 
    CONSLValueId String | -- a = 1;
    CONSLValueStruct [Token] | -- node.left = 1;
    CONSLValueArray String [Exp] | -- v[1][2][3] = 1;
    CONSLValueDerref String   -- *ptr = 1;
    deriving (Eq, Show)

data BinOp = 
    CONSBinOp Token
    deriving (Eq, Show)

data UnOp = 
    CONSUnOp Token
    deriving (Eq, Show)