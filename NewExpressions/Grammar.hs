module NewExpressions.Grammar where

-- natalia's modules
import Lexical.Lexemes

-- Haskell modules

data Type = 
    NatNothing
    deriving (Eq, Show)

data Exp = 
    CONSExpLit Type Token | -- literals
    CONSExpBin Type BinOp Exp Exp | -- binary operations 
    CONSExpUn Type UnOp Exp | -- unary operations
    CONSExpAssign Type LValue Exp | -- assignment
    CONSExpLValue Type LValue | -- lvalues (they are rvalues aswell)
    CONSExpStruct Type String [Exp] | -- struct_name{foo1, foo2, ...}
    CONSExpSet Type [Exp] | -- {1,2,3}
    CONSExpFuncCall Type String [Exp] -- function call 
    deriving (Eq, Show)

data LValue = 
    CONSLValueId String | -- a = 1;
    CONSLValueStruct [String] | -- node.left = 1;
    CONSLValueArray String [Exp] | -- v[1][2][3] = 1;
    CONSLValueDerref String Int   -- *ptr = 1;
    deriving (Eq, Show)

data BinOp = 
    CONSBinOp Token
    deriving (Eq, Show)

data UnOp = 
    CONSUnOp Token
    deriving (Eq, Show)