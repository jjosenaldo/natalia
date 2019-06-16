module Expressions.Grammar where

-- natalia's modules
import Lexical.Lexemes
import TypeValue.TypeValue

-- Haskell modules

data Exp = 
    CONSExpLit Type Token | -- literals

    -- TODO
    CONSExpBin Type BinOp Exp Exp | -- binary operations 

    -- TODO
    CONSExpUn Type UnOp Exp | -- unary operations

    -- TODO
    CONSExpAssign Type LValue Exp | -- assignment

    -- TODO
    CONSExpLValue Type LValue | -- lvalues (they are rvalues aswell)

    -- TODO
    CONSExpStruct Type String [Exp] | -- struct_name{foo1, foo2, ...}
    
    CONSExpSet Type [Exp] | -- {1,2,3}

    -- TODO
    CONSExpFuncCall Type String [Exp] | -- function call 

    -- TODO
    CONSExpCmdZero Type Token | -- binary "command" call (like read())

    -- TODO
    CONSExpCmdUn Type Token Exp  -- unary "command" call (like toString())
    deriving (Eq, Show)

data LValue = 
    CONSLValueId String | -- a = 1;
    CONSLValueStruct String [String] | -- node.left = 1;
    CONSLValueArray String [Exp] | -- v[1][2][3] = 1;
    CONSLValueDerref String Int   -- *ptr = 1;
    deriving (Eq, Show)

data BinOp = 
    CONSBinOp Token
    deriving (Eq, Show)

data UnOp = 
    CONSUnOp Token
    deriving (Eq, Show)