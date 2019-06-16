module PredefBlocks.Grammar where

-- natalia's modules
-- import Lexical.Lexemes
-- import TypeValue.TypeValue

-- Haskell modules

data PredefBlock = 
    CONSSubprogramsBlock [Subprogram] | -- literals
    CONSMainBlock [Statement] | -- binary operations 
    CONSTypedefsBlock [Typedef] | -- unary operations
    CONSGlobalsBlock [Initialization]
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