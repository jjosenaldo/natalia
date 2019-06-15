module NewExpressions.Grammar where

-- natalia's modules
import Lexical.Lexemes

-- Haskell modules


data Exp = 
    CONSExpBool BoolExp
    deriving (Eq, Show)

data BoolExp = 
    CONSBoolExpBin BoolBinOp BoolExp BoolExp | 
    CONSBoolExpUn BoolUnOp BoolExp | 
    CONSBoolExpLit Token
    deriving (Eq, Show)

data BoolBinOp = 
    CONSBoolBinOp Token
    deriving (Eq, Show)

data BoolUnOp = 
    CONSBoolUnOp Token
    deriving (Eq, Show)

-- data Lit = 
--     CONSLitInt Integer (Int, Int) |
--     CONSLitBool Bool (Int, Int) | 
--     CONSLitDouble Double (Int, Int) | 
--     CONSLitString String (Int, Int) 
    -- deriving (Eq, Show)