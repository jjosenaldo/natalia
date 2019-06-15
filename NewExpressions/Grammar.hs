module NewExpressions.Grammar where

-- natalia's modules
import Lexical.Lexemes

-- Haskell modules


data Exp = 
    CONSNumExp NumExp |
    CONSBoolExp BoolExp |
    CONSLocalVarExp Token
    deriving (Eq, Show)

data NumExp = 
    CONSNumExpBin NumBinOp NumExp NumExp | 
    CONSNumExpUn NumUnOp NumExp | 
    CONSNumExpLit Token
    deriving (Eq, Show)

data NumBinOp = 
    CONSNumBinOp Token
    deriving (Eq, Show)

data NumUnOp = 
    CONSNumUnOp Token
    deriving (Eq, Show)

data BoolExp = 
    CONSBoolExpBin BoolBinOp BoolExp BoolExp | 
    CONSBoolExpUn BoolUnOp BoolExp | 
    CONSBoolExpBinRel RelBinOp NumExp NumExp | 
    CONSBoolExpLit Token 
    deriving (Eq, Show)

data BoolBinOp = 
    CONSBoolBinOp Token
    deriving (Eq, Show)

data BoolUnOp = 
    CONSBoolUnOp Token
    deriving (Eq, Show)

data RelBinOp = 
    CONSRelBinOp Token
    deriving (Eq, Show)