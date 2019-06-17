module Expressions.Grammar where

-- natalia's modules
import Lexical.Lexemes
import TypeValue.TypeValue


-- Haskell modules

data Exp = 
    CONSExpLit Type Token | -- literals
    CONSExpBin Type BinOp Exp Exp | -- binary operations 
    CONSExpUn Type UnOp Exp | -- unary operations
    CONSExpAssign Type LValue Exp | -- assignment
    CONSExpLValue Type LValue | -- lvalues (they are rvalues aswell)
    CONSExpStruct Type String [Exp] | -- struct_name{foo1, foo2, ...}
    CONSExpSet Type [Exp] | -- {1,2,3}
    CONSExpFuncCall Type String [Exp] | -- function call 
    CONSExpCmdZero Type Token | -- binary "command" call (like read())
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

-- FIELD FUNCTIONS FOR EXPS ---------------------------------------------------------------------

getExpLitToken :: Exp -> Maybe Token 
getExpLitToken (CONSExpLit _ tok) = Just tok
getExpLitToken _ = Nothing

getExpBinBinOp :: Exp -> BinOp 
getExpBinBinOp (CONSExpBin _ op _ _) = op

getExpBinExp1 :: Exp -> Exp
getExpBinExp1 (CONSExpBin _ _ exp1 _) = exp1 

getExpBinExp2 :: Exp -> Exp
getExpBinExp2 (CONSExpBin _ _ _ exp2) = exp2

getExpLValue :: Exp -> Maybe LValue
getExpLValue (CONSExpLValue _ lv) = Just lv
getExpLValue _ = Nothing

-- FIELD FUNCTIONS FOR BINOPS ------------------------------------------------------------

getBinOpTok :: BinOp -> Token
getBinOpTok (CONSBinOp tok) = tok

-- FIELD FUNCTIONS FOR LVALUES --------------------------------------------------------

isLValueLocalVar :: LValue -> Bool
isLValueLocalVar (CONSLValueId _) = True 
isLValueLocalVar _ = False

getLocalVarId :: LValue -> String 
getLocalVarId (CONSLValueId id) = id 

isLValueStruct :: LValue -> Bool 
isLValueStruct (CONSLValueStruct _ _ ) = True 
isLValueStruct _ = False

isLValueArray :: LValue -> Bool 
isLValueArray (CONSLValueArray _ _ ) = True 
isLValueArray _ = False

isLValueDerref :: LValue -> Bool 
isLValueDerref (CONSLValueDerref _ _ ) = True 
isLValueDerref _ = False