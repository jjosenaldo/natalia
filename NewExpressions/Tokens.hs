module NewExpressions.Tokens where

-- natalia's modules
import Lexical.Lexemes
import NewExpressions.Grammar

-- Haskell modules
import Text.Parsec

-- plusToken = tokenPrim show updatePos get_token where
--     get_token (Plus p) = Just (  BinOperation (Plus p)   )
--     get_token _       = Nothing

-- timesToken = tokenPrim show updatePos get_token where
--     get_token (Times p) = Just (  BinOperation (Times p)   )
--     get_token _       = Nothing

-- intToken = tokenPrim show updatePos get_token where
--     get_token (Int x p) = Just  (  CONSLitInt x p )
--     get_token _       = Nothing
intToken :: ParsecT [Token] st IO (NumExp)
intToken = tokenPrim show updatePos get_token where
    get_token (Int x p) = Just  (  CONSNumExpLit (Int x p) )
    get_token _       = Nothing

doubleToken :: ParsecT [Token] st IO (NumExp)
doubleToken = tokenPrim show updatePos get_token where
    get_token (Double x p) = Just  (  CONSNumExpLit (Double x p) )
    get_token _       = Nothing

minusUnToken :: ParsecT [Token] st IO (NumExp -> NumExp)
minusUnToken = tokenPrim show updatePos get_token where
    get_token (Minus p) = Just  (  CONSNumExpUn (CONSNumUnOp (Minus p)) )
    get_token _       = Nothing
    
minusBinToken :: ParsecT [Token] st IO (NumExp -> NumExp -> NumExp)
minusBinToken = tokenPrim show updatePos get_token where
    get_token (Minus p) = Just  (  CONSNumExpBin (CONSNumBinOp (Minus p)) )
    get_token _       = Nothing

plusToken :: ParsecT [Token] st IO (NumExp -> NumExp -> NumExp)
plusToken = tokenPrim show updatePos get_token where
    get_token (Plus p) = Just  (  CONSNumExpBin (CONSNumBinOp (Plus p)) )
    get_token _       = Nothing

timesToken :: ParsecT [Token] st IO (NumExp -> NumExp -> NumExp)
timesToken = tokenPrim show updatePos get_token where
    get_token (Times p) = Just  (  CONSNumExpBin (CONSNumBinOp (Times p)) )
    get_token _       = Nothing

divToken :: ParsecT [Token] st IO (NumExp -> NumExp -> NumExp)
divToken = tokenPrim show updatePos get_token where
    get_token (Div p) = Just  (  CONSNumExpBin (CONSNumBinOp (Div p)) )
    get_token _       = Nothing

expoToken :: ParsecT [Token] st IO (NumExp -> NumExp -> NumExp)
expoToken = tokenPrim show updatePos get_token where
    get_token (Expo p) = Just  (  CONSNumExpBin (CONSNumBinOp (Expo p)) )
    get_token _       = Nothing

modToken :: ParsecT [Token] st IO (NumExp -> NumExp -> NumExp)
modToken = tokenPrim show updatePos get_token where
    get_token (Mod p) = Just  (  CONSNumExpBin (CONSNumBinOp (Mod p)) )
    get_token _       = Nothing


boolToken :: ParsecT [Token] st IO (BoolExp)
boolToken = tokenPrim show updatePos get_token where
    get_token (Bool x p) = Just  (  CONSBoolExpLit (Bool x p) )
    get_token _       = Nothing

andToken :: ParsecT [Token] st IO (BoolExp -> BoolExp -> BoolExp)
andToken = tokenPrim show updatePos get_token where
    get_token (And p) = Just  (  CONSBoolExpBin (CONSBoolBinOp (And p)) )
    get_token _       = Nothing

orToken :: ParsecT [Token] st IO (BoolExp -> BoolExp -> BoolExp)
orToken = tokenPrim show updatePos get_token where
    get_token (Or p) = Just  (  CONSBoolExpBin (CONSBoolBinOp (Or p)) )
    get_token _       = Nothing

negationToken :: ParsecT [Token] st IO (BoolExp -> BoolExp)
negationToken = tokenPrim show updatePos get_token where
    get_token (Negation p) = Just  (  CONSBoolExpUn (CONSBoolUnOp (Negation p)) )
    get_token _       = Nothing

lessThan :: ParsecT [Token] st IO (RelBinOp)
lessThan = tokenPrim show updatePos get_token where
    get_token (LessThan p) = Just  (  CONSRelBinOp (LessThan p) ) 
    get_token _       = Nothing

leftParenToken :: ParsecT [Token] st IO (Token)
leftParenToken = tokenPrim show updatePos get_token where
    get_token (LParen p) = Just  (  LParen p ) 
    get_token _       = Nothing

rightParenToken :: ParsecT [Token] st IO (Token)
rightParenToken = tokenPrim show updatePos get_token where
    get_token (RParen p) = Just  (  RParen p ) 
    get_token _       = Nothing


-- stringToken = tokenPrim show updatePos get_token where
--     get_token (String x p) = Just  (  CONSLitString x p)
--     get_token _       = Nothing

-- doubleToken = tokenPrim show updatePos get_token where
--     get_token (Double x p) = Just  (  CONSLitDouble x p)
--     get_token _       = Nothing

-- literals = intToken <|> boolToken <|> stringToken <|> doubleToken

-- TODO
updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos _ (tok:_) = pos -- necessita melhoria
updatePos pos _ []      = pos  