module NewExpressions.Tokens where

-- natalia's modules
import Lexical.Lexemes
import NewExpressions.Grammar

-- Haskell modules
import Text.Parsec

intToken :: ParsecT [Token] st IO (Exp)
intToken = tokenPrim show updatePos get_token where
    get_token (Int x p) = Just  (  CONSExpLit NatNothing (Int x p) )
    get_token _       = Nothing

doubleToken :: ParsecT [Token] st IO (Exp)
doubleToken = tokenPrim show updatePos get_token where
    get_token (Double x p) = Just  (  CONSExpLit NatNothing (Double x p) )
    get_token _       = Nothing

minusUnToken :: ParsecT [Token] st IO (Exp -> Exp)
minusUnToken = tokenPrim show updatePos get_token where
    get_token (Minus p) = Just  (  CONSExpUn NatNothing (CONSUnOp (Minus p))   )
    get_token _       = Nothing
    
minusBinToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
minusBinToken = tokenPrim show updatePos get_token where
    get_token (Minus p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Minus p)) )
    get_token _       = Nothing

plusToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
plusToken = tokenPrim show updatePos get_token where
    get_token (Plus p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Plus p)) )
    get_token _       = Nothing

timesToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
timesToken = tokenPrim show updatePos get_token where
    get_token (Times p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Times p)) )
    get_token _       = Nothing

divToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
divToken = tokenPrim show updatePos get_token where
    get_token (Div p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Div p)) )
    get_token _       = Nothing

expoToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
expoToken = tokenPrim show updatePos get_token where
    get_token (Expo p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Expo p)) )
    get_token _       = Nothing

modToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
modToken = tokenPrim show updatePos get_token where
    get_token (Mod p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Mod p)) )
    get_token _       = Nothing


boolToken :: ParsecT [Token] st IO (Exp)
boolToken = tokenPrim show updatePos get_token where
    get_token (Bool x p) = Just  (  CONSExpLit NatNothing (Bool x p) )
    get_token _       = Nothing

andToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
andToken = tokenPrim show updatePos get_token where
    get_token (And p) = Just  (  CONSExpBin NatNothing (CONSBinOp (And p)) )
    get_token _       = Nothing

orToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
orToken = tokenPrim show updatePos get_token where
    get_token (Or p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Or p)) )
    get_token _       = Nothing

negationToken :: ParsecT [Token] st IO (Exp -> Exp)
negationToken = tokenPrim show updatePos get_token where
    get_token (Negation p) = Just  (  CONSExpUn NatNothing (CONSUnOp (Negation p))   )
    get_token _       = Nothing

lessThanToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
lessThanToken = tokenPrim show updatePos get_token where
    get_token (LessThan p) = Just  (  CONSExpBin NatNothing (CONSBinOp (LessThan p)) )
    get_token _       = Nothing

greaterThanToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
greaterThanToken = tokenPrim show updatePos get_token where
    get_token (GreaterThan p) = Just  (  CONSExpBin NatNothing (CONSBinOp (GreaterThan p)) )
    get_token _       = Nothing

lessEqualsToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
lessEqualsToken = tokenPrim show updatePos get_token where
    get_token (LessEquals p) = Just  (  CONSExpBin NatNothing (CONSBinOp (LessEquals p)) )
    get_token _       = Nothing

greaterEqualsToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
greaterEqualsToken = tokenPrim show updatePos get_token where
    get_token (GreaterEquals p) = Just  (  CONSExpBin NatNothing (CONSBinOp (GreaterEquals p)) )
    get_token _       = Nothing

equalsToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
equalsToken = tokenPrim show updatePos get_token where
    get_token (Equals p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Equals p)) )
    get_token _       = Nothing

differentToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
differentToken = tokenPrim show updatePos get_token where
    get_token (Different p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Different p)) )
    get_token _       = Nothing

leftParenToken :: ParsecT [Token] st IO (Token)
leftParenToken = tokenPrim show updatePos get_token where
    get_token (LParen p) = Just  (  LParen p ) 
    get_token _       = Nothing

rightParenToken :: ParsecT [Token] st IO (Token)
rightParenToken = tokenPrim show updatePos get_token where
    get_token (RParen p) = Just  (  RParen p ) 
    get_token _       = Nothing

idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show updatePos get_token where
    get_token (Id x p) = Just  (  Id x p ) 
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