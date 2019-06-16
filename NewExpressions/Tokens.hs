module NewExpressions.Tokens where

-- natalia's modules
import Lexical.Lexemes
import NewExpressions.Grammar

-- Haskell modules
import Text.Parsec

intToken :: ParsecT [Token] st IO (Exp)
intToken = tokenPrim show updatePos get_token where
    get_token (Int x p) = Just  (  CONSExpLit NatInt (Int x p) )
    get_token _       = Nothing

doubleToken :: ParsecT [Token] st IO (Exp)
doubleToken = tokenPrim show updatePos get_token where
    get_token (Double x p) = Just  (  CONSExpLit NatDouble (Double x p) )
    get_token _       = Nothing

stringToken :: ParsecT [Token] st IO (Exp)
stringToken = tokenPrim show updatePos get_token where
    get_token (String x p) = Just  (  CONSExpLit NatString (String x p) )
    get_token _       = Nothing

nullToken :: ParsecT [Token] st IO (Exp)
nullToken = tokenPrim show updatePos get_token where
    get_token (Null p) = Just  (  CONSExpLit NatNull (Null p) )
    get_token _       = Nothing

minusUnToken :: ParsecT [Token] st IO (Exp -> Exp)
minusUnToken = tokenPrim show updatePos get_token where
    get_token (Minus p) = Just  (  CONSExpUn NatNothing (CONSUnOp (Minus p))   )
    get_token _       = Nothing

uppersandToken :: ParsecT [Token] st IO (Exp -> Exp)
uppersandToken = tokenPrim show updatePos get_token where
    get_token (Uppersand p) = Just  (  CONSExpUn NatNothing (CONSUnOp (Uppersand p))   )
    get_token _       = Nothing
    
minusBinToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
minusBinToken = tokenPrim show updatePos get_token where
    get_token (Minus p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Minus p)) )
    get_token _       = Nothing

plusToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
plusToken = tokenPrim show updatePos get_token where
    get_token (Plus p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Plus p)) )
    get_token _       = Nothing

timesToken :: ParsecT [Token] st IO (Token)
timesToken = tokenPrim show updatePos get_token where
    get_token (Times p) = Just  (  Times p )
    get_token _       = Nothing

timesTokenAsNumOp = 
    do 
        times <- timesToken
        return $ CONSExpBin NatNothing (CONSBinOp times)

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
    get_token (Bool x p) = Just  (  CONSExpLit NatNull (Bool x p) )
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

interrogationToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
interrogationToken = tokenPrim show updatePos get_token where
    get_token (Interrogation p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Interrogation p)) )
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

leftBraceToken :: ParsecT [Token] st IO (Token)
leftBraceToken = tokenPrim show updatePos get_token where
    get_token (LBrace p) = Just  (  LBrace p ) 
    get_token _       = Nothing

rightBraceToken :: ParsecT [Token] st IO (Token)
rightBraceToken = tokenPrim show updatePos get_token where
    get_token (RBrace p) = Just  (  RBrace p ) 
    get_token _       = Nothing

leftBracketToken :: ParsecT [Token] st IO (Token)
leftBracketToken = tokenPrim show updatePos get_token where
    get_token (LBracket p) = Just  (  LBracket p ) 
    get_token _       = Nothing

rightBracketToken :: ParsecT [Token] st IO (Token)
rightBracketToken = tokenPrim show updatePos get_token where
    get_token (RBracket p) = Just  (  RBracket p ) 
    get_token _       = Nothing

assignToken :: ParsecT [Token] st IO (Token)
assignToken = tokenPrim show updatePos get_token where
    get_token (Assign p) = Just  (  Assign p ) 
    get_token _       = Nothing

idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show updatePos get_token where
    get_token (Id x p) = Just  ( Id x p ) 
    get_token _       = Nothing

dotToken :: ParsecT [Token] st IO (Token)
dotToken = tokenPrim show updatePos get_token where
    get_token (Dot p) = Just  ( Dot p ) 
    get_token _       = Nothing

commaToken :: ParsecT [Token] st IO (Token)
commaToken = tokenPrim show updatePos get_token where
    get_token (Comma p) = Just  ( Comma p ) 
    get_token _       = Nothing

toDoubleToken :: ParsecT [Token] st IO (Token)
toDoubleToken = tokenPrim show updatePos get_token where
    get_token (ToDouble p) = Just  ( ToDouble p ) 
    get_token _       = Nothing

toIntToken :: ParsecT [Token] st IO (Token)
toIntToken = tokenPrim show updatePos get_token where
    get_token (ToInt p) = Just  ( ToInt p ) 
    get_token _       = Nothing

toStringToken :: ParsecT [Token] st IO (Token)
toStringToken = tokenPrim show updatePos get_token where
    get_token (ToString p) = Just  ( ToString p ) 
    get_token _       = Nothing

toBoolToken :: ParsecT [Token] st IO (Token)
toBoolToken = tokenPrim show updatePos get_token where
    get_token (ToBool p) = Just  ( ToBool p ) 
    get_token _       = Nothing

readToken :: ParsecT [Token] st IO (Token)
readToken = tokenPrim show updatePos get_token where
    get_token (Read p) = Just  ( Read p ) 
    get_token _       = Nothing

-- TODO
updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos _ (tok:_) = pos -- necessita melhoria
updatePos pos _ []      = pos  