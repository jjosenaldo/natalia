module Lexical.Tokens where

-- natalia's modules
import Expressions.Grammar
import Lexical.Lexemes
import Memory.Memory
import TypeValue.TypeValue
import Types.Typedef
import Types.Types

-- Haskell modules
import Text.Parsec
import Control.Monad.IO.Class
import System.IO.Unsafe

_intToken :: ParsecT [Token] st IO (Exp)
_intToken = tokenPrim show updatePos get_token where
    get_token (Int x p) = Just  (  CONSExpLit NatInt (Int x p) )
    get_token _       = Nothing

_doubleToken :: ParsecT [Token] st IO (Exp)
_doubleToken = tokenPrim show updatePos get_token where
    get_token (Double x p) = Just  (  CONSExpLit NatDouble (Double x p) )
    get_token _       = Nothing

_stringToken :: ParsecT [Token] st IO (Exp)
_stringToken = tokenPrim show updatePos get_token where
    get_token (String x p) = Just  (  CONSExpLit NatString (String x p) )
    get_token _       = Nothing

_nullToken :: ParsecT [Token] st IO (Exp)
_nullToken = tokenPrim show updatePos get_token where
    get_token (Null p) = Just  (  CONSExpLit NatNull (Null p) )
    get_token _       = Nothing

_minusUnToken :: ParsecT [Token] st IO (Exp -> Exp)
_minusUnToken = tokenPrim show updatePos get_token where
    get_token (Minus p) = Just  (  CONSExpUn NatNothing (CONSUnOp (Minus p))   )
    get_token _       = Nothing

_uppersandToken :: ParsecT [Token] st IO (Exp -> Exp)
_uppersandToken = tokenPrim show updatePos get_token where
    get_token (Uppersand p) = Just  (  CONSExpUn NatNothing (CONSUnOp (Uppersand p))   )
    get_token _       = Nothing
    
_minusBinToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_minusBinToken = tokenPrim show updatePos get_token where
    get_token (Minus p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Minus p)) )
    get_token _       = Nothing

_plusToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_plusToken = tokenPrim show updatePos get_token where
    get_token (Plus p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Plus p)) )
    get_token _       = Nothing

_timesToken :: ParsecT [Token] st IO (Token)
_timesToken = tokenPrim show updatePos get_token where
    get_token (Times p) = Just  (  Times p )
    get_token _       = Nothing

_timesTokenAsNumOp = 
    do 
        times <- _timesToken
        return $ CONSExpBin NatNothing (CONSBinOp times)

_divToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_divToken = tokenPrim show updatePos get_token where
    get_token (Div p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Div p)) )
    get_token _       = Nothing

_expoToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_expoToken = tokenPrim show updatePos get_token where
    get_token (Expo p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Expo p)) )
    get_token _       = Nothing

_modToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_modToken = tokenPrim show updatePos get_token where
    get_token (Mod p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Mod p)) )
    get_token _       = Nothing


_boolToken :: ParsecT [Token] st IO (Exp)
_boolToken = tokenPrim show updatePos get_token where
    get_token (Bool x p) = Just  (  CONSExpLit NatBool (Bool x p) )
    get_token _       = Nothing

_andToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_andToken = tokenPrim show updatePos get_token where
    get_token (And p) = Just  (  CONSExpBin NatNothing (CONSBinOp (And p)) )
    get_token _       = Nothing

_orToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_orToken = tokenPrim show updatePos get_token where
    get_token (Or p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Or p)) )
    get_token _       = Nothing

_negationToken :: ParsecT [Token] st IO (Exp -> Exp)
_negationToken = tokenPrim show updatePos get_token where
    get_token (Negation p) = Just  (  CONSExpUn NatNothing (CONSUnOp (Negation p))   )
    get_token _       = Nothing

_lessThanToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_lessThanToken = tokenPrim show updatePos get_token where
    get_token (LessThan p) = Just  (  CONSExpBin NatNothing (CONSBinOp (LessThan p)) )
    get_token _       = Nothing

_greaterThanToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_greaterThanToken = tokenPrim show updatePos get_token where
    get_token (GreaterThan p) = Just  (  CONSExpBin NatNothing (CONSBinOp (GreaterThan p)) )
    get_token _       = Nothing

_lessEqualsToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_lessEqualsToken = tokenPrim show updatePos get_token where
    get_token (LessEquals p) = Just  (  CONSExpBin NatNothing (CONSBinOp (LessEquals p)) )
    get_token _       = Nothing

_greaterEqualsToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_greaterEqualsToken = tokenPrim show updatePos get_token where
    get_token (GreaterEquals p) = Just  (  CONSExpBin NatNothing (CONSBinOp (GreaterEquals p)) )
    get_token _       = Nothing

_interrogationToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_interrogationToken = tokenPrim show updatePos get_token where
    get_token (Interrogation p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Interrogation p)) )
    get_token _       = Nothing

_equalsToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_equalsToken = tokenPrim show updatePos get_token where
    get_token (Equals p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Equals p)) )
    get_token _       = Nothing

_differentToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_differentToken = tokenPrim show updatePos get_token where
    get_token (Different p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Different p)) )
    get_token _       = Nothing

_leftParenToken :: ParsecT [Token] st IO (Token)
_leftParenToken = tokenPrim show updatePos get_token where
    get_token (LParen p) = Just  (  LParen p ) 
    get_token _       = Nothing

_rightParenToken :: ParsecT [Token] st IO (Token)
_rightParenToken = tokenPrim show updatePos get_token where
    get_token (RParen p) = Just  (  RParen p ) 
    get_token _       = Nothing

_leftBraceToken :: ParsecT [Token] st IO (Token)
_leftBraceToken = tokenPrim show updatePos get_token where
    get_token (LBrace p) = Just  (  LBrace p ) 
    get_token _       = Nothing

_rightBraceToken :: ParsecT [Token] st IO (Token)
_rightBraceToken = tokenPrim show updatePos get_token where
    get_token (RBrace p) = Just  (  RBrace p ) 
    get_token _       = Nothing

_leftBracketToken :: ParsecT [Token] st IO (Token)
_leftBracketToken = tokenPrim show updatePos get_token where
    get_token (LBracket p) = Just  (  LBracket p ) 
    get_token _       = Nothing

_rightBracketToken :: ParsecT [Token] st IO (Token)
_rightBracketToken = tokenPrim show updatePos get_token where
    get_token (RBracket p) = Just  (  RBracket p ) 
    get_token _       = Nothing

_assignToken :: ParsecT [Token] st IO (Token)
_assignToken = tokenPrim show updatePos get_token where
    get_token (Assign p) = Just  (  Assign p ) 
    get_token _       = Nothing

_idToken :: ParsecT [Token] st IO (Token)
_idToken = tokenPrim show updatePos get_token where
    get_token (Id x p) = Just  ( Id x p ) 
    get_token _       = Nothing

_dotToken :: ParsecT [Token] st IO (Token)
_dotToken = tokenPrim show updatePos get_token where
    get_token (Dot p) = Just  ( Dot p ) 
    get_token _       = Nothing

_commaToken :: ParsecT [Token] st IO (Token)
_commaToken = tokenPrim show updatePos get_token where
    get_token (Comma p) = Just  ( Comma p ) 
    get_token _       = Nothing

_semicolonToken :: ParsecT [Token] st IO (Token)
_semicolonToken = tokenPrim show updatePos get_token where
    get_token (SemiColon p) = Just  ( SemiColon p ) 
    get_token _       = Nothing

_toDoubleToken :: ParsecT [Token] st IO (Token)
_toDoubleToken = tokenPrim show updatePos get_token where
    get_token (ToDouble p) = Just  ( ToDouble p ) 
    get_token _       = Nothing

_toIntToken :: ParsecT [Token] st IO (Token)
_toIntToken = tokenPrim show updatePos get_token where
    get_token (ToInt p) = Just  ( ToInt p ) 
    get_token _       = Nothing

_toStringToken :: ParsecT [Token] st IO (Token)
_toStringToken = tokenPrim show updatePos get_token where
    get_token (ToString p) = Just  ( ToString p ) 
    get_token _       = Nothing

_toBoolToken :: ParsecT [Token] st IO (Token)
_toBoolToken = tokenPrim show updatePos get_token where
    get_token (ToBool p) = Just  ( ToBool p ) 
    get_token _       = Nothing

_readToken :: ParsecT [Token] st IO (Token)
_readToken = tokenPrim show updatePos get_token where
    get_token (Read p) = Just  ( Read p ) 
    get_token _       = Nothing

_printToken :: ParsecT [Token] st IO (Token)
_printToken = tokenPrim show updatePos get_token where
    get_token (Print p) = Just  ( Print p ) 
    get_token _       = Nothing

_lexicalTypeToken :: ParsecT [Token] st IO (Token)
_lexicalTypeToken = tokenPrim show updatePos get_token where
    get_token (Type x p) = Just (Type x p)
    get_token _        = Nothing 

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos _ (tok:_) = pos -- necessita melhoria
updatePos pos _ []      = pos  
