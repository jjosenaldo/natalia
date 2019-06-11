module Lexical.Tokens where

import Lexical.Lexemes
import Text.Parsec
import Control.Monad.IO.Class
import Types.Types
import System.IO.Unsafe
import Memory.Memory


data ReturnObject = RetToken Token | RetValue Value | RetMemoryCell MemoryCell deriving (Eq, Show)

getRetToken::ReturnObject -> Token
getRetToken (RetToken t) = t
getRetToken _ = error "Invalid conversion from ReturnObject to RetToken"

getRetValue::ReturnObject -> Value
getRetValue (RetValue v) = v
getRetValue _ = error "Invalid conversion from ReturnObject to RetValue"

getRetMemoryCell::ReturnObject -> MemoryCell
getRetMemoryCell (RetMemoryCell var) = var
getRetMemoryCell _ = error "Invalid conversion from ReturnObject to RetValue"

-- Pre-defined block (main)
mainToken :: ParsecT [Token] st IO (ReturnObject)
mainToken = tokenPrim show update_pos get_token where
    get_token (Main p) = Just (RetToken (Main p))
    get_token _       = Nothing

-- Block opening character
leftBraceToken :: ParsecT [Token] st IO (ReturnObject)
leftBraceToken = tokenPrim show update_pos get_token where
    get_token (LBrace p) = Just (RetToken (LBrace p))
    get_token _       = Nothing


-- terminal: block closing character
rightBraceToken :: ParsecT [Token] st IO (ReturnObject)
rightBraceToken = tokenPrim show update_pos get_token where
    get_token (RBrace p) = Just (RetToken (RBrace p))
    get_token _       = Nothing

-- Left parenthesis
left_paren_token :: ParsecT [Token] st IO (Token)
left_paren_token = tokenPrim show update_pos get_token where
    get_token (LParen p) = Just (LParen p)
    get_token _       = Nothing

-- Right parenthesis
right_paren_token :: ParsecT [Token] st IO (Token)
right_paren_token = tokenPrim show update_pos get_token where
    get_token (RParen p) = Just (RParen p)
    get_token _       = Nothing

plus_token :: ParsecT [Token] st IO (ReturnObject)
plus_token = tokenPrim show update_pos get_token where
    get_token (Plus p) = Just (RetToken (Plus p))
    get_token _       = Nothing


less_than_token :: ParsecT [Token] st IO (ReturnObject)
less_than_token = tokenPrim show update_pos get_token where
    get_token (LessThan p)  = Just (RetToken (LessThan p))
    get_token _             = Nothing

minus_token :: ParsecT [Token] st IO (ReturnObject)
minus_token = tokenPrim show update_pos get_token where
    get_token (Minus p) = Just (RetToken (Minus p))
    get_token _       = Nothing

times_token :: ParsecT [Token] st IO (ReturnObject)
times_token = tokenPrim show update_pos get_token where
    get_token (Times p) = Just (RetToken (Times p))
    get_token _       = Nothing

expo_token :: ParsecT [Token] st IO (ReturnObject)
expo_token = tokenPrim show update_pos get_token where
    get_token (Expo p) = Just (RetToken (Expo p))
    get_token _       = Nothing

div_token :: ParsecT [Token] st IO (ReturnObject)
div_token = tokenPrim show update_pos get_token where
    get_token (Div p) = Just (RetToken (Div p))
    get_token _       = Nothing

mod_token :: ParsecT [Token] st IO (ReturnObject)
mod_token = tokenPrim show update_pos get_token where
    get_token (Mod p) = Just (RetToken (Mod p))
    get_token _       = Nothing


-- terminal: name of the *int* type %TODO: update comment
typeToken :: ParsecT [Token] st IO (ReturnObject)
typeToken = tokenPrim show update_pos get_token where
    get_token (Type x p) = Just (RetToken (Type x p))
    get_token _        = Nothing 

-- Identifier (of a variable/function/procedure/etc)
id_token :: ParsecT [Token] st IO (ReturnObject)
id_token = tokenPrim show update_pos get_token where
    get_token (Id x p) = Just (RetToken (Id x p))
    get_token _      = Nothing

-- terminal: assignment symbol
assignToken :: ParsecT [Token] st IO (ReturnObject)
assignToken = tokenPrim show update_pos get_token where
    get_token (Assign p) = Just (RetToken (Assign p))
    get_token _      = Nothing

-- terminal: literal of type int
int_token :: ParsecT [Token] st IO (ReturnObject)
int_token = tokenPrim show update_pos get_token where
    get_token (Int x _) = Just (RetValue (ConsNatInt x))
    get_token _       = Nothing

-- literal of type double
double_token :: ParsecT [Token] st IO (ReturnObject)
double_token = tokenPrim show update_pos get_token where
    get_token (Double x p) = Just (RetValue (ConsNatDouble x))
    get_token _       = Nothing

equalsToken :: ParsecT [Token] st IO (ReturnObject)
equalsToken = tokenPrim show update_pos get_token where
    get_token (Equals p) = Just (RetToken (Equals p))
    get_token _            = Nothing

andToken :: ParsecT [Token] st IO (ReturnObject)
andToken = tokenPrim show update_pos get_token where
    get_token (And p) = Just (RetToken (And p))
    get_token _         = Nothing

orToken :: ParsecT [Token] st IO (ReturnObject)
orToken = tokenPrim show update_pos get_token where
    get_token (Or p) = Just (RetToken (Or p))
    get_token _         = Nothing
    

-- terminal: command terminator
semiColonToken :: ParsecT [Token] st IO (ReturnObject)
semiColonToken = tokenPrim show update_pos get_token where
    get_token (SemiColon p) = Just (RetToken (SemiColon p))
    get_token _         = Nothing

negationToken :: ParsecT [Token] st IO (ReturnObject)
negationToken = tokenPrim show update_pos get_token where
    get_token (Negation p)  = Just (RetToken (Negation p))
    get_token _             = Nothing



-- TODO
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  