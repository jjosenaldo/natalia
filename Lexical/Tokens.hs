module Lexical.Tokens where

import Lexical.Lexemes
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

-- terminal: pre-defined block (main)
mainToken :: ParsecT [Token] st IO (Token)
mainToken = tokenPrim show update_pos get_token where
    get_token (Main p) = Just (Main p)
    get_token _       = Nothing

-- terminal: block opening character
leftBraceToken :: ParsecT [Token] st IO (Token)
leftBraceToken = tokenPrim show update_pos get_token where
    get_token (LBrace p) = Just (LBrace p)
    get_token _       = Nothing

plus_token :: ParsecT [Token] st IO (Token)
plus_token = tokenPrim show update_pos get_token where
    get_token (Plus p) = Just (Plus p)
    get_token _       = Nothing

minus_token :: ParsecT [Token] st IO (Token)
minus_token = tokenPrim show update_pos get_token where
    get_token (Minus p) = Just (Minus p)
    get_token _       = Nothing

times_token :: ParsecT [Token] st IO (Token)
times_token = tokenPrim show update_pos get_token where
    get_token (Times p) = Just (Times p)
    get_token _       = Nothing

expo_token :: ParsecT [Token] st IO (Token)
expo_token = tokenPrim show update_pos get_token where
    get_token (Expo p) = Just (Expo p)
    get_token _       = Nothing

div_token :: ParsecT [Token] st IO (Token)
div_token = tokenPrim show update_pos get_token where
    get_token (Div p) = Just (Div p)
    get_token _       = Nothing

mod_token :: ParsecT [Token] st IO (Token)
mod_token = tokenPrim show update_pos get_token where
    get_token (Mod p) = Just (Mod p)
    get_token _       = Nothing

-- terminal: block closing character
rightBraceToken :: ParsecT [Token] st IO (Token)
rightBraceToken = tokenPrim show update_pos get_token where
    get_token (RBrace p) = Just (RBrace p)
    get_token _       = Nothing

-- terminal: name of the *int* type %TODO: update comment
typeToken :: ParsecT [Token] st IO (Token)
typeToken = tokenPrim show update_pos get_token where
    get_token (Type x p) = Just (Type x p)
    get_token _        = Nothing 

-- terminal: identifier name
idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show update_pos get_token where
    get_token (Id x p) = Just (Id x p)
    get_token _      = Nothing

-- terminal: assignment symbol
assignToken :: ParsecT [Token] st IO (Token)
assignToken = tokenPrim show update_pos get_token where
    get_token (Assign p) = Just (Assign p)
    get_token _      = Nothing

-- terminal: literal of type int
int_token :: ParsecT [Token] st IO (Token)
int_token = tokenPrim show update_pos get_token where
    get_token (Int x p) = Just (Int x p)
    get_token _       = Nothing

-- literal of type double
double_token :: ParsecT [Token] st IO (Token)
double_token = tokenPrim show update_pos get_token where
    get_token (Double x p) = Just (Double x p)
    get_token _       = Nothing

-- terminal: command terminator
semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
    get_token (SemiColon p) = Just (SemiColon p)
    get_token _         = Nothing

-- TODO
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  