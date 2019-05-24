module Parsers.Tokens where

import Lexical.Lexemes
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

-- terminal: pre-defined block (main)
mainToken :: ParsecT [Token] st IO (Token)
mainToken = tokenPrim show update_pos get_token where
    get_token Main = Just Main
    get_token _       = Nothing

-- terminal: block opening character
leftBraceToken :: ParsecT [Token] st IO (Token)
leftBraceToken = tokenPrim show update_pos get_token where
    get_token LBrace = Just LBrace
    get_token _       = Nothing

plusToken :: ParsecT [Token] st IO (Token)
plusToken = tokenPrim show update_pos get_token where
    get_token Plus = Just Plus
    get_token _       = Nothing

-- terminal: block closing character
rightBraceToken :: ParsecT [Token] st IO (Token)
rightBraceToken = tokenPrim show update_pos get_token where
    get_token RBrace = Just RBrace
    get_token _       = Nothing

-- terminal: name of the *int* type %TODO: update comment
typeToken :: ParsecT [Token] st IO (Token)
typeToken = tokenPrim show update_pos get_token where
    get_token (Type x) = Just (Type x)
    get_token _        = Nothing 

-- terminal: identifier name
idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show update_pos get_token where
    get_token (Id x) = Just (Id x)
    get_token _      = Nothing

-- terminal: assignment symbol
assignToken :: ParsecT [Token] st IO (Token)
assignToken = tokenPrim show update_pos get_token where
    get_token Assign = Just Assign
    get_token _      = Nothing

-- terminal: literal of type int
intToken :: ParsecT [Token] st IO (Token)
intToken = tokenPrim show update_pos get_token where
    get_token (Int x) = Just (Int x)
    get_token _       = Nothing

-- terminal: command terminator
semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
    get_token SemiColon = Just SemiColon
    get_token _         = Nothing

-- TODO
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  