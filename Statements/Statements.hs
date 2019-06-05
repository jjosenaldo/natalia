module Statements.Statements where

import Expressions.Expressions
import Memory.Memory
import Lexical.Lexemes
import Lexical.Tokens
import Types.Types

import Text.Parsec
import Control.Monad.IO.Class

-- nonterminal: initialization of an *int* variable
var_initialization :: ParsecT [Token] [(Token,Token)] IO([Token])
var_initialization = do
    a <- typeToken
    b <- idToken
    c <- assignToken
    d <- expression
    e <- getState

    if (not (is_compatible a d)) then fail "type mismatch"
    else
        do
            updateState(symtable_insert (b, d))

    -- optional: print symbols_table content
    s <- getState
    liftIO (print s)

    return (a:b:c:[d])

-- nonterminal: statement
statement :: ParsecT [Token] [(Token,Token)] IO()
statement = 
    try
    (do
        a <- var_initialization
        return ())
    <|>
    (do
        a <- var_attribution
        return ())

-- nonterminal: list of statements
statements :: ParsecT [Token] [(Token,Token)] IO()
statements = 
    try
    (do
        a <- statement
        b <- semiColonToken
        c <- statements
        return ())
    <|>
    (do
        return ())