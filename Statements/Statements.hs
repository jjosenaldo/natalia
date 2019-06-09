module Statements.Statements where

import Expressions.Expressions
import Memory.Memory
import Lexical.Lexemes
import Lexical.Tokens
import Types.Types

import Text.Parsec
import Control.Monad.IO.Class

-- nonterminal: initialization of an *int* variable
var_initialization :: ParsecT [Token] [(Token,Token)] IO()
var_initialization = do
    t <- typeToken
    name <- id_token
    ass <- assignToken
    expr_value <- expression
    s <- getState

    let expr_type = get_value_type expr_value

    if (not (attr_compatible_types t expr_type)) then fail ("ERROR at " ++ show(get_pos expr_value)  ++ ": type mismatch in the initialization of a variable.")
    else
        do
            updateState(symtable_insert (name, expr_value))
            -- optional: print symbols_table content
            s <- getState
            liftIO (print s)
            return ()

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