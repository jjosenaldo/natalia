module Expressions.Expressions where

import Expressions.Evaluation
import Expressions.Operations
import Lexical.Lexemes
import Lexical.Tokens
import Memory.Memory

import Text.Parsec
import Control.Monad.IO.Class

{-

    =, ++x, --x, x++, x--

    un -
  
    ^

    *, /, %

    +, bin -

-}

-- nonterminal: attribution of a value to an *int* variable
var_attribution :: ParsecT [Token] [(Token,Token)] IO(Token)
var_attribution = do
    a <- idToken
    b <- assignToken
    c <- int_token
    updateState(symtable_update(a,c))

    -- optional: print symbols_table content
    s <- getState
    liftIO (print s)

    return (c);

expression_const :: ParsecT [Token] [(Token,Token)] IO(Token)     
expression_const = int_token

expression :: ParsecT [Token] [(Token,Token)] IO(Token)     
expression = expression4

expression0 :: ParsecT [Token] [(Token,Token)] IO(Token)
expression0 = var_attribution <|> expression_const

expression1 :: ParsecT [Token] [(Token,Token)] IO(Token)
expression1 = 
    try
    (do
        op <- minus_token
        n2 <- expression1
        return (unary_eval op n2))
    <|>
    expression0

expression2 :: ParsecT [Token] [(Token,Token)] IO(Token)
expression2 = 
    try
    (do
        n1 <- expression1
        op <- exp_token
        n2 <- expression2
        return (binary_eval n1 op n2))
    <|>
    expression1

expression3 :: ParsecT [Token] [(Token,Token)] IO(Token)
expression3 = 
    do
        n1 <- expression2
        result <- eval_remaining_3 n1 
        return (result)

eval_remaining_3 :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining_3 n1 = 
    try
    (do
        op <- bin_op_left_3_token
        n2 <- expression2
        result <- eval_remaining_3 (binary_eval n1 op n2)
        return (result))
    <|>
    (do
        return (n1))

expression4 :: ParsecT [Token] [(Token,Token)] IO(Token)
expression4 = 
    do
        n1 <- expression3
        result <- eval_remaining_4 n1 
        return (result)

eval_remaining_4 :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining_4 n1 = 
    try
    (do
        op <- bin_op_left_4_token
        n2 <- expression3
        result <- eval_remaining_4 (binary_eval n1 op n2)
        return (result))
    <|>
    (do
        return (n1))