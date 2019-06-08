module Expressions.Expressions where

import Expressions.Evaluation
import Expressions.Operations
import Lexical.Lexemes
import Lexical.Tokens
import Memory.Memory

import Text.Parsec
import Control.Monad.IO.Class

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

exp_const :: ParsecT [Token] [(Token,Token)] IO(Token)     
exp_const = int_token

expression :: ParsecT [Token] [(Token,Token)] IO(Token)     
expression = exp_num4

expression0 :: ParsecT [Token] [(Token,Token)] IO(Token)
expression0 = var_attribution <|> exp_const

exp_num1 :: ParsecT [Token] [(Token,Token)] IO(Token)
exp_num1 = 
    try
    (do
        op <- minus_token
        n2 <- exp_num1
        return (unary_eval op n2))
    <|>
    expression0

exp_num2 :: ParsecT [Token] [(Token,Token)] IO(Token)
exp_num2 = 
    try
    (do
        n1 <- exp_num1
        op <- expo_token
        n2 <- exp_num2
        return (binary_eval n1 op n2))
    <|>
    exp_num1

exp_num3 :: ParsecT [Token] [(Token,Token)] IO(Token)
exp_num3 = 
    do
        n1 <- exp_num2
        result <- eval_remaining3 n1 
        return (result)

eval_remaining3 :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining3 n1 = 
    try
    (do
        op <- bin_op_left_3_token
        n2 <- exp_num2
        result <- eval_remaining3 (binary_eval n1 op n2)
        return (result))
    <|>
    (do
        return (n1))

exp_num4 :: ParsecT [Token] [(Token,Token)] IO(Token)
exp_num4 = 
    do
        n1 <- exp_num3
        result <- eval_remaining4 n1 
        return (result)

eval_remaining4 :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining4 n1 = 
    try
    (do
        op <- bin_op_left_4_token
        n2 <- exp_num3
        result <- eval_remaining4 (binary_eval n1 op n2)
        return (result))
    <|>
    (do
        return (n1))