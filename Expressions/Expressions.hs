module Expressions.Expressions where

import Expressions.Evaluation
import Expressions.Operations
import Lexical.Lexemes
import Lexical.Tokens
import Memory.Memory
import Types.Types

import Text.Parsec
import Control.Monad.IO.Class

-- General expression
expression :: ParsecT [Token] [(Token,Token)] IO(Token)     
expression = exp_num

-- A numeric expression
exp_num :: ParsecT [Token] [(Token,Token)] IO(Token)     
exp_num = exp_num4

-- Assignment of a value to a variable
var_attribution :: ParsecT [Token] [(Token,Token)] IO(Token)
var_attribution = do
    a <- idToken
    b <- assignToken
    c <- expression
    s <- getState
    let var_type = var_type_from_name a s
    let expr_type = get_value_type c

    if (not (attr_compatible_types var_type expr_type)) then fail ("ERROR at " ++ show(get_pos c)  ++ ": type mismatch in the attribution of a value to a variable.")
    else
        do
            updateState(symtable_update(a,c))
            
            -- optional: print symbols_table content
            s <- getState
            liftIO (print s)
            return (c)

-- Constant expression
exp_const :: ParsecT [Token] [(Token,Token)] IO(Token)     
exp_const = int_token <|> double_token

-- Parenthesized expression
exp_parenthesized :: ParsecT [Token] [(Token,Token)] IO(Token)
exp_parenthesized = 
    do
        lparen <- left_paren_token
        expr <- expression
        rparen <- right_paren_token
        return (expr)

-- Expression that has precedence 0
exp0 :: ParsecT [Token] [(Token,Token)] IO(Token)
exp0 = var_attribution <|> exp_const <|> exp_parenthesized

-- Numeric expression that has precedence 1
exp_num1 :: ParsecT [Token] [(Token,Token)] IO(Token)
exp_num1 = 
    try
    (do
        op <- minus_token
        n2 <- exp_num1
        return (unary_eval op n2))
    <|>
    exp0

-- Numeric expression that has precedence 2
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

-- Numeric expression that has precedence 3
exp_num3 :: ParsecT [Token] [(Token,Token)] IO(Token)
exp_num3 = 
    do
        n1 <- exp_num2
        result <- eval_remaining_exp_num3 n1 
        return (result)

-- Evaluates an expression that has precedence 3
eval_remaining_exp_num3 :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining_exp_num3 n1 = 
    try
    (do
        op <- bin_op_left_3_token
        n2 <- exp_num2
        result <- eval_remaining_exp_num3 (binary_eval n1 op n2)
        return (result))
    <|>
    (do
        return (n1))

-- Numeric expression that has precedence 4
exp_num4 :: ParsecT [Token] [(Token,Token)] IO(Token)
exp_num4 = 
    do
        n1 <- exp_num3
        result <- eval_remaining_exp_num4 n1 
        return (result)

-- Evaluates an expression that has precedence 4
eval_remaining_exp_num4 :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining_exp_num4 n1 = 
    try
    (do
        op <- bin_op_left_4_token
        n2 <- exp_num3
        result <- eval_remaining_exp_num4 (binary_eval n1 op n2)
        return (result))
    <|>
    (do
        return (n1))