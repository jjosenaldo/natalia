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
expression = exp4

-- Expression that has precedence 4
exp4 :: ParsecT [Token] [(Token,Token)] IO(Token)
exp4 = 
    do
        n1 <- exp3
        result <- eval_remaining_exp4 n1 
        return (result)

-- Evaluates the remainder of a expression that has precedence 4
eval_remaining_exp4 :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining_exp4 n1 = 
    try
    (do
        op <- bin_num_op_left_4_token
        n2 <- exp3
        result <- eval_remaining_exp4 (binary_eval n1 op n2)
        return (result))
    <|>
    (do
        return (n1))

-- Expression that has precedence 3
exp3 :: ParsecT [Token] [(Token,Token)] IO(Token)
exp3 = 
    do
        n1 <- exp2
        result <- eval_remaining_exp3 n1 
        return (result)

-- Evaluates the remainder of a expression that has precedence 3
eval_remaining_exp3 :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining_exp3 n1 = 
    try
    (do
        op <- bin_num_op_left_3_token
        n2 <- exp2
        result <- eval_remaining_exp3 (binary_eval n1 op n2)
        return (result))
    <|>
    (do
        return (n1))

-- Expression that has precedence 2
exp2 :: ParsecT [Token] [(Token,Token)] IO(Token)
exp2 = 
    try
    (do
        n1 <- exp1
        op <- expo_token
        n2 <- exp2
        return (binary_eval n1 op n2))
    <|>
    exp1

-- Expression that has precedence 1
exp1 :: ParsecT [Token] [(Token,Token)] IO(Token)
exp1 = 
    try
    (do
        op <- minus_token
        n2 <- exp1
        return (unary_eval op n2))
    <|>
    exp0

-- Expression that has precedence 0
exp0 :: ParsecT [Token] [(Token,Token)] IO(Token)
exp0 = try var_attribution <|> exp_const <|> exp_parenthesized <|> exp_local_var




-- Assignment of a value to a variable
var_attribution :: ParsecT [Token] [(Token,Token)] IO(Token)
var_attribution = do
    a <- id_token
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

-- Expression that consists of a local variable
exp_local_var :: ParsecT [Token] [(Token,Token)] IO(Token)
exp_local_var = 
    do
        mem <- getState
        name <- id_token
        let value = symtable_get name mem
        return (value)

