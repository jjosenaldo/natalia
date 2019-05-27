module Expressions.Expressions where

import Expressions.Evaluation
import Lexical.Lexemes
import Lexical.Tokens

import Text.Parsec
import Control.Monad.IO.Class

-- <expr> ::= <expr3>
-- <expr3> ::= <expr3> + <expr2> | 
--             <expr3> - <expr2> | 
--             <expr2> 
-- <expr2> ::= <expr2> * <expr1> | 
--             <expr2> / <expr1> | <expr1>
-- <expr1> ::= <expr1> % <expr0> | <expr0>

expression :: ParsecT [Token] [(Token,Token)] IO(Token)
expression = expression3

expression3 :: ParsecT [Token] [(Token,Token)] IO(Token)
expression3 = 
    try
    (do
        a <- expression3
        b <- plus_token
        c <- expression2
        return (eval a b c))
     <|>
     (do
        a <- expression3
        b <- minus_token
        c <- expression2
        return (eval a b c))
     <|>
     (do
        a <- expression2
        return (a))

expression2 :: ParsecT [Token] [(Token,Token)] IO(Token)
expression2 = 
    try
    (do
        c <- expression1
        b <- times_token
        a <- expression2
        return (eval c b a))
    <|>
    (do
        c <- expression1
        b <- div_token
        a <- expression2
        return (eval c b a))
    <|>
    (do
        a <- expression1
        return (a))

expression1 :: ParsecT [Token] [(Token,Token)] IO(Token)
expression1 = 
    try
    (do
        c <- expression0
        b <- mod_token
        a <- expression1
        return (eval c b a))
    <|>
    (do
        a <- expression0
        return (a))
    
expression0 :: ParsecT [Token] [(Token,Token)] IO(Token)
expression0 = 
    try
    (do
        a <- int_token
        return (a))