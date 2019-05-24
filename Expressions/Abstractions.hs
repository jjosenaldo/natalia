module Expressions.Abstractions where

import Expressions.Evaluation
import Lexical.Lexemes
import Lexical.Tokens

import Text.Parsec

expression :: ParsecT [Token] [(Token,Token)] IO(Token)
expression = try bin_expression <|> zeroary_expression

zeroary_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
zeroary_expression = do
                   a <- intToken 
                   return (a)

bin_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
bin_expression = do
                   a <- intToken
                   b <- plusToken
                   c <- intToken
                   return (eval a b c)