module Expressions.Operations where

import Lexical.Tokens

import Text.Parsec

bin_op_left_3_token = times_token <|> div_token <|> mod_token

bin_op_left_4_token = plus_token <|> minus_token