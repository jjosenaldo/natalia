module Expressions.Operations where

import Lexical.Tokens

import Text.Parsec

-- Binary operators that are left-associative and have precedence 3
bin_num_op_left_3_token = times_token <|> div_token <|> mod_token

-- Binary operators that are left-associative and have precedence 4
bin_num_op_left_4_token = plus_token <|> minus_token