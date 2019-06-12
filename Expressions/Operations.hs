module Expressions.Operations where

import Lexical.Tokens

import Text.Parsec

-- Binary operators that are left-associative and have precedence 3
bin_op_left_3_token = times_token <|> div_token <|> mod_token

-- Binary operators that are left-associative and have precedence 4
bin_op_left_4_token = plus_token <|> minus_token

group1OpToken = negationToken <|> minus_token
group2OpToken = times_token <|> div_token <|> mod_token
group3OpToken = plus_token <|> minus_token
group4OpToken = expo_token 
group5OpToken = less_than_token    <|> 
                greater_than_token <|> 
                less_equals_token  <|>
                greater_equals_token <|>
                equalsToken <|>
                differenceToken <|>
                subsetToken
group6OpToken = equalsToken
group7OpToken = andToken
group8OpToken = orToken
group9OpToken = assignToken