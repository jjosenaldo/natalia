module Expressions.Operations where

import Lexical.Tokens

import Text.Parsec

-- Binary operators that are left-associative and have precedence 3
bin_op_left_3_token = times_token <|> div_token <|> mod_token

-- Binary operators that are left-associative and have precedence 4
bin_op_left_4_token = plusToken <|> minusToken

group1OpToken = negationToken <|> minusToken
group2OpToken = times_token <|> div_token <|> mod_token
group3OpToken = plusToken <|> minusToken
group4OpToken = expoToken 
group5OpToken = lessThanToken    <|> 
                greater_than_token <|> 
                lessEqualsToken  <|>
                greater_equals_token <|>
                equalsToken <|>
                differentToken <|>
                inToken 
group6OpToken = equalsToken
group7OpToken = andToken
group8OpToken = orToken
group9OpToken = assignToken