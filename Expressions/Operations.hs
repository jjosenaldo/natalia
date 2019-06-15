module Expressions.Operations where

import Lexical.Tokens

import Text.Parsec

-- Binary operators that are left-associative and have precedence 3
binOpLeft3Token = timesToken <|> divToken <|> modToken

-- Binary operators that are left-associative and have precedence 4
binOpLeft4Token = plusToken <|> minusToken

group1OpToken = negationToken <|> minusToken
group2OpToken = timesToken <|> divToken <|> modToken
group3OpToken = plusToken <|> minusToken
group4OpToken = expoToken 
group5OpToken = lessThanToken    <|> 
                greaterThanToken <|> 
                lessEqualsToken  <|>
                greaterEqualsToken <|>
                equalsToken <|>
                differentToken <|>
                inToken 
group6OpToken = equalsToken
group7OpToken = andToken
group8OpToken = orToken
group9OpToken = assignToken
