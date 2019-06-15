module NewExpressions.Parsers where

-- natalia's modules
import Lexical.Lexemes
import NewExpressions.Grammar
import NewExpressions.Tokens

-- Haskell modules
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.String

table = 
    [   
        [Prefix negationToken],
        [Infix andToken AssocLeft],
        [Infix orToken AssocLeft],
        [Prefix minusUnToken],
        [Infix expoToken AssocLeft],
        [Infix timesToken AssocLeft, Infix divToken AssocLeft, Infix modToken AssocLeft],
        [Infix plusToken AssocLeft, Infix minusBinToken AssocLeft]
    ]

terms = 
    try (parens expr) <|> try intToken <|> try doubleToken <|> boolToken

expr = buildExpressionParser table terms

parens x = 
    do 
        l <- leftParenToken
        a <- x
        r <- rightParenToken
        return a


--exprParser = boolExprParser

parser :: [Token] -> IO (Either ParseError (Exp))
parser tokens = runParserT expr [] "Syntactical error:" tokens