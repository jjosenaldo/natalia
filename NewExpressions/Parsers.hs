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

table = [   [Infix andToken AssocLeft]
            ,[Infix orToken AssocLeft]

        ]

expr = buildExpressionParser table boolToken

parser :: [Token] -> IO (Either ParseError (BoolExp))
parser tokens = runParserT expr [] "Syntactical error:" tokens