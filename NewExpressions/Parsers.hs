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

tableBoolOperations = 
    [   
        [Prefix negationToken],
        [Infix andToken AssocLeft],
        [Infix orToken AssocLeft]
    ]

tableNumOperations = 
    [   
        [Prefix minusUnToken],
        [Infix expoToken AssocLeft],
        [Infix timesToken AssocLeft, Infix divToken AssocLeft, Infix modToken AssocLeft],
        [Infix plusToken AssocLeft, Infix minusBinToken AssocLeft]
    ]

boolTerms = 
    boolToken 
    <|>
    do 
        a <- numExprParser
        op <- lessThan
        b <- numExprParser
        return $ CONSBoolExpBinRel op a b

exprParser = 
    do 
        a <- boolExprParser
        return $ CONSBoolExp a
    <|>
    do 
        a <- numExprParser
        return $ CONSNumExp a

boolExprParser = buildExpressionParser tableBoolOperations boolTerms
numExprParser = buildExpressionParser tableNumOperations (intToken <|> doubleToken)

--exprParser = boolExprParser

parser :: [Token] -> IO (Either ParseError (Exp))
parser tokens = runParserT exprParser [] "Syntactical error:" tokens