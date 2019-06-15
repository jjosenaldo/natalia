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
    try (parens boolExprParser)
    <|>
    try boolToken 
    <|>
    try 
    (do 
        a <- numExprParser
        op <- lessThan
        b <- numExprParser
        return $ CONSBoolExpBinRel op a b)
    
numTerms = 
    try (parens numExprParser)
    <|>
    try intToken
    <|>
    doubleToken

exprParser = 
    try
    (do 
        a <- boolExprParser
        return $ CONSBoolExp a)
    <|>
    do 
        a <- numExprParser
        return $ CONSNumExp a

parens x = 
    do 
        l <- leftParenToken
        a <- x
        r <- rightParenToken
        return a

boolExprParser = buildExpressionParser tableBoolOperations boolTerms
numExprParser = buildExpressionParser tableNumOperations numTerms

--exprParser = boolExprParser

parser :: [Token] -> IO (Either ParseError (Exp))
parser tokens = runParserT exprParser [] "Syntactical error:" tokens