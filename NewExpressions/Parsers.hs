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
        [Infix lessEqualsToken AssocLeft, Infix greaterEqualsToken AssocLeft, Infix lessThanToken AssocLeft, Infix greaterThanToken AssocLeft, Infix equalsToken AssocLeft, Infix differentToken AssocLeft], 
        [Prefix minusUnToken],
        [Infix expoToken AssocLeft],
        [Infix timesToken AssocLeft, Infix divToken AssocLeft, Infix modToken AssocLeft],
        [Infix plusToken AssocLeft, Infix minusBinToken AssocLeft]
    ]

-- Terms of a general expression (i.e., the expressions with the greatest precedence)
terms = 
    try (parens expr) <|> try assign <|> try intToken <|> try doubleToken <|> try boolToken <|> try idTokenAsExp 
    
-- Assignment of a value to a variable
assign = 
    do 
        lval <- lvalue
        ass <- assignToken
        e <- expr
        return $ CONSExpAssign NatNothing lval e

-- Thing that can be at the left of the = in an assignment
lvalue = 
    do 
        id <- idToken
        exps <- many ( arrIndex )
        return $ CONSLValueArray (get_id_name id) exps

-- [EXP] for some expression EXP
arrIndex = 
    do 
        l <- leftBracketToken
        e <- expr
        r <- rightBracketToken
        return e

-- General expression
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