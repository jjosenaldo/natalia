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
    try (parens expr) <|> try assign <|> try lvalueAsExpr <|> try intToken <|> try doubleToken <|> boolToken  
    
-- Assignment of a value to a variable
assign = 
    do 
        lval <- lvalue
        ass <- assignToken
        e <- expr
        return $ CONSExpAssign NatNothing lval e

-- LValue as an expression (every LValue is a RValue!)
lvalueAsExpr = 
    do 
        lv <- lvalue 
        return $ CONSExpLValue lv

-- Thing that can be at the left of the = in an assignment
lvalue = try lvalueArrAccess <|> try lvalueStructAccess <|> lvalueLocalVar

-- v[a][b][c]
lvalueArrAccess = 
    do 
        id <- idToken
        firstExp <- arrIndex
        exps <- many ( arrIndex )
        return $ CONSLValueArray (get_id_name id) (firstExp : exps)

-- [EXP] for some expression EXP. This is used in the "lvalueArrAccess" function.
arrIndex = 
    do 
        l <- leftBracketToken
        e <- expr
        r <- rightBracketToken
        return e

-- node.left = 1
lvalueStructAccess = 
    do 
        id <- idToken
        firstField <- structAccess
        remainingFields <- many (structAccess)

        return $ CONSLValueStruct (firstField : remainingFields)

-- .FIELD for some field FIELD of a struct. This is used in the "lvalueStructAccess" function
structAccess = 
    do 
        dot <- dotToken
        field <- idToken
        return (get_id_name field)

-- Local variable
lvalueLocalVar = 
    do 
        id <- idToken
        return $ CONSLValueId (get_id_name id)

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