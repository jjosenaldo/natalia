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
        [Prefix uppersandToken],
        [Infix interrogationToken AssocLeft],
        [Prefix minusUnToken],
        [Infix expoToken AssocLeft],
        [Infix timesTokenAsNumOp AssocLeft, Infix divToken AssocLeft, Infix modToken AssocLeft],
        [Infix plusToken AssocLeft, Infix minusBinToken AssocLeft]
    ]

-- Terms of a general expression (i.e., the expressions with the greatest precedence)
terms = 
    try funcCall <|> try (parens expr) <|> try structValue <|> try setValue <|> try assign <|> try lvalueAsExpr <|> try intToken <|> try doubleToken <|> boolToken <|> stringToken
    
funcCall = 
    do 
        id <- idToken
        lp <- leftParenToken
        exprs <- sepBy expr commaToken
        rp <- rightParenToken
        return $ CONSExpFuncCall NatNothing (get_id_name id) exprs 

setValue = 
    do 
        lb <- leftBraceToken
        exprs <- sepBy expr commaToken
        rb <- rightBraceToken
        return $ CONSExpSet NatNothing exprs


-- Value of a struct, like: rational_t{1, 0}
structValue = 
    do 
        id <- idToken 
        lb <- leftBraceToken
        exprs <- sepBy expr commaToken
        rb <- rightBraceToken
        return $ CONSExpStruct NatNothing (get_id_name id) exprs

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
        return $ CONSExpLValue NatNothing lv

-- Thing that can be at the left of the = in an assignment
lvalue = try lvalueArrAccess <|> try lvalueStructAccess <|> try lvalueLocalVar <|> lvalueDerref

-- v[a][b][c] = ...
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

-- node.left = ...
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

-- a = ...
lvalueLocalVar = 
    do 
        id <- idToken
        return $ CONSLValueId (get_id_name id)

-- ****p = ...
lvalueDerref = 
    do 
        t <- timesToken
        ts <- many (timesToken)
        id <- idToken

        return $ CONSLValueDerref (get_id_name id) (length (t:ts))

-- General expression
expr = buildExpressionParser table terms

parens x = 
    do 
        l <- leftParenToken
        a <- x
        r <- rightParenToken
        return a

parser :: [Token] -> IO (Either ParseError (Exp))
parser tokens = runParserT expr [] "Syntactical error:" tokens