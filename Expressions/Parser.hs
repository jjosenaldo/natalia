module Expressions.Parser where

-- natalia's modules
import Lexical.Lexemes
import Lexical.Tokens
import Expressions.Grammar
import TypeValue.TypeValue

-- Haskell modules
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.String

table = 
    [   
        [Prefix _negationToken],
        [Infix _andToken AssocLeft],
        [Infix _orToken AssocLeft],
        [Infix _lessEqualsToken AssocLeft, Infix _greaterEqualsToken AssocLeft, Infix _lessThanToken AssocLeft, Infix _greaterThanToken AssocLeft, Infix _equalsToken AssocLeft, Infix _differentToken AssocLeft], 
        [Prefix _uppersandToken],
        [Infix _interrogationToken AssocLeft],
        [Prefix _minusUnToken],
        [Infix _expoToken AssocLeft],
        [Infix _timesTokenAsNumOp AssocLeft, Infix _divToken AssocLeft, Infix _modToken AssocLeft],
        [Infix _plusToken AssocLeft, Infix _minusBinToken AssocLeft]
    ]

-- Terms of a general expression (i.e., the expressions with the greatest precedence)
terms = 
    try zeroaryCmd <|>
    try unaryCmd <|>
    try _nullToken <|> 
    try funcCall <|> 
    try (parens _expr) <|> 
    try structValue <|> 
    try setValue <|> 
    try assign <|> 
    try lvalueAsExpr <|> 
    try _intToken <|> 
    try _doubleToken <|> 
    try _boolToken <|> 
    _stringToken

zeroaryCmd = 
    do 
        rd <- _readToken
        lp <- _leftParenToken
        rp <- _rightParenToken
        return $ CONSExpCmdZero NatNothing rd

unaryCmd = 
    do 
        rd <- try _toStringToken <|> try _toIntToken <|> try _toDoubleToken <|> _toBoolToken
        lp <- _leftParenToken
        arg <- _expr
        rp <- _rightParenToken
        return $ CONSExpCmdUn NatNothing rd arg

-- Function call, like f(x,y)
funcCall = 
    do 
        id <- _idToken
        lp <- _leftParenToken
        exprs <- sepBy _expr _commaToken
        rp <- _rightParenToken
        return $ CONSExpFuncCall NatNothing (get_id_name id) exprs 

-- A set, like {1,2,3,4}
setValue = 
    do 
        lb <- _leftBraceToken
        exprs <- sepBy _expr _commaToken
        rb <- _rightBraceToken
        return $ CONSExpSet NatNothing exprs

-- Value of a struct, like: rational_t{1, 0}
structValue = 
    do 
        id <- _idToken 
        lb <- _leftBraceToken
        exprs <- sepBy _expr _commaToken
        rb <- _rightBraceToken
        return $ CONSExpStruct NatNothing (get_id_name id) exprs

-- Assignment of a value to a variable
assign = 
    do 
        lval <- lvalue
        ass <- _assignToken
        e <- _expr
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
        id <- _idToken
        firstExp <- arrIndex
        exps <- many ( arrIndex )
        return $ CONSLValueArray (get_id_name id) (firstExp : exps)

-- [EXP] for some expression EXP. This is used in the "lvalueArrAccess" function.
arrIndex = 
    do 
        l <- _leftBracketToken
        e <- _expr
        r <- _rightBracketToken
        return e

-- node.left = ...
lvalueStructAccess = 
    do 
        id <- _idToken
        firstField <- structAccess
        remainingFields <- many (structAccess)

        return $ CONSLValueStruct (get_id_name id) (firstField : remainingFields)

-- .FIELD for some field FIELD of a struct. This is used in the "lvalueStructAccess" function
structAccess = 
    do 
        dot <- _dotToken
        field <- _idToken
        return (get_id_name field)

-- a = ...
lvalueLocalVar = 
    do 
        id <- _idToken
        return $ CONSLValueId (get_id_name id)

-- ****p = ...
lvalueDerref = 
    do 
        t <- _timesToken
        ts <- many (_timesToken)
        id <- _idToken

        return $ CONSLValueDerref (get_id_name id) (length (t:ts))

-- General expression
_expr = buildExpressionParser table terms

parens x = 
    do 
        l <- _leftParenToken
        a <- x
        r <- _rightParenToken
        return a