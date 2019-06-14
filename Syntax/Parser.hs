module Syntax.Parser where

-- natalia's modules
import Syntax.Definition
import Lexical.Tokens
import Lexical.Lexemes

-- Haskell's modules
import Text.Parsec
-- import Control.Monad.IO.Class
-- import System.Environment
-- import System.IO.Unsafe

_expression :: ParsecT [Token] st IO (ReturnObject)
_expression = _expGroup0 -- TODO: change for expGroup9

_expGroup0 = 
    -- try _structFieldRead  -- TODO: implement this 
    -- <|>
    -- try _structValue
    -- <|> 
    -- _memoryAccess
    -- <|>
    -- _setExpression
    -- <|>
    -- _expArray
    -- <|>
    _boolTokenExpression
    <|> 
    _intTokenExpression
    <|>
    _doubleTokenExpression
    <|>
    _nullTokenExpression
    <|>
    _stringTokenExpression
    <|>
    _varIdExpression
    <|>
    _expParenthesized

_boolTokenExpression = 
    do 
        retBool <- boolToken
        let bool = getRetValue retBool 

        return (RetExpression (CONSValue bool))

_intTokenExpression = 
    do 
        retInt <- intToken
        let int = getRetValue retInt 

        return (RetExpression (CONSValue int))

_doubleTokenExpression = 
    do 
        retDouble <- doubleToken
        let double = getRetValue retDouble 

        return (RetExpression (CONSValue double))

_nullTokenExpression = 
    do 
        retNull <- nullToken
        let null = getRetValue retNull

        return (RetExpression (CONSValue null))

_stringTokenExpression = 
    do 
        retString <- stringToken
        let string = getRetValue retString 

        return (RetExpression (CONSValue string))

_varIdExpression =
    do 
        retId <- idToken
        let id = CONSToken (getRetToken retId) -- Id
        return (RetExpression (CONSId id))
    
_expParenthesized = 
    do 
        retLParen <- leftParenToken
        retExpression <- _expression
        retRParen <- rightParenToken

        return (RetExpression (getRetExpression retExpression))