module Syntax.Parser where

-- natalia's modules
import Syntax.Definition
import Lexical.Tokens

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
    -- <|> -- TODO: implement this
    -- _intTokenExpression
    -- <|>
    -- _doubleTokenExpression
    -- <|>
    -- _nullTokenExpression
    -- <|>
    -- _stringTokenExpression
    -- <|>
    -- _varIdExpression
    -- <|>
    -- _expParenthesized

_boolTokenExpression = 
    do 
        retBool <- boolToken
        let bool = getRetValue retBool -- Bool

        return (RetExpression (CONSValue bool))