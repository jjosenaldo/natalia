module Syntax.Parser where

-- natalia's modules
import Syntax.Definition
import Lexical.Tokens
import Lexical.Lexemes
import Types.Types
import TypeValue.TypeValue

-- Haskell's modules
import Text.Parsec
import Control.Monad.IO.Class -- liftIO
-- import System.Environment
-- import System.IO.Unsafe

_expression :: Type -> ParsecT [Token] st IO (ReturnObject)
_expression expectedType = _expGroup1 expectedType -- TODO: change for expGroup9

-- GROUP 0 EXPRESSIONS --------------------------------------------------------------------------------------------------------

_expGroup0 expectedType = 
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
    _boolTokenExpression expectedType
    <|> 
    _intTokenExpression expectedType
    <|>
    _doubleTokenExpression expectedType
    <|>
    _nullTokenExpression expectedType
    <|>
    _stringTokenExpression expectedType
    -- <|>
    -- _varIdExpression expectedType
    -- <|>
    -- _expParenthesized expectedType

-- | A general parser for literals that type-checks things.
_generalLiteralTokenExpression :: Type -- ^ the expected type
                               -> ParsecT [Token] st IO (ReturnObject) -- ^ the parser for a literal 
                               -> Type -- ^ the type of the literal that will be parsed parsed 
                               -> ParsecT [Token] st IO (ReturnObject) -- ^ a parser with type-checking for the literal
_generalLiteralTokenExpression expectedType literalToken actualType = 
    do 
        retLiteral <- literalToken
        let literal = getRetValue retLiteral -- Value
        
        if not (checkCompatibleTypes expectedType actualType ) then 
            error ("ERROR at " ++ show(getPosValue literal) ++ ": You passed a " ++ (getNameOfType actualType) ++ " where a " ++ (getNameOfType expectedType) ++ " was expected.")

        else
            do 
                return (RetExpression ((CONSValue literal) actualType))
                
_boolTokenExpression expectedType = _generalLiteralTokenExpression expectedType boolToken NatBool
_intTokenExpression expectedType = _generalLiteralTokenExpression expectedType intToken NatInt
_doubleTokenExpression expectedType = _generalLiteralTokenExpression expectedType doubleToken NatDouble
_stringTokenExpression expectedType = _generalLiteralTokenExpression expectedType stringToken NatString
_nullTokenExpression expectedType = _generalLiteralTokenExpression expectedType nullToken NatNull


-- _varIdExpression expectedType =
--     do 
--         retId <- idToken
--         let id = CONSTokenId (getRetToken retId) -- Id
--         return (RetExpression (CONSId id))
    
-- _expParenthesized expectedType = 
--     do 
--         retLParen <- leftParenToken
--         retExpression <- _expression
--         retRParen <- rightParenToken

--         return (RetExpression (getRetExpression retExpression))

-- GROUP 1 EXPRESSIONS --------------------------------------------------------------------------------------------------------

-- This group contains only operations !, unary -, reference access (&) and value access (*)         
-- TODO: reference access, value access
_expGroup1 :: Type -> ParsecT [Token] st IO(ReturnObject)
_expGroup1 expectedType = 
    try
    (do 
        retExpr <- _group1OpExpression expectedType
        let expr = getRetExpression retExpr -- Expression
        return (RetExpression expr))
    <|>
    _expGroup0 expectedType

_group1OpExpression :: Type -> ParsecT [Token] st IO(ReturnObject)
_group1OpExpression expectedType = _negationExpression expectedType -- <|> _minusExpression expectedType

_negationExpression :: Type -> ParsecT [Token] st IO(ReturnObject)
_negationExpression expectedType = 
    do 
        retNeg <- _negationTokenOp
        let neg = getRetUnOperation retNeg -- UnOperation
        retExpr <- _expression expectedType
        let expr = getRetExpression retExpr -- Expression
        let exprType = getTypeOfExpression expr -- Type
        let actualType = getUnOperationReturnType neg exprType

        if not (checkCompatibleTypes expectedType actualType) then
            error ("ERROR at " ++ show(getSyntacticalUnitPos expr) ++ ": you're trying to pass a " ++ show(actualType) ++ " when a " ++ show(expectedType) ++ " is expected!")
        else
            do 
                return (RetExpression ((CONSUnOperation neg expr) actualType) )

_negationTokenOp = 
    do 
        retNeg <- negationToken
        let tok = getRetToken retNeg -- Token
        let op = CONSTokenUnOperation tok -- UnOperation
        return (RetUnOperation op)

-- _minusTokenOp = 
--     do 
--         retMinus <- minusToken
--         let tok = getRetToken retMinus -- Token
--         let op = CONSTokenUnOperation tok -- UnOperation
--         return (RetUnOperation op)

-- GROUP 2 EXPRESSIONS --------------------------------------------------------------------------------------------------------