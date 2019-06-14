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
    <|>
    _localVarExpression expectedType
    <|>
    _expParenthesized expectedType

-- | Throws a type error.
throwTypeError :: (Int, Int) -- ^ the position in which the error occurs
               -> Type -- ^ the expected type
               -> Type -- ^ the actual type
               -> ParsecT [Token] st IO (ReturnObject) -- ^ the error thrown
throwTypeError pos expectedType actualType =  
    do 
        error ("ERROR at " ++ show(pos) ++ ": You passed a " ++ (getNameOfType actualType) ++ " where a " ++ (getNameOfType expectedType) ++ " was expected.")

-- | A general parser for literals that type-checks things.
_generalLiteralTokenExpression :: Type -- ^ the expected type
                               -> ParsecT [Token] st IO (ReturnObject) -- ^ the parser for a literal 
                               -> Type -- ^ the type of the literal that will be parsed parsed 
                               -> ParsecT [Token] st IO (ReturnObject) -- ^ a parser with type-checking for the literal
_generalLiteralTokenExpression expectedType literalToken actualType = 
    do 
        retLiteral <- literalToken
        let literal = getRetValue retLiteral -- Value
        
        if not (checkCompatibleTypes expectedType actualType ) then do 
            err <- throwTypeError (getPosValue literal) expectedType actualType
            return (RetNothing)
        else
            do 
                return (RetExpression ((CONSValue literal) actualType))
                
_boolTokenExpression expectedType = _generalLiteralTokenExpression expectedType boolToken NatBool
_intTokenExpression expectedType = _generalLiteralTokenExpression expectedType intToken NatInt
_doubleTokenExpression expectedType = _generalLiteralTokenExpression expectedType doubleToken NatDouble
_stringTokenExpression expectedType = _generalLiteralTokenExpression expectedType stringToken NatString
_nullTokenExpression expectedType = _generalLiteralTokenExpression expectedType nullToken NatNull

_localVarExpression :: Type -> ParsecT [Token] st IO (ReturnObject)
_localVarExpression expectedType = 
    do 
        retId <- idToken
        let idAsToken = getRetToken retId -- Token, with constructor Id x p
        let idName = get_id_name idAsToken -- String
        let actualType = getTypeOfLocalVar idName -- Type 
        let pos = get_pos idAsToken

        if not (checkCompatibleTypes expectedType actualType ) then do 
            err <- throwTypeError pos expectedType actualType
            return (RetNothing)
        else
            do 
                return (RetExpression (CONSId (CONSTokenId idAsToken) actualType))
    
_expParenthesized :: Type -> ParsecT [Token] st IO (ReturnObject)
_expParenthesized expectedType = 
    do 
        retLParen <- leftParenToken
        retExpression <- _expression expectedType
        retRParen <- rightParenToken

        let exp = getRetExpression retExpression -- Expression
        let actualType = getTypeOfExpression exp -- Type
        let lparenToken = getRetToken retLParen -- Token
        let pos = get_pos lparenToken -- (Int, Int)
        

        if not (checkCompatibleTypes expectedType actualType ) then do 
            err <- throwTypeError pos expectedType actualType
            return (RetNothing)
        else
            do 
                return (RetExpression exp)

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