module Syntax.Expressions where

-- natalia's modules
import Syntax.Definition
import Lexical.Tokens
import Lexical.Lexemes
import Types.Types
import TypeValue.TypeValue

-- Haskell's modules
import Text.Parsec
import Control.Monad.IO.Class -- liftIO

expressionParser :: ParsecT [Token] st IO (ReturnObject)
expressionParser = 
    do
        retExpression <- _expression NatInt
        let actualExpression = getRetExpression retExpression
        eof
        return (RetExpression actualExpression)


_expression :: Type -> ParsecT [Token] st IO (ReturnObject)
_expression = _expGroup9 -- TODO: eventually this should be changed to expGroup9

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
                
_boolTokenExpression = _generalLiteralTokenExpression NatBool boolToken 
_intTokenExpression = _generalLiteralTokenExpression NatInt intToken 
_doubleTokenExpression = _generalLiteralTokenExpression NatDouble doubleToken 
_stringTokenExpression = _generalLiteralTokenExpression NatString stringToken  
_nullTokenExpression = _generalLiteralTokenExpression NatNull nullToken 

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

-- This group contains only the operations !, unary -, reference access (&) and value access (*)         
-- TODO: operators of reference access (&), value access (*)
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
_group1OpExpression expectedType = _negationExpression expectedType <|> _unMinusExpression expectedType

_negationExpression = _generalUnExpression _negationTokenOp
_unMinusExpression = _generalUnExpression _unMinusTokenOp

_negationTokenOp = _generalUnOperatorParser negationToken 
_unMinusTokenOp = _generalUnOperatorParser minusToken 


-- GROUP 2 EXPRESSIONS --------------------------------------------------------------------------------------------------------

-- This group contains only the binary operations *, / and %.
_expGroup2 :: Type -> ParsecT [Token] st IO(ReturnObject)
_expGroup2 expectedType =
    do   
        retLeftExpr <- _expGroup1 expectedType
        let leftExpr = getRetExpression retLeftExpr -- Expression

        retExprResult <- _evalRemainingGroup2 leftExpr
        let exprResult = getRetExpression retExprResult -- Expression (left)

        return (RetExpression exprResult)

_evalRemainingGroup2 :: Expression -> ParsecT [Token] st IO(ReturnObject)
_evalRemainingGroup2 leftExpr =
    try
    (do
        -- Reads a binary operation
        retOp <- _group2OpToken -- RetToken
        let op = getRetBinOperator retOp -- BinOperation

        -- Reads the second operation if it is of the correct type
        let expectedType = getBinOperatorExpectedSecondType op (getTypeOfExpression leftExpr)
        retExprRight <- _expGroup1 expectedType

        let exprRight = getRetExpression retExprRight -- Expression (right)

        let returnType = getBinOperatorReturnType op (getTypeOfExpression leftExpr) (getTypeOfExpression exprRight)  -- Type

        let currentResult = CONSBinOperation op leftExpr exprRight returnType -- Expression

        retFinalResult <- _evalRemainingGroup2 currentResult 
        let finalResult = getRetExpression retFinalResult -- Expression

        return (RetExpression finalResult))
    <|>
    (do
        return (RetExpression leftExpr))


_group2OpToken = _timesTokenOp <|> _divTokenOp <|> _modTokenOp

_timesTokenOp = _generalBinOperatorParser timesToken 
_divTokenOp = _generalBinOperatorParser divToken 
_modTokenOp = _generalBinOperatorParser modToken 

-- GROUP 9 EXPRESSIONS --------------------------------------------------------------------------------------------------------

_expGroup9 :: Type -> ParsecT [Token] st IO(ReturnObject)
_expGroup9 expectedType = _expLocalVarAssignment expectedType <|> _expGroup2 expectedType -- TODO: this should contain _expGroup8 

_expLocalVarAssignment expectedType = 
    do 
        retId <- idToken
        
        let idName = get_id_name (getRetToken retId) -- String (variable name)
        let localVariableType = getTypeOfLocalVar idName -- Type 
        let pos = get_pos (getRetToken retId)

        -- Checks the type of the local variable being assigned
        if not (checkCompatibleTypes expectedType localVariableType ) then do 
            err <- throwTypeError pos expectedType localVariableType
            return (RetNothing)
        else
            do 
                retAssign <- assignToken

                -- Gets the expression -- which is already of a correct type -- whose value will be assigned to the variable
                retExpr <- _expression localVariableType
                let expr = getRetExpression retExpr -- Expression
                
                {- TODO: the assignment should update the subprogram's internal memory! -} 
                {- Or... should it? This is just a syntactical description, after all -}

                return (RetExpression expr)
                

-- return (RetExpression (CONSId (CONSTokenId idAsToken) actualType))

-- PARSER BUILDERS ------------------------------------------------------------------------------------------------------------

-- | Builds a parser for literals that type-checks things.
_generalLiteralTokenExpression :: Type -- ^ the type of the literal that will be parsed
                               -> ParsecT [Token] st IO (ReturnObject) -- ^ the parser for a literal 
                               -> Type -- ^  the expected type
                               -> ParsecT [Token] st IO (ReturnObject) -- ^ a parser with type-checking for the literal
_generalLiteralTokenExpression  actualType literalToken  expectedType = 
    do 
        -- Parses a literal
        retLiteral <- literalToken
        let literal = getRetValue retLiteral -- Value
        
        -- Checks if the type of the literal read matches with the type of the expected 
        if not (checkCompatibleTypes expectedType actualType ) then do 
            err <- throwTypeError (getPosValue literal) expectedType actualType
            return (RetNothing)
        else
            do 
                return (RetExpression ((CONSValue literal) actualType))

-- | Builds a parser for an unary expression.
_generalUnExpression :: ParsecT [Token] st IO (ReturnObject) -- ^ a parser for an unary operation
                     -> Type -- ^ the expected type
                     -> ParsecT [Token] st IO (ReturnObject) -- ^ the parser built
_generalUnExpression generalUnOperator expectedType = 
    do 
        -- Parses a unary operator
        retOp <- generalUnOperator
        let op = getRetUnOperator retOp -- UnOperator
        let typeExpectedByTheOperator = getUnOperatorExpectedType op -- Type

        -- Parses an expression of the type expected by the operator
        retExpr <- _expression typeExpectedByTheOperator
        let expr = getRetExpression retExpr -- Expression
        let exprType = getTypeOfExpression expr -- Type

        -- Type of the expression read
        let actualType = getUnOperatorReturnType op exprType

        -- This is not necessary: the expression already is of the correct type
        {-if not (checkCompatibleTypes expectedType actualType) then do
            err <- throwTypeError (getSyntacticalUnitPos expr) expectedType actualType
            return (RetNothing)
        else
            do 
                return (RetExpression ((CONSUnOperation op expr) actualType) )-}
        return (RetExpression ((CONSUnOperation op expr) actualType) )

-- | Builds a parser for an unary operator.
_generalUnOperatorParser :: ParsecT [Token] st IO (ReturnObject) -- ^ a parser that parses the token for an unary operation
                          -> ParsecT [Token] st IO (ReturnObject) -- ^ the parser built

_generalUnOperatorParser unOperatorToken = 
    do 
        retOp <- unOperatorToken
        let tok = getRetToken retOp -- Token
        let op = CONSTokenUnOperator tok -- UnOperation
        return (RetUnOperator op)

-- | Builds a parser for a binary operator.
_generalBinOperatorParser :: ParsecT [Token] st IO (ReturnObject) -- ^ a parser that parses the token for a binary operator 
                          -> ParsecT [Token] st IO (ReturnObject) -- ^ the parser built

_generalBinOperatorParser binOperatorToken = 
    do 
        retOp <- binOperatorToken
        let tok = getRetToken retOp -- Token
        let op = CONSTokenBinOperator tok -- UnOperation
        return (RetBinOperator op)