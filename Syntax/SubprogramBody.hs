module Syntax.SubprogramBody where

-- natalia's modules
import Lexical.Tokens
import Lexical.Lexemes
import Memory.Memory
import Syntax.Definition
import Syntax.Expressions
import Types.Types
import TypeValue.TypeValue

-- Haskell's modules
import Text.Parsec
import Control.Monad.IO.Class -- liftIO

subprogramBodyParser :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
subprogramBodyParser = 
    do 
        retFunctionBody <- functionBodyParser
        return (retFunctionBody) 

functionBodyParser :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
functionBodyParser = 
    do 
        retLeftBrace <- leftBraceToken
        retStatementList <- _statementList []
        retRightBrace <- rightBraceToken

        let statementList = getRetStatementList retStatementList -- [Statement]

        return (RetFunctionBody (CONSFunctionBody statementList))

_statementList :: [Statement] -> ParsecT [Token] [MemoryCell] IO (ReturnObject)
_statementList stmts = 
    try
    (do 
        retStatement <- _statement
        let actualStatement = getRetStatement retStatement -- Statement
        retSemicolon <- semiColonToken

        retRemainingStatements <- _statementList (  stmts ++ [actualStatement]  )
        return (retRemainingStatements))
    <|>
    (do
        return (RetStatementList stmts))
    
_statement :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
_statement = try _varInit <|> _varAssignment

_varInit :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
_varInit = 
    do 
        -- Reads the type
        retType <- generalType
        let actualType = getRetType retType -- Type

        -- Reads the variable
        retId <- idToken 
        let idAsToken = getRetToken retId -- Token (with the constructor Id x p)
        let id = CONSTokenId idAsToken -- Id

        -- Reads the = sign
        retAssign <- assignToken

        -- Reads the value being assigned
        retExpression <- _expression actualType
        let actualExpression = getRetExpression retExpression -- Expression

        -- Constructs the VarInit object
        let varInit = CONSVarInit actualType id actualExpression

        return (RetStatement (CONSStatementVarInit varInit))

_varAssignment :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
_varAssignment = 
    do 
        -- Reads the variable
        retId <- idToken 
        let idAsToken = getRetToken retId -- Token (with the constructor Id x p)
        let id = CONSTokenId idAsToken -- Id
        let varType = getTypeOfLocalVar (get_id_name idAsToken) -- Type

        -- Reads the = sign
        retAssign <- assignToken

        -- Reads the value being assigned
        retExpression <- _expression varType
        let actualExpression = getRetExpression retExpression -- Expression

        return (RetStatement (CONSStatementVarAssign (CONSVarAssign id actualExpression)))