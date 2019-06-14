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
        retBlock <- _block
        return (retBlock) 

_block :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
_block = 
    do 
        retLeftBrace <- leftBraceToken
        retStatementList <- _statementList []
        retRightBrace <- rightBraceToken

        let statementList = getRetStatementList retStatementList -- [Statement]


        return (RetBlock (CONSBlock statementList))

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
    try
    (do
        retBlock <- _block
        let actualBlock = getRetBlock retBlock -- Block
        let statementThatEnclosesTheBlock = CONSStatementBlock actualBlock -- Statement
        retRemainingStatements <- _statementList (  stmts ++ [statementThatEnclosesTheBlock]  )
        return (retRemainingStatements))
    <|>
    (do
        --liftIO(print("I've got no more statements to read"))
        return (RetStatementList stmts))
    
_statement :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
_statement = try _varInit <|> try _varAssignment <|> _print

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

_print :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
_print = 
    do
        retPrint <- printToken
        retLeftParen <- leftParenToken
        retExpr <- _expression NatGenType
        retRightParen <- rightParenToken

        let actualExpr = getRetExpression retExpr -- Expression
        
        return (RetStatement (CONSStatementPrint (CONSPrint actualExpr)))

-- _if :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
-- _if = 
--     do 
--         retIf <- ifToken
--         retLeftParen <- leftParenToken
--         retExp <- _expression NatBool
--         retRightParen <- rightParenToken
        



--         retElseStatements <- _statementList