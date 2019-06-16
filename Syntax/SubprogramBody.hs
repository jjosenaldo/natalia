module Syntax.SubprogramBody where

-- natalia's modules
import Lexical.Tokens
import Lexical.Lexemes
import Memory.Memory
import Syntax.Definition
import Syntax.Expressions
import Types.Types
import TypeValue.TypeValue
import Expressions.Parser

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

--_statementList :: [Statement] -> ParsecT [Token] [MemoryCell] IO (ReturnObject)
_statementList stmts = 
    try
    (do 
        stmt <- _statement
        retSemicolon <- semiColonToken
        --let actualStatement = getRetStatement retStatement -- Statement

        --retRemainingStatements <- _statementList (  stmts ++ [actualStatement]  )
        return ())
    <|>
    try
    (do
        retBlock <- _block
        retRemainingStatements <- _statementList (  stmts ++ [statementThatEnclosesTheBlock]  )
        return ())
        -- let actualBlock = getRetBlock retBlock -- Block
        -- let statementThatEnclosesTheBlock = CONSStatementBlock actualBlock -- Statement
    -- <|>
    -- try (_if stmts)
    -- <|>
    -- try (_while stmts)
    <|>
    (do
        --liftIO(print("I've got no more statements to read"))
        return (RetStatementList stmts))
    
--_statement :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
_statement = _print -- <|> try _varInit <|> try _varAssignment

-- _varInit :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
-- _varInit = 
--     do 
--         -- Reads the type
--         retType <- generalType
--         let actualType = getRetType retType -- Type

--         -- Reads the variable
--         retId <- idToken 
--         let idAsToken = getRetToken retId -- Token (with the constructor Id x p)
--         let id = CONSTokenId idAsToken -- Id

--         -- Reads the = sign
--         retAssign <- assignToken

--         -- Reads the value being assigned
--         retExpression <- _expr

--         let actualExpression = getRetExpression retExpression -- Expression

--         -- Constructs the VarInit object
--         let varInit = CONSVarInit actualType id actualExpression

--         return (RetStatement (CONSStatementVarInit varInit))

-- _varAssignment :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
-- _varAssignment = 
--     do 
--         -- Reads the variable
--         retId <- idToken 
--         let idAsToken = getRetToken retId -- Token (with the constructor Id x p)
--         let id = CONSTokenId idAsToken -- Id
--         let varType = getTypeOfLocalVar (get_id_name idAsToken) -- Type

--         -- Reads the = sign
--         retAssign <- assignToken

--         -- Reads the value being assigned
--         retExpression <- _expr
--         let actualExpression = getRetExpression retExpression -- Expression

--         return (RetStatement (CONSStatementVarAssign (CONSVarAssign id actualExpression)))

--_print :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
_print = 
    do
        retPrint <- printToken
        retLeftParen <- leftParenToken
        retExpr <- _expr
        retRightParen <- rightParenToken
        
        return ()

-- _if :: [Statement] -> ParsecT [Token] [MemoryCell] IO (ReturnObject)
-- _if stmts = 
--     try 
--     (do -- if-else
--         retIf <- ifToken
--         retLeftParen <- leftParenToken
--         retExp <- _expr
--         retRightParen <- rightParenToken
        
--         let ifExp = getRetExpression retExp

--         retBlock <- _block
--         let actualBlock = getRetBlock retBlock -- Block
        
--         retElse <- elseToken
--         retElseBlock <- _block
--         let actualElseBlock = getRetBlock retElseBlock
        
--         let statementThatEnclosesTheBlock = CONSStatementIfElse ifExp actualBlock actualElseBlock -- Statement
        
--         retRemainingStatements <- _statementList (  stmts ++ [statementThatEnclosesTheBlock]  )
--         return (retRemainingStatements))
--     <|>
--     (do
--         retIf <- ifToken
--         retLeftParen <- leftParenToken
--         retExp <- _expr
--         retRightParen <- rightParenToken
        
--         let ifExp = getRetExpression retExp

--         retBlock <- _block
--         let actualBlock = getRetBlock retBlock -- Block
--         let statementThatEnclosesTheBlock = CONSStatementIf ifExp actualBlock -- Statement
        
--         retRemainingStatements <- _statementList (  stmts ++ [statementThatEnclosesTheBlock]  )
--         return (retRemainingStatements))

-- _while :: [Statement] -> ParsecT [Token] [MemoryCell] IO (ReturnObject)
-- _while stmts = 
--     do
--         retWhileToken <- whileToken
--         retLParen <- leftParenToken
--         retExp <- _expr
--         retRParen <- rightParenToken

--         retBlock <- _block
--         let exp = getRetExpression retExp
--         let actualBlock = getRetBlock retBlock -- Block
--         let statementThatEnclosesTheBlock = CONSStatementWhile exp actualBlock -- Statement
--         retRemainingStatements <- _statementList (  stmts ++ [statementThatEnclosesTheBlock]  )
--         return (retRemainingStatements)