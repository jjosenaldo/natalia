module Statements.Parser where 

-- natalia's modules
import Data.Maybe
import Expressions.Grammar
import Expressions.Parser
import GenParsers.GenParser
import Lexical.Lexemes
import Lexical.Tokens
import Statements.Grammar
import Types.Types

-- Haskell modules
import Control.Monad.IO.Class
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.String


_statementList = _remainingStatements []

_remainingStatements stmts = 
    try 
    (do
        stmt <- _statementNotBlock
        sep <- _semicolonToken
        let currStmtList = stmts ++ [stmt]
        ret <- _remainingStatements  currStmtList
        return $ ret 
    )
    <|>
    try 
    (do 
        stmt <- _statementWithoutSemicolon
        let currStmtList = stmts ++ [stmt]
        ret <- _remainingStatements  currStmtList
        return $  ret
    )
    <|>
    return stmts

_statementWithoutSemicolon = try _ifElseAsStmt <|>  try _ifAsStmt <|> try _whileAsStmt <|> _blockAsStmt 
_statementNotBlock =  try _varInitAsStmt <|> try _assignmentAsStmt <|> try _printAsStmt <|>  try _returnAsStmt <|> _procCallAsStmt


-- PRINT -----------------------------------------------------------------------------

_printAsStmt = 
    do 
        prt <- _print 
        return $ CONSStatementPrint prt 

_print = 
    do 
        print <- _printToken -- Token
        expr <- _parens _expr -- Exp
        return $ CONSPrint expr

-- VAR INTIIALIZATION -----------------------------------------------------------------------------

_varInitAsStmt = 
    do 
        init <- _varInit -- Statement
        return $ CONSStatementVarInit init

_varInit = 
    do 
        ttype  <- generalType -- Token
        let actualType = getRetType ttype
        id <- _idToken
        ass <- _assignToken -- Token 
        expr <- _expr -- Exp
        return $ CONSVarInit actualType (get_id_name id) expr

-- LVALUE ASSIGNMENT -----------------------------------------------------------------------------

_assignmentAsStmt = 
    do 
        ass <- _assignment
        return $ CONSStatementAssignment ass

_assignment = 
    do 
        lv <- lvalue  
        --liftIO(print(show(lv)))
        ass <- _assignToken
        expr2 <- _expr 
        return $ CONSAssignment lv expr2


-- RETURN-------------- -----------------------------------------------------------------------------

_returnAsStmt = 
    do 
        ret <- _return
        return $ CONSStatementReturn ret

_return =
    do 
        returnToken <- _returnToken
        expr <- _expr;
        return (CONSReturn expr) 

-- BLOCK ----------------------------------------------------------------------------

_blockAsStmt = 
    do 
        blk <- _block
        return $ CONSStatementBlock blk

_block = 
    do 
        stmts <- _braces _statementList
        return $ CONSBlock stmts

-- IF ------------------------------------------------------------------------

_ifAsStmt = 
    do
        if_ <- _if
        return $ CONSStatementIf if_

_if =
    do
        ifcommand <- ifToken
        expr <- _parens _expr
        block <- _block
        return  (CONSIf (expr) (block)) 

_ifElseAsStmt = 
    do 
        ifElse <- _ifElse
        return $ CONSStatementIfElse ifElse


_ifElse = 
    do 
        if_ <- ifToken
        expr <- _parens _expr
        block1_ <- _block
        else_ <- elseToken
        block2_ <- _block
        return (CONSIfElse (expr) (block1_) (block2_) )

-- WHILE ------------------------------------------------------------

_whileAsStmt = 
    do 
        while_ <- _while
        return $ CONSStatementWhile while_

_while =
    do 
        while_ <- whileToken
        expr <- _parens _expr
        block <- _block
        return (CONSWhile expr block)

-- PROC CALL ------------------------------------------------------------

_procCallAsStmt = 
    do 
        proc <- _procCall
        return $ CONSStatementProcCall proc 
_procCall = 
    do 
        id <- _idToken
        exps <- _parens $ sepBy _expr _commaToken
        return $ CONSProcCall (get_id_name id) exps
