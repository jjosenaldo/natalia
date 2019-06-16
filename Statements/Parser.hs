module Statements.Parser where 

-- natalia's modules
import Expressions.Parser
import GenParsers.GenParser
import Lexical.Lexemes
import Lexical.Tokens
import Statements.Grammar
import Types.Types

-- Haskell modules
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
        blk <- _block
        let currStmtList = stmts ++ (getBlockStatements blk)
        ret <- _remainingStatements  currStmtList
        return $  ret
    )
    <|>
    return stmts


_statementNotBlock = _varInitAsStmt <|> _printAsStmt


_printAsStmt = 
    do 
        prt <- _print 
        return $ CONSStatementPrint prt 

_print = 
    do 
        print <- _printToken -- Token
        expr <- _parens _expr -- Exp
        return $ CONSPrint expr


_varInitAsStmt = 
    do 
        init <- _varInit -- Statement
        return $ CONSStatementVarInit init

_varInit = 
    do 
        ttype  <- generalType -- Token
        id <- _idToken
        ass <- _assignToken -- Token 
        expr <- _expr -- Exp
        return $ CONSVarInit (getTypeFromTypeToken ttype) (get_id_name id) expr

_return =
    do 
        returnToken <- _returnToken
        expr <- _expr;
        return (CONSReturn expr) 

_block = 
    do 
        stmts <- _braces _statementList
        return $ CONSBlock stmts