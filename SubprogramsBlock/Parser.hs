module SubprogramsBlock.Parser where

import Expressions.Grammar
import GenParsers.GenParser
import Lexical.Lexemes
import Lexical.Tokens
import Statements.Parser
import SubprogramsBlock.Grammar
import TypeValue.TypeValue

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.String

_subprogramsBlock = 
    do
        stoken <- _subprogramsToken
        
        subPrograms <- _braces $  many (_subprogram)

        return (CONSSubprogramsBlock subPrograms)

_subprogram = (try _func) <|> (try _proc)

_param =
    do
        retType <- generalType
        let actualType = getRetType retType
        name <- _idToken
        return (actualType, get_id_name name)

_func =
    do
        fToken <- _funcToken
        id <- _idToken
        params <- _parens $ sepBy _param _commaToken
        colon <- colonToken
        returnType <- generalType
        let actualType = getRetType returnType
        stmtList <- _braces _statementList
        return (CONSFunction (get_id_name id) params actualType stmtList)


_proc =
    do
        pToken <- _procToken
        id <- _idToken
        params <- _parens $ sepBy _param _commaToken
        stmtList <- _braces _statementList
        return (CONSProcedure (get_id_name id) params stmtList)
