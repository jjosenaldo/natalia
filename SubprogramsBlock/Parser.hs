module SubprogramsBlock.Parser where

import Lexical.Lexemes
import Lexical.Tokens
import Expressions.Grammar
import TypeValue.TypeValue
import Statements.Parser
import SubprogramsBlock.Grammar

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.String

_subprogramsBlock = 
    do
        stoken <- _subprogramsToken
        lBrace <- _leftBraceToken
        subPrograms <- many (_subprogram)
        rBrace <- _rightBraceToken

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
        lParen <- _leftParenToken
        params <- sepBy _param _commaToken
        colon <- colonToken
        returnType <- generalType
        let actualType = getRetType returnType
        lBrace <- _leftBraceToken
        stmtList <- many (_statement)
        return (CONSFunction (get_id_name id) params actualType stmtList)


_proc =
    do
        fToken <- _funcToken
        id <- _idToken
        lParen <- _leftParenToken
        params <- sepBy _param _commaToken
        lBrace <- _leftBraceToken
        stmtList <- many (_statement)
        return (CONSProcedure (get_id_name id) params stmtList)
