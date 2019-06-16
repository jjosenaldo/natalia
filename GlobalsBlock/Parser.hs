module GlobalsBlock.Parser where 

import GlobalsBlock.Grammar
import Lexical.Tokens
import Lexical.Lexemes
import Expressions.Parser

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.String

_globalsBlock = 
    do 
        initList <- many (_initialization)
        return (CONSGlobalsBlock initList)

_initialization = 
    do
        retType <- generalType
        let actualType = getRetType retType
        id <- _idToken
        assignToken <- _assignToken
        expr <- _expr
        semiColon <- _semiColonToken

        return (CONSInitialization actualType  (get_id_name id) expr)

