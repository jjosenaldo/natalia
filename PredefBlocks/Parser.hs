module PredefBlocks.Parser where

-- natalia's modules
import Expressions.Parser
import GenParsers.GenParser
import Lexical.Lexemes
import Lexical.Tokens
import PredefBlocks.Grammar
import Statements.Parser
import Types.Typedef
import TypeValue.TypeValue

-- Haskell modules
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.String


-- TYPEDEFS BLOCK -------------------------------------------------------------------------------
_typedefsBlock =
    do
        ttoken <- _typedefsToken
        tdList <- _braces typedefsList
        return (CONSTypedefsBlock tdList) 

_typedef = (try _typeAlias) <|> (try _structDef)

_structFieldDecl structName =
    try
    (do -- recursive field
        id <- _idToken
        nameId <- _idToken
        let fieldType = get_id_name id
        let fieldName = get_id_name nameId
        scolon <- _semiColonToken
        if fieldType == structName then 
            do
                return ((NatStruct structName), fieldName)
        else
            do
                fail ("expected recursive field definition"))
    <|>
    (do -- nonrecursive field
        retType <- generalType
        let actualType = getRetType retType
        id <- _idToken
        scolon <- _semiColonToken
        return (actualType, (get_id_name id)))

_structDef =
    do
        id <- _idToken
        structFields <- _braces (many (_structFieldDecl (get_id_name id)))
        return (StructDef (get_id_name id) structFields)

_typeAlias =
    do
        id <- _idToken
        retType <- generalType
        let actualType = getRetType retType
        return (ConsTypedef (get_id_name id) actualType)

typedefsList = 
    do -- type def
        tdList <- sepBy _typedef _commaToken
        return tdList



-- SUBPROGRAMS BLOCK ----------------------------------------------------------------------------

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

-- GLOBALS BLOCK ----------------------------------------------------------------------------------------------

_globalsBlock = 
    do 
        gtoken <- _globalsToken
        lBrace <- _leftBraceToken
        initList <- many (_initialization)
        rBrace <- _rightBraceToken
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


-- MAIN BLOCK --------------------------------------------------------------------------------------------------------

_mainBlock =
    do
        mtoken <- _mainToken
        stmtList <- _braces _statementList
        return $ CONSMainBlock stmtList
