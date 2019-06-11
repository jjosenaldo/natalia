module Statements.Statements where

import Expressions.Expressions
import Memory.Memory
import Lexical.Lexemes
import Lexical.Tokens
import Types.Types

import Text.Parsec
import Control.Monad.IO.Class

-- nonterminal: initialization of an *int* variable
var_initialization :: ParsecT [Token] [MemoryCell] IO()
var_initialization = do
    mem <- getState -- [MemoryCell]
    t <- typeToken -- RetToken Type
    name <- id_token -- RetTOken Id

    if memory_has_name (get_id_name (getRetToken name)) mem then fail ("ERROR on the initialization of '" ++ (get_id_name (getRetToken name)) ++ "' at " ++ show (get_pos (getRetToken name)) ++ ": variable already exists.")
    else 
        do
            ass <- assignToken
            expr_value <- expression -- RetValue Value
            

            let expr_type = getTypeFromValue (getRetValue expr_value)

            if (not (checkCompatibleTypes (getTypeFromTypeToken (getRetToken t)) expr_type)) then fail ("ERROR at " ++ show(get_pos (getRetToken name))  ++ ": type mismatch in the initialization of a variable.")
            else
                do
                    let variableToInsert = Variable (ConstructVariable (get_id_name (getRetToken name)) (getRetValue expr_value) False)
                    updateState (memory_insert variableToInsert)
                    -- optional: print symbols_table content
                    s <- getState --[MemoryCell]
                    liftIO (print s)
                    return ()

-- nonterminal: statement
statement :: ParsecT [Token] [MemoryCell] IO()
statement = 
    --try
    (do
        a <- var_initialization
        return ())
    <|>
    (do
        a <- var_attribution
        return ())

-- nonterminal: list of statements
statements :: ParsecT [Token] [MemoryCell] IO()
statements = 
    try
    (do
        a <- statement
        b <- semiColonToken
        c <- statements
        return ())
    <|>
    (do
        return ())