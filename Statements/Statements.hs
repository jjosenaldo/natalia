module Statements.Statements where

import Expressions.Expressions
import Memory.Memory
import Lexical.Lexemes
import Lexical.Tokens
import Types.Types
import TypeValue.TypeValue

import Text.Parsec
import Control.Monad.IO.Class

-- nonterminal: initialization of an *int* variable
varInitialization :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
varInitialization = do
    mem <- getState -- [MemoryCell]
    rettype <- generalType -- RetType Type
    let var_type = getRetType rettype -- Type
    name <- idToken -- RetTOken Id
    
    if (memoryHasName (get_id_name (getRetToken name)) mem) then error ("ERROR on the initialization of '" ++ (get_id_name (getRetToken name)) ++ "' at " ++ show (get_pos (getRetToken name)) ++ ": variable already exists.")
    else 
        do
            ass <- assignToken
            --liftIO(print(show(getRetToken(name))))
            expr_value <- expression -- RetValue Value
            --liftIO(print(show(getRetValue(expr_value))))
            let expr_type = getTypeFromValue (getRetValue expr_value)
            
            if (not (checkCompatibleTypes var_type expr_type)) then error ("ERROR at " ++ show(get_pos (getRetToken name))  ++ ": type mismatch in the initialization of a variable. expected: " ++ show(var_type) ++", got: " ++ show(expr_type))
            else
                do
                    let variableToInsert = Variable (ConstructVariable (get_id_name (getRetToken name)) (getRetValue expr_value) False)
                    updateState (memoryInsert variableToInsert)
                    -- optional: print symbols_table content
                    s <- getState --[MemoryCell]
                    liftIO (print s)
                    return (RetNothing)

-- nonterminal: statement
statement :: ParsecT [Token] [MemoryCell] IO()
statement = 
    try
    (do
        a <- varInitialization
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
