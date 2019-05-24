module Main (main) where

import Expressions.Abstractions
import Lexical.Lexemes
import Memory.Memory
import Parsers.Tokens

import Text.Parsec
import Control.Monad.IO.Class
import System.IO.Unsafe

-- nonterminal: initialization of an *int* variable
var_initialization :: ParsecT [Token] [(Token,Token)] IO([Token])
var_initialization = do
    a <- typeToken
    b <- idToken
    c <- assignToken
    d <- expression
    e <- getState

    if (not (is_compatible a d)) then fail "type mismatch"
    else
        do
            updateState(symtable_insert (b, d))

    -- optional: print symbols_table content
    s <- getState
    liftIO (print s)

    return (a:b:c:[d])

-- nonterminal: attribution of a value to an *int* variable
var_attribution :: ParsecT [Token] [(Token,Token)] IO([Token])
var_attribution = do
    a <- idToken
    b <- assignToken
    c <- intToken
    updateState(symtable_update(a,c))

    -- optional: print symbols_table content
    s <- getState
    liftIO (print s)

    return (a:b:[c])

-- nonterminal: statement
statement :: ParsecT [Token] [(Token,Token)] IO([Token])
statement = 
    (do
        a <- var_initialization
        return (a))
    <|>
    (do
        a <- var_attribution
        return (a))

-- nonterminal: list of statements
statements :: ParsecT [Token] [(Token,Token)] IO([Token])
statements = 
    (do
        a <- statement
        b <- semiColonToken
        c <- statements
        return (a ++ b:c))
    <|>
    (do
        return ([]))

-- nonterminal: the entire program
program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
            a <- mainToken
            b <- leftBraceToken
            c <- statements
            d <- rightBraceToken
            eof
            return (a:b:c ++ [d])

-- functions for types

get_value :: Token -> [(Token, Token)] -> Token
get_value _ [] = error "getValue error: variable not found"
get_value (Id id1) ((Id id2, value):t) = if id1 == id2 then value
                                                else get_value (Id id1) t
-- checks if two VALUES are of compatible types
is_compatible :: Token -> Token -> Bool
is_compatible (Int _ ) (Int _ ) = True
is_compatible (Type x) (Int _ ) 
    | x == "int" = True
    | otherwise = False
is_compatible _ _ = False

-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "./Programs/init-int.nat")) of
            { Left err -> print err; 
                Right ans -> print ans
            }




-- Qual a diferença exata da symtable pra memória?
-- Precisa de float e double?