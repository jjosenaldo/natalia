module Main (main) where

import Lexical.Tokens
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

-- terminal: pre-defined block (main)
mainToken = tokenPrim show update_pos get_token where
    get_token Main = Just Main
    get_token _       = Nothing

-- terminal: block opening character
leftBraceToken = tokenPrim show update_pos get_token where
    get_token LBrace = Just LBrace
    get_token _       = Nothing

-- terminal: block closing character
rightBraceToken = tokenPrim show update_pos get_token where
    get_token RBrace = Just RBrace
    get_token _       = Nothing

-- terminal: name of the *int* type
typeIntToken = tokenPrim show update_pos get_token where
    get_token TypeInt = Just TypeInt
    get_token _        = Nothing 

-- terminal: identifier name
idToken = tokenPrim show update_pos get_token where
    get_token (Id x) = Just (Id x)
    get_token _      = Nothing

-- terminal: assignment symbol
assignToken = tokenPrim show update_pos get_token where
    get_token Assign = Just Assign
    get_token _      = Nothing

-- terminal: literal of type int
intToken = tokenPrim show update_pos get_token where
    get_token (Int x) = Just (Int x)
    get_token _       = Nothing

-- terminal: command terminator
semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
    get_token SemiColon = Just SemiColon
    get_token _         = Nothing

-- TODO
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  

-- nonterminal: initialization of an *int* variable
varInitialization :: ParsecT [Token] [(Token,Token)] IO([Token])
varInitialization = do
    a <- typeIntToken
    b <- idToken
    c <- assignToken
    d <- intToken
    updateState(symtable_insert(b, d))

    -- optional: print symbols_table content
    s <- getState
    liftIO (print s)

    return (a:b:c:[d])

-- nonterminal: attribution of a value to an *int* variable
varAttribution :: ParsecT [Token] [(Token,Token)] IO([Token])
varAttribution = do
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
        a <- varInitialization
        return (a))
    <|>
    (do
        a <- varAttribution
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

-- functions for the table of symbols

get_default_value :: Token -> Token
get_default_value (TypeInt) = Int 0          

symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (id1, v1) ((id2, v2):t) = 
                                if id1 == id2 then (id1, v1) : t
                                else (id2, v2) : symtable_update (id1, v1) t

symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (id1, v1) ((id2, v2):t) = 
                                if id1 == id2 then t
                                else (id2, v2) : symtable_remove (id1, v1) t                               


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "./Programs/init-int.nat")) of
            { Left err -> print err; 
                Right ans -> print ans
            }