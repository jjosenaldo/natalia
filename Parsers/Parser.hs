module Main (main) where

import Lexical.Lexems
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

plusToken = tokenPrim show update_pos get_token where
    get_token Plus = Just Plus
    get_token _       = Nothing

-- terminal: block closing character
rightBraceToken = tokenPrim show update_pos get_token where
    get_token RBrace = Just RBrace
    get_token _       = Nothing

-- terminal: name of the *int* type %TODO: update comment
typeToken = tokenPrim show update_pos get_token where
    get_token (Type x) = Just (Type x)
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

-- expression evaluation
expression :: ParsecT [Token] [(Token,Token)] IO(Token)
expression = try bin_expression <|> zeroary_expression

zeroary_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
zeroary_expression = do
                   a <- intToken 
                   return (a)

bin_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
bin_expression = do
                   a <- intToken
                   b <- plusToken
                   c <- intToken
                   return (eval a b c)

eval :: Token -> Token -> Token -> Token
eval (Int x ) (Plus ) (Int y) = Int (x + y)

-- functions for the table of symbols
symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "update fail: variable not found"
symtable_update (id1, v1) ((id2, v2):t) = 
                                if id1 == id2 then (id1, v1) : t
                                else (id2, v2) : symtable_update (id1, v1) t

symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "deletion fail: variable not found"
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




-- Qual a diferença exata da symtable pra memória?
-- Precisa de float e double?