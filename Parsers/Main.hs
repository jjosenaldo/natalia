module Main (main) where

import Lexical.Lexemes
import Lexical.Tokens
import Statements.Statements

import Text.Parsec
import Control.Monad.IO.Class
import System.IO.Unsafe

-- the entire program
program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
            a <- mainToken
            b <- leftBraceToken
            c <- statements
            d <- rightBraceToken
            eof
            return (a:b:c ++ [d])

-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "./Programs/init-int.nat")) of
            { Left err -> print err; 
                Right ans -> print ans
            }




