module Main (main) where

-----------------------------------------------  SEMANTICS  ---------------------------------------------------------

-- natalia's modules
import Lexical.Lexemes
import Lexical.Tokens
import Memory.Memory
import TypeValue.TypeValue
import Program.Semantics
import Program.Grammar
import Program.Parser

-- Haskell's modules
import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Text.Parsec


-- the entire program
program :: ParsecT [Token] ProgramState IO (Program)
program = do
            program <- _program
            eof
            return (program)

-- -- invocação do parser para o símbolo de partida 

-- parser :: [Token] -> IO (Either ParseError (Program))
-- parser tokens = runParserT program [] "Error message" tokens

-- main :: IO ()
-- main = do
--     args <- getArgs

--     if length args == 0 then
--         putStrLn "You must pass a natalia program as an argument!"
--     else
--         case unsafePerformIO (parser (getTokens (head args))) of
--                 { Left err -> print err; 
--                     Right ans -> print ans;
--                 }

-- SEMANTICS ----------------------------------------------------------------

parser :: [Token] -> IO (Either ParseError (Program))
parser tokens = runParserT _program (CONSState "" 0 [] []) "Error message" tokens

main :: IO ()
main = do
    args <- getArgs

    if length args == 0 then
        putStrLn "You must pass a natalia program as an argument!"
    else
        case unsafePerformIO (parser (getTokens (head args))) of
                { Left err -> print err; 
                    Right ans -> print ans;
                }

-- SYNTAX ----------------------------------------------------------------