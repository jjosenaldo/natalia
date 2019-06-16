module TestTypingFunction where

-- natalia's modules
import Lexical.Lexemes
import Expressions.Grammar
import Expressions.Parser

-- Haskell modules
-- import Control.Monad.IO.Class
import System.Environment -- getArgs
import System.IO.Unsafe -- unsafePerformIO
import Text.Parsec -- ParseError, 

main :: IO ()
main = do
    args <- getArgs

    if length args == 0 then
        putStrLn "You must pass a natalia program as an argument!"
    else
        case unsafePerformIO (parser (getTokens (head args))) of
                { Left err -> print err; 
                    Right ans -> do
                                    print(ans)
                }

parser :: [Token] -> IO (Either ParseError (Exp))
parser tokens = 
    do 
        expWithoutType <- runParserT _expr [] "Syntactical error:" tokens
        let expWithType = setExpType