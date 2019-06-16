module SemExprTest where 

-- natalia's modules
import Lexical.Lexemes
import Expressions.Grammar
import Expressions.Parser
import Expressions.Semantics
import Expressions.Typing

-- Haskell's modules
import System.Environment -- getArgs
import System.IO.Unsafe -- unsafePerformIO
import Text.Parsec -- ParseError

main :: IO ()
main = do
    args <- getArgs

    if length args == 0 then
        putStrLn "You must pass a natalia program as an argument!"
    else
        case unsafePerformIO (parser (getTokens (head args))) of
                { Left err -> print err; 
                    Right exprTree -> do
                                    -- That's the memory after parsing the whole .nat file for the first time. It contains the definitions of global variables, structs and subprograms.
                                    let memoryWithDefinitions = []
                                    let expWithType = setExpType memoryWithDefinitions exprTree
                                    
                                    -- That's the memory at some point in the execution of the program. It contains local variables alongside with definitions of things.
                                    let runningTimeMemory = []
                                    let result = playExpression runningTimeMemory expWithType 
                                    print(fst result)
                                    print(snd result)
                }

parser :: [Token] -> IO (Either ParseError (Exp))
parser tokens = runParserT   _expr  [] "Syntactical error:" tokens -- Don't alter this line!