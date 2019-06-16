module Syntax.ExpressionTest where 

-- natalia's modules
import Lexical.Lexemes
import Lexical.Tokens
import Syntax.Expressions
import TypeValue.TypeValue

-- Haskell modules
import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Text.Parsec


parserExprTest :: [Token] -> IO (Either ParseError (ReturnObject))
parserExprTest tokens = runParserT (_expression NatInt) [] "Syntactical error:" tokens