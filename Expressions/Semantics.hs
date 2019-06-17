module Expressions.Semantics where 

-- natalia's modules
import Expressions.Grammar
import Expressions.Typing
import Lexical.Lexemes
import Memory.Memory
import PredefBlocks.Grammar
import Program.Grammar
--import Program.Parser
--import Program.ProgramState
import Statements.Grammar
import Types.Types
import TypeValue.TypeValue
import Value.Value

-- Haskell's modules
import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Text.Parsec

playExp :: Exp -> ParsecT [Token] ProgramState IO (Value)
playExp = playExpLit 

playExpLit expr = 
    do 
        let tok = getExpLitToken expr -- Token
        let val = getValueFromToken tok -- Value
        return $ val 