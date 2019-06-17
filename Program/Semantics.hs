module Program.Semantics where

-- natalia's modules
import Expressions.Grammar
import Expressions.Semantics
import Expressions.Typing
import Lexical.Lexemes
import Memory.Memory
import PredefBlocks.Grammar
import Program.Grammar
import Program.Parser
import Statements.Grammar
import Statements.Semantics
import Types.Types
import TypeValue.TypeValue
import Value.Value

-- Haskell's modules
import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Text.Parsec

playProgram  = 
    do 
        pg <- _program -- Program
        let mainBlk = getProgramMainBlock pg -- MainBlock
        let mainStmts = getMainBlockStatements mainBlk -- [Statements]
        ret <- playStmtsWithNoReturn mainStmts
        s <- getState
        liftIO(print(s))
        return ()