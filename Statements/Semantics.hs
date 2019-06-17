module Statements.Semantics where

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
import Types.Types
import TypeValue.TypeValue
import Value.Value

-- Haskell's modules
import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Text.Parsec
    

playStmtsWithNoReturn :: [Statement] -> ParsecT [Token] [MemoryCell] IO ()
playStmtsWithNoReturn [] = 
    do 
        return ()

playStmtsWithNoReturn (stmt : stmts ) = 
    do 
        ret <- playStmtWithNoReturn stmt 
        ret <- playStmtsWithNoReturn stmts
        return ()

playStmtWithNoReturn = playPrint


playPrint :: Statement -> ParsecT [Token] [MemoryCell] IO ()
playPrint stmt = 
    do 
        let prnt = getStatementPrint stmt -- Print 
        let expr = getPrintExp prnt -- Exp
        
        if (getExpType expr) == NatString then do 
            val <- playExp expr -- Value
            let valAsStr = getValueAsString val -- String
            liftIO (putStrLn valAsStr)
            return ()
        else 
            error ("EXECERROR: You can only print " ++ (getNameOfType NatString) ++ "!")

        return ()
