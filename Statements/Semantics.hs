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
import Data.Maybe
import System.Environment
import System.IO.Unsafe
import Text.Parsec
    

playStmtsWithoutRet :: [Statement] -> ParsecT [Token] [MemoryCell] IO ()
playStmtsWithoutRet [] = 
    do 
        return ()

playStmtsWithoutRet (stmt : stmts ) = 
    do 
        ret <- playStmtWithoutRet stmt 
        ret <- playStmtsWithoutRet stmts
        return ()

playStmtWithoutRet :: Statement -> ParsecT [Token] [MemoryCell] IO ()
playStmtWithoutRet stmt = try (playIfElseWithoutRet stmt) <|> try (playIfWithoutRet stmt ) <|> playPrint stmt


playPrint :: Statement -> ParsecT [Token] [MemoryCell] IO ()
playPrint stmt = 
    do 
        let maybeprnt = getStatementPrint stmt -- Maybe Print 

        if isNothing maybeprnt then fail ("error")
        else do 
            let prnt = fromJust maybeprnt
            let expr = getPrintExp prnt -- Exp
            val <- playMyExp expr
            
            if (getTypeFromValue val) == NatString then do 
                liftIO (putStrLn (getValueAsString val))
                return ()
            else 
                error ("EXECERROR: You can only print " ++ (getNameOfType NatString) ++ "!")

playIfWithoutRet :: Statement -> ParsecT [Token] [MemoryCell] IO ()
playIfWithoutRet stmt = 
    do 
        let maybeif = getStatementIf stmt 
        if isNothing maybeif then fail ("error")
        else do 
            let myIf = fromJust maybeif
            let blk = getIfBlock myIf
            let expr = getIfExp myIf
            exprval <- playMyExp expr 
            let actualType = getTypeFromValue exprval
            
            if not ( actualType == NatBool ) then error ("EXECERROR: The expression in a if must be of type " ++ (getNameOfType NatBool))
            else if (exprval == (ConsNatBool True)) then do 
                ret <- playBlockWithoutRet blk
                return ()
            else do 
                return ()



playIfElseWithoutRet :: Statement -> ParsecT [Token] [MemoryCell] IO ()
playIfElseWithoutRet stmt = 
    do 
        let maybeifelse = getStatementIfElse stmt -- Maybe IfElse

        if isNothing maybeifelse then fail ("error")
        else do 
            let ifelse = fromJust maybeifelse
            let blk1 = getIfElseBlock1 ifelse -- Block
            let blk2 = getIfElseBlock2 ifelse -- Block
            let expr = getIfElseExp ifelse -- Block
            exprval <- playMyExp expr 
            let actualType = getTypeFromValue exprval

            if not ( actualType == NatBool ) then error ("EXECERROR: The expression in a if must be of type " ++ (getNameOfType NatBool))
            else if (exprval == (ConsNatBool True)) then do 
                ret <- playBlockWithoutRet blk1
                return ()
            else do
                ret <- playBlockWithoutRet blk2
                return ()

playBlockWithoutRet :: Block -> ParsecT [Token] [MemoryCell] IO ()
playBlockWithoutRet blk = 
    do 
        let blkStmts = getBlockStatementList blk -- [Statement]
        ret <- playStmtsWithoutRet blkStmts
        return ()