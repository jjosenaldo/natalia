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
    

playStmtsWithoutRet :: [Statement] -> ParsecT [Token] ProgramState IO ()
playStmtsWithoutRet [] = 
    do 
        return ()

playStmtsWithoutRet (stmt : stmts ) = 
    do 
        ret <- playStmtWithoutRet stmt 
        ret <- playStmtsWithoutRet stmts
        return ()

playStmtWithoutRet :: Statement -> ParsecT [Token] ProgramState IO ()
playStmtWithoutRet stmt = {-try (playWhile stmt) <|>-} try (playVarInit stmt) <|> try (playIfElseWithoutRet stmt) <|> try (playIfWithoutRet stmt ) <|> playPrint stmt


playPrint :: Statement -> ParsecT [Token] ProgramState IO ()
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

playIfWithoutRet :: Statement -> ParsecT [Token] ProgramState IO ()
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



playIfElseWithoutRet :: Statement -> ParsecT [Token] ProgramState IO ()
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

playBlockWithoutRet :: Block -> ParsecT [Token] ProgramState IO ()
playBlockWithoutRet blk = 
    do 
        let blkStmts = getBlockStatementList blk -- [Statement]
        ret <- playStmtsWithoutRet blkStmts
        return ()

playVarInit :: Statement -> ParsecT [Token] ProgramState IO ()
playVarInit stmt = 
    do 
        let maybevarinit = getStatementVarInit stmt 
        if isNothing maybevarinit then fail ("error in the init of a variable")
        else do 
            let varinit = fromJust maybevarinit -- VarInit
            let t = getVarInitType varinit -- Type 
            let n = getVarInitName varinit -- String
            let expr = getVarInitExp varinit -- Exp
            exprval <- playMyExp expr -- Value 
            let exprtype = getTypeFromValue exprval -- Type 

            if checkCompatibleTypes t exprtype then do 
                s <- getState
                let level = getStateLevel s -- Int
                let sub = getStateActiveSubprogram s-- String
                let var = ConstructVariable n exprval False sub level
                let cell = Variable var -- MemoryCell
                modifyState(memoryInsert cell)
                return ()

            else error ("EXECERROR: you can't assign a " ++ (getNameOfType exprtype) ++ " to a variable of type " ++ (getNameOfType t))

-- playAssignment :: Statement -> ParsecT [Token] ProgramState IO ()
-- playAssignment stmt =
--     do 
--         let maybeassign = getStatementAssignment stmt 
--         if isNothing maybeassign then fail ("error")
--         else do
--             let assign = fromJust maybeassign -- Assignment
--             let asslvalue = getLValueAssign assign -- LValue
--             let expr = getVarAssignExp assign -- Exp 
--             newVal <- playMyExp expr -- Value

--             if isLValueLocalVar asslvalue then do 
--                 let id = getLocalVarId asslvalue -- String
--                 s <- getState -- ProgramState
--                 cell <- getMemoryCellByName id s -- MemoryCell
--                 newCell <- setValue cell newVal -- MemoryCell

-- playWhile :: Statement -> ParsecT [Token] ProgramState IO ()
-- playWhile stmt = 
--     do 
--         let maybewhile = getStatementWhile stmt -- Maybe While
--         if isNothing maybewhile then fail ("error in a while") 
--         else do 
--             let while = fromJust maybewhile -- While 
--             let expr = getWhileExp while -- Exp 
--             let blk = getWhileBlock while -- Block
--             ret <- semanticWhile expr blk 

-- semanticWhile expr blk = 
--     do 
