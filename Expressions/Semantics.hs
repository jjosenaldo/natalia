module Expressions.Semantics where 

-- natalia's modules
import Expressions.Grammar
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

binaryEval :: Value -- ^ first operand
            -> Token -- ^ operator
            -> Value -- ^ second operand
            -> Value -- ^ result of the operation

binaryEval (ConsNatString x) (Plus _) (ConsNatString y) = ConsNatString (x ++ y)
binaryEval _ (Plus p) _ = error ("ERROR at " ++ show(p) ++ ": the + operator expects two numbers.")


playMyExp :: Exp -> ParsecT [Token] [MemoryCell] IO (Value)
playMyExp expr = 
    do 
        
        mem <- getState -- [MemoryCell]
        let newExp = setExpType mem expr -- Exp
        val <- playExp newExp
        return $ val

playExp :: Exp -> ParsecT [Token] [MemoryCell] IO (Value)
playExp expr = (try (playExpBinEval expr)) <|> (playExpLit expr) 

playExpLit expr = 
    do 
        let tok = getExpLitToken expr -- Token
        let val = getValueFromToken tok -- Value
        return $ val 

playExpBinEval (CONSExpBin t (CONSBinOp binOp) expr1 expr2) = 
    do 
        res1 <- (playExp expr1)
        res2 <- (playExp expr2)
        let val = (binaryEval (res1) (binOp) (res2))
        return $ val
        
playExpBinEval _ = 
    do 
        fail ("error")