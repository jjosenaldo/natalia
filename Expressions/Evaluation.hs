module Expressions.Evaluation where

import Lexical.Lexemes
import Memory.Memory
import TypeValue.TypeValue
import Types.Types
import Data.List
import Syntax.Definition


-- | Implementation of binary operations
unaryEval :: Token -- ^ operator
            -> Value -- ^ operand
            -> Value -- ^ result of the operation

-- Operator - (unary)
unaryEval (Minus p) (ConsNatInt x) = ConsNatInt (-x) 
unaryEval (Minus p) (ConsNatDouble x) = ConsNatDouble (-x) 
unaryEval (Minus p) _ = error ("ERROR at " ++ show(p) ++ ": the unary - operator expects a number.")

unaryEval (Negation p) (ConsNatBool x) = ConsNatBool (not(x)) 
unaryEval (Negation p) _ = error ("ERROR at " ++ show(p) ++ ": the unary ! operator expects a boolean.")


evaluateExpression :: Expression -> [MemoryCell] -> (Value, [MemoryCell])

-- avaliate a value
evaluateExpression (CONSValue x _) (cell) =  (x, cell) 

-- avaliate a id
evaluateExpression (CONSId (CONSTokenId id) _) (cell) = 
    (getValue   -- get the value
        (memoryGet           -- get the memory cell  
            (get_id_name id)  -- attribute of memoryGet - get the name of token (String) 
            (get_pos id)      -- attribute of memoryGet - get the position of token (int, int)
             cell )           -- attribute of memoryGet - the memory cell
        (get_pos id),   -- attribute of getValue - the position cell of token
    cell)

-- avaliate a unary operator
evaluateExpression (CONSUnOperation (CONSTokenUnOperator unOp) (exp) _) (cell) = 
    ((unaryEval 
        (unOp)
        (fst (evaluateExpression (exp) (cell)))), -- recursion ( get the value)
     cell)

-- avaliate a binary operator
evaluateExpression (CONSBinOperation (CONSTokenBinOperator binOp) (exp1) (exp2) _) (cell) =
    ((binaryEval
        (fst (evaluateExpression (exp1) (cell)))    -- recursion (get value of exp1)
        (binOp)                                     -- binary operator 
        (fst (evaluateExpression (exp2) (cell)))),  -- recursion (get value of exp2)
      cell)

-- avaliate a assignment operator
evaluateExpression (CONSExprVarAssignment (CONSTokenId id) (exp) _) (cell) =
    (fst value, 
        (memoryUpdate 
            (setValue 
                (memoryGet
                    (get_id_name id)
                    (get_pos id)
                    (cell)
                )
                (fst value)
            )
            (cell)
        )
    )
    where value = evaluateExpression (exp) (cell)

