module Expressions.EvaluationTest where 

-- natalia's modules
import Expressions.Evaluation
import Lexical.Lexemes
import Lexical.Tokens
import Syntax.Expressions
import TypeValue.TypeValue

-- Haskell modules
import Control.Monad.IO.Class
import Text.Parsec

expressionEvaluationTest :: ParsecT [Token] st IO (ReturnObject)
expressionEvaluationTest = 
    do 
        retExpression <- _expression NatInt
        let actualExpression = getRetExpression retExpression -- Expression
        let result = evaluateExpression actualExpression [] -- (Value, [MemoryCell])

        -- Prints the Value
        liftIO(print(show(fst result ))) 

        -- Prints the [MemoryCell]
        liftIO(print(show(snd result ))) 

        return(RetNothing)