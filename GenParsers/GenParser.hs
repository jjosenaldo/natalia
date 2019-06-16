module GenParsers.GenParser where 

-- natalia's modules
import Lexical.Tokens

-- Haskell modules
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.String

_parens p = 
    do 
        lp <- _leftParenToken
        pp <- p
        rp <- _rightParenToken
        return $ pp

_braces p = 
    do 
        lb <- _leftBraceToken
        pp <- p
        rb <- _rightBraceToken
        return $ pp