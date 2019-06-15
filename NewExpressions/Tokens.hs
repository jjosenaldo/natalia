module NewExpressions.Tokens where

-- natalia's modules
import Lexical.Lexemes
import NewExpressions.Grammar

-- Haskell modules
import Text.Parsec

-- plusToken = tokenPrim show updatePos get_token where
--     get_token (Plus p) = Just (  BinOperation (Plus p)   )
--     get_token _       = Nothing

-- timesToken = tokenPrim show updatePos get_token where
--     get_token (Times p) = Just (  BinOperation (Times p)   )
--     get_token _       = Nothing

-- intToken = tokenPrim show updatePos get_token where
--     get_token (Int x p) = Just  (  CONSLitInt x p )
--     get_token _       = Nothing

boolToken :: ParsecT [Token] st IO (BoolExp)
boolToken = tokenPrim show updatePos get_token where
    get_token (Bool x p) = Just  (  CONSBoolExpLit (Bool x p) )
    get_token _       = Nothing

andToken :: ParsecT [Token] st IO (BoolExp -> BoolExp -> BoolExp)
andToken = tokenPrim show updatePos get_token where
    get_token (And p) = Just  (  CONSBoolExpBin (CONSBoolBinOp (And p)) )
    get_token _       = Nothing

orToken :: ParsecT [Token] st IO (BoolExp -> BoolExp -> BoolExp)
orToken = tokenPrim show updatePos get_token where
    get_token (Or p) = Just  (  CONSBoolExpBin (CONSBoolBinOp (Or p)) )
    get_token _       = Nothing

negationToken :: ParsecT [Token] st IO (BoolExp -> BoolExp)
negationToken = tokenPrim show updatePos get_token where
    get_token (Negation p) = Just  (  CONSBoolExpUn (CONSBoolUnOp (Negation p)) )
    get_token _       = Nothing


-- stringToken = tokenPrim show updatePos get_token where
--     get_token (String x p) = Just  (  CONSLitString x p)
--     get_token _       = Nothing

-- doubleToken = tokenPrim show updatePos get_token where
--     get_token (Double x p) = Just  (  CONSLitDouble x p)
--     get_token _       = Nothing

-- literals = intToken <|> boolToken <|> stringToken <|> doubleToken

-- TODO
updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos _ (tok:_) = pos -- necessita melhoria
updatePos pos _ []      = pos  