module Lexical.Tokens where

-- natalia's modules
import Lexical.Lexemes
import Memory.Memory
import TypeValue.TypeValue
import Types.Types

-- External modules
import Text.Parsec
import Control.Monad.IO.Class
import System.IO.Unsafe

data ReturnObject = 
    RetToken Token | 
    RetValue Value | 
    RetType Type | 
    RetNothing |
    RetMemoryCell MemoryCell deriving (Eq, Show)

getRetToken::ReturnObject -> Token
getRetToken (RetToken t) = t
getRetToken _ = error "Invalid conversion from ReturnObject to RetToken"

getRetValue::ReturnObject -> Value
getRetValue (RetValue v) = v
getRetValue _ = error "Invalid conversion from ReturnObject to RetValue"

getRetType::ReturnObject -> Type
getRetType (RetType ttype) = ttype
getRetType _ = error "Invalid conversion from ReturnObject to RetType"

getRetMemoryCell::ReturnObject -> MemoryCell
getRetMemoryCell (RetMemoryCell var) = var
getRetMemoryCell _ = error "Invalid conversion from ReturnObject to RetValue"

-- Pre-defined block (main)
mainToken :: ParsecT [Token] st IO (ReturnObject)
mainToken = tokenPrim show update_pos get_token where
    get_token (Main p) = Just (RetToken (Main p))
    get_token _       = Nothing

-- Block opening character
leftBraceToken :: ParsecT [Token] st IO (ReturnObject)
leftBraceToken = tokenPrim show update_pos get_token where
    get_token (LBrace p) = Just (RetToken (LBrace p))
    get_token _       = Nothing

-- terminal: block closing character
rightBraceToken :: ParsecT [Token] st IO (ReturnObject)
rightBraceToken = tokenPrim show update_pos get_token where
    get_token (RBrace p) = Just (RetToken (RBrace p))
    get_token _       = Nothing
    
leftBracketToken :: ParsecT [Token] st IO (ReturnObject)
leftBracketToken = tokenPrim show update_pos get_token where
    get_token (LBracket p) = Just (RetToken (LBracket p))
    get_token _       = Nothing

rightBracketToken :: ParsecT [Token] st IO (ReturnObject)
rightBracketToken = tokenPrim show update_pos get_token where
    get_token (RBracket p) = Just (RetToken (RBracket p))
    get_token _       = Nothing


-- Left parenthesis
left_paren_token :: ParsecT [Token] st IO (Token)
left_paren_token = tokenPrim show update_pos get_token where
    get_token (LParen p) = Just (LParen p)
    get_token _       = Nothing

-- Right parenthesis
right_paren_token :: ParsecT [Token] st IO (Token)
right_paren_token = tokenPrim show update_pos get_token where
    get_token (RParen p) = Just (RParen p)
    get_token _       = Nothing

plus_token :: ParsecT [Token] st IO (ReturnObject)
plus_token = tokenPrim show update_pos get_token where
    get_token (Plus p) = Just (RetToken (Plus p))
    get_token _       = Nothing


less_than_token :: ParsecT [Token] st IO (ReturnObject)
less_than_token = tokenPrim show update_pos get_token where
    get_token (LessThan p)  = Just (RetToken (LessThan p))
    get_token _             = Nothing

greater_than_token :: ParsecT [Token] st IO (ReturnObject)
greater_than_token = tokenPrim show update_pos get_token where
    get_token (GreaterThan p)  = Just (RetToken (GreaterThan p))
    get_token _             = Nothing

less_equals_token :: ParsecT [Token] st IO (ReturnObject)
less_equals_token = tokenPrim show update_pos get_token where
    get_token (LessEquals p)  = Just (RetToken (LessEquals p))
    get_token _ = Nothing

greater_equals_token :: ParsecT [Token] st IO (ReturnObject)
greater_equals_token = tokenPrim show update_pos get_token where
    get_token (GreaterEquals p)  = Just (RetToken (GreaterEquals p))
    get_token _ = Nothing   

minus_token :: ParsecT [Token] st IO (ReturnObject)
minus_token = tokenPrim show update_pos get_token where
    get_token (Minus p) = Just (RetToken (Minus p))
    get_token _       = Nothing

times_token :: ParsecT [Token] st IO (ReturnObject)
times_token = tokenPrim show update_pos get_token where
    get_token (Times p) = Just (RetToken (Times p))
    get_token _       = Nothing

expo_token :: ParsecT [Token] st IO (ReturnObject)
expo_token = tokenPrim show update_pos get_token where
    get_token (Expo p) = Just (RetToken (Expo p))
    get_token _       = Nothing

div_token :: ParsecT [Token] st IO (ReturnObject)
div_token = tokenPrim show update_pos get_token where
    get_token (Div p) = Just (RetToken (Div p))
    get_token _       = Nothing

mod_token :: ParsecT [Token] st IO (ReturnObject)
mod_token = tokenPrim show update_pos get_token where
    get_token (Mod p) = Just (RetToken (Mod p))
    get_token _       = Nothing


-- terminal: name of the *int* type %TODO: update comment
lexicalTypeToken :: ParsecT [Token] st IO (ReturnObject)
lexicalTypeToken = tokenPrim show update_pos get_token where
    get_token (Type x p) = Just (RetToken (Type x p))
    get_token _        = Nothing 

-- Identifier (of a variable/function/procedure/etc)
id_token :: ParsecT [Token] st IO (ReturnObject)
id_token = tokenPrim show update_pos get_token where
    get_token (Id x p) = Just (RetToken (Id x p))
    get_token _      = Nothing

-- terminal: assignment symbol
assignToken :: ParsecT [Token] st IO (ReturnObject)
assignToken = tokenPrim show update_pos get_token where
    get_token (Assign p) = Just (RetToken (Assign p))
    get_token _      = Nothing

-- terminal: literal of type int
int_token :: ParsecT [Token] st IO (ReturnObject)
int_token = tokenPrim show update_pos get_token where
    get_token (Int x _) = Just (RetValue (ConsNatInt x))
    get_token _       = Nothing

stringToken :: ParsecT [Token] st IO (ReturnObject)
stringToken = tokenPrim show update_pos get_token where
    get_token (String x _) = Just (RetValue (ConsNatString x))
    get_token _            = Nothing

-- literal of type double
double_token :: ParsecT [Token] st IO (ReturnObject)
double_token = tokenPrim show update_pos get_token where
    get_token (Double x p) = Just (RetValue (ConsNatDouble x))
    get_token _       = Nothing

bool_token :: ParsecT [Token] st IO (ReturnObject)
bool_token = tokenPrim show update_pos get_token where
    get_token (Bool x p) = Just (RetValue (ConsNatBool x))
    get_token _ = Nothing   

equalsToken :: ParsecT [Token] st IO (ReturnObject)
equalsToken = tokenPrim show update_pos get_token where
    get_token (Equals p) = Just (RetToken (Equals p))
    get_token _            = Nothing

differenceToken :: ParsecT [Token] st IO (ReturnObject)
differenceToken = tokenPrim show update_pos get_token where
    get_token (Difference p) = Just (RetToken (Difference p))
    get_token _ = Nothing

andToken :: ParsecT [Token] st IO (ReturnObject)
andToken = tokenPrim show update_pos get_token where
    get_token (And p) = Just (RetToken (And p))
    get_token _         = Nothing

orToken :: ParsecT [Token] st IO (ReturnObject)
orToken = tokenPrim show update_pos get_token where
    get_token (Or p) = Just (RetToken (Or p))
    get_token _         = Nothing


-- terminal: command terminator
semiColonToken :: ParsecT [Token] st IO (ReturnObject)
semiColonToken = tokenPrim show update_pos get_token where
    get_token (SemiColon p) = Just (RetToken (SemiColon p))
    get_token _         = Nothing

negationToken :: ParsecT [Token] st IO (ReturnObject)
negationToken = tokenPrim show update_pos get_token where
    get_token (Negation p)  = Just (RetToken (Negation p))
    get_token _             = Nothing

commaToken :: ParsecT [Token] st IO (ReturnObject)
commaToken = tokenPrim show update_pos get_token where
    get_token (Comma p)  = Just (RetToken (Comma p))
    get_token _ = Nothing   

generalType :: ParsecT [Token] st IO (ReturnObject)
generalType = 
    try
    primitiveType
    <|>
    aggregateType

primitiveType :: ParsecT [Token] st IO (ReturnObject) 
primitiveType = 
    do
        retprimitivetype <- lexicalTypeToken -- ReturnObject
        let primitivetype = getRetToken retprimitivetype -- Token 
        let typeToReturn = getTypeFromTypeToken primitivetype -- Type

        return (RetType typeToReturn)

aggregateType :: ParsecT [Token] st IO (ReturnObject)
aggregateType = 
    try
    setType
    <|>
    arrayType

setType :: ParsecT [Token] st IO (ReturnObject)
setType =
    do
        retlbrace <- leftBraceToken
        rettype <- generalType
        retrbrace <- rightBraceToken

        let ttype = getRetType rettype -- Type
        let return_type = RetType (NatSet ttype)

        return (return_type)

arrayType :: ParsecT [Token] st IO (ReturnObject)
arrayType =
    do
        lbrack <- leftBracketToken
        rettype <- generalType
        rbrack <- rightBracketToken
        
        let innerType = getRetType rettype -- Type
        let returnType = RetType (NatArray innerType)

        return (returnType)

-- TODO
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  