module Lexical.Tokens where

-- natalia's modules
import Lexical.Lexemes
import Syntax.Definition
import Memory.Memory
import TypeValue.TypeValue
import Types.Typedef
import Types.Types

-- Haskell modules
import Text.Parsec
import Control.Monad.IO.Class
import System.IO.Unsafe

data ReturnObject = 
    RetToken Token | 
    RetValue Value | 
    RetType Type | 
    RetNothing |
    RetMemoryCell MemoryCell |
    RetStructStructure [(Type, String)] |
    RetExpression Expression |
    RetUnOperator UnOperator |
    RetBinOperator BinOperator |
    RetStructValues [(String, Value)] deriving (Eq, Show)


getRetUnOperator :: ReturnObject -> UnOperator
getRetUnOperator (RetUnOperator x) = x
getRetUnOperator _ = error "Invalid conversion from ReturnObject to RetUnOperator"

getRetBinOperator :: ReturnObject -> BinOperator
getRetBinOperator (RetBinOperator x) = x
getRetBinOperator _ = error "Invalid conversion from ReturnObject to RetBinOperator"

getRetExpression :: ReturnObject -> Expression
getRetExpression (RetExpression x) = x
getRetExpression _ = error "Invalid conversion from ReturnObject to RetExpression"

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
getRetMemoryCell _ = error "Invalid conversion from ReturnObject to RetMemoryCell"

getRetStructStructure :: ReturnObject -> [(Type, String)]
getRetStructStructure (RetStructStructure x) = x
getRetStructStructure _ = error "Invalid conversion from ReturnObject to RetStructStructure"

getRetStructValues :: ReturnObject -> [(String, Value)]
getRetStructValues (RetStructValues x) = x
getRetStructValues _ = error "Invalid conversion from ReturnObject to RetStructValues"

-- Pre-defined block (main)
mainToken :: ParsecT [Token] st IO (ReturnObject)
mainToken = tokenPrim show update_pos get_token where
    get_token (Main p) = Just (RetToken (Main p))
    get_token _       = Nothing

-- Pre-defined block (typedefs)
typedefsToken :: ParsecT [Token] st IO (ReturnObject)
typedefsToken = tokenPrim show update_pos get_token where
    get_token (Typedefs p) = Just (RetToken (Typedefs p))
    get_token _       = Nothing

-- Pre-defined block (subprograms)
subprogramsToken :: ParsecT [Token] st IO (ReturnObject)
subprogramsToken = tokenPrim show update_pos get_token where
    get_token (Subprograms p) = Just (RetToken (Subprograms p))
    get_token _       = Nothing

-- Pre-defined block (globals)
globalsToken :: ParsecT [Token] st IO (ReturnObject)
globalsToken = tokenPrim show update_pos get_token where
    get_token (Globals p) = Just (RetToken (Globals p))
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
leftParenToken :: ParsecT [Token] st IO (ReturnObject)
leftParenToken = tokenPrim show update_pos get_token where
    get_token (LParen p) = Just (RetToken (LParen p))
    get_token _       = Nothing

-- Right parenthesis
rightParenToken :: ParsecT [Token] st IO (ReturnObject)
rightParenToken = tokenPrim show update_pos get_token where
    get_token (RParen p) = Just (RetToken (RParen p))
    get_token _       = Nothing

plusToken :: ParsecT [Token] st IO (ReturnObject)
plusToken = tokenPrim show update_pos get_token where
    get_token (Plus p) = Just (RetToken (Plus p))
    get_token _       = Nothing


lessThanToken :: ParsecT [Token] st IO (ReturnObject)
lessThanToken = tokenPrim show update_pos get_token where
    get_token (LessThan p)  = Just (RetToken (LessThan p))
    get_token _             = Nothing

greater_than_token :: ParsecT [Token] st IO (ReturnObject)
greater_than_token = tokenPrim show update_pos get_token where
    get_token (GreaterThan p)  = Just (RetToken (GreaterThan p))
    get_token _             = Nothing

lessEqualsToken :: ParsecT [Token] st IO (ReturnObject)
lessEqualsToken = tokenPrim show update_pos get_token where
    get_token (LessEquals p)  = Just (RetToken (LessEquals p))
    get_token _ = Nothing

greater_equals_token :: ParsecT [Token] st IO (ReturnObject)
greater_equals_token = tokenPrim show update_pos get_token where
    get_token (GreaterEquals p)  = Just (RetToken (GreaterEquals p))
    get_token _ = Nothing   

minusToken :: ParsecT [Token] st IO (ReturnObject)
minusToken = tokenPrim show update_pos get_token where
    get_token (Minus p) = Just (RetToken (Minus p))
    get_token _       = Nothing

timesToken :: ParsecT [Token] st IO (ReturnObject)
timesToken = tokenPrim show update_pos get_token where
    get_token (Times p) = Just (RetToken (Times p))
    get_token _       = Nothing

expoToken :: ParsecT [Token] st IO (ReturnObject)
expoToken = tokenPrim show update_pos get_token where
    get_token (Expo p) = Just (RetToken (Expo p))
    get_token _       = Nothing

divToken :: ParsecT [Token] st IO (ReturnObject)
divToken = tokenPrim show update_pos get_token where
    get_token (Div p) = Just (RetToken (Div p))
    get_token _       = Nothing

modToken :: ParsecT [Token] st IO (ReturnObject)
modToken = tokenPrim show update_pos get_token where
    get_token (Mod p) = Just (RetToken (Mod p))
    get_token _       = Nothing


-- terminal: name of the *int* type %TODO: update comment
lexicalTypeToken :: ParsecT [Token] st IO (ReturnObject)
lexicalTypeToken = tokenPrim show update_pos get_token where
    get_token (Type x p) = Just (RetToken (Type x p))
    get_token _        = Nothing 

-- Identifier (of a variable/function/procedure/etc)
idToken :: ParsecT [Token] st IO (ReturnObject)
idToken = tokenPrim show update_pos get_token where
    get_token (Id x p) = Just (RetToken (Id x p))
    get_token _      = Nothing

-- terminal: assignment symbol
assignToken :: ParsecT [Token] st IO (ReturnObject)
assignToken = tokenPrim show update_pos get_token where
    get_token (Assign p) = Just (RetToken (Assign p))
    get_token _      = Nothing

-- terminal: literal of type int
intToken :: ParsecT [Token] st IO (ReturnObject)
intToken = tokenPrim show update_pos get_token where
    get_token (Int x _) = Just (RetValue (ConsNatInt x))
    get_token _       = Nothing

nullToken :: ParsecT [Token] st IO (ReturnObject)
nullToken = tokenPrim show update_pos get_token where
    get_token (Null _)  = Just (RetValue (ConsNatNull))
    get_token _         = Nothing

stringToken :: ParsecT [Token] st IO (ReturnObject)
stringToken = tokenPrim show update_pos get_token where
    get_token (String x _) = Just (RetValue (ConsNatString x))
    get_token _            = Nothing

-- literal of type double
doubleToken :: ParsecT [Token] st IO (ReturnObject)
doubleToken = tokenPrim show update_pos get_token where
    get_token (Double x p) = Just (RetValue (ConsNatDouble x))
    get_token _       = Nothing

boolToken :: ParsecT [Token] st IO (ReturnObject)
boolToken = tokenPrim show update_pos get_token where
    get_token (Bool x p) = Just (RetValue (ConsNatBool x))
    get_token _ = Nothing     

equalsToken :: ParsecT [Token] st IO (ReturnObject)
equalsToken = tokenPrim show update_pos get_token where
    get_token (Equals p) = Just (RetToken (Equals p))
    get_token _            = Nothing

differentToken :: ParsecT [Token] st IO (ReturnObject)
differentToken = tokenPrim show update_pos get_token where
    get_token (Different p) = Just (RetToken (Different p))
    get_token _ = Nothing

andToken :: ParsecT [Token] st IO (ReturnObject)
andToken = tokenPrim show update_pos get_token where
    get_token (And p) = Just (RetToken (And p))
    get_token _         = Nothing

orToken :: ParsecT [Token] st IO (ReturnObject)
orToken = tokenPrim show update_pos get_token where
    get_token (Or p) = Just (RetToken (Or p))
    get_token _         = Nothing

-- operator ?
inToken :: ParsecT [Token] st IO (ReturnObject)
inToken = tokenPrim show update_pos get_token where
    get_token (In p) = Just (RetToken (In p))
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
    
dotToken :: ParsecT [Token] st IO (ReturnObject)
dotToken = tokenPrim show update_pos get_token where
    get_token (Dot p)  = Just (RetToken (Dot p))
    get_token _ = Nothing

generalType :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
generalType = 
    try preDefinedTypedefType <|> try primitiveType <|> try aggregateType

preDefinedTypedefType :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
preDefinedTypedefType = 
    do
        retIdToken <- idToken -- RetToken
        let actualIdToken = getRetToken retIdToken -- Id
        memory <- getState
        let pos = get_pos actualIdToken -- (Int, Int)
        let idName = get_id_name actualIdToken -- String
        let typedefReturnType = getMemoryCellType (memory_get idName pos memory)

        return (RetType(getTypedefType typedefReturnType))

primitiveType :: ParsecT [Token] [MemoryCell] IO (ReturnObject) 
primitiveType = 
    do
        retprimitivetype <- lexicalTypeToken -- ReturnObject
        let primitivetype = getRetToken retprimitivetype -- Token 
        let typeToReturn = getTypeFromTypeToken primitivetype -- Type

        return (RetType typeToReturn)

aggregateType :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
aggregateType = 
    try
    setType
    <|>
    arrayType

setType :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
setType =
    do
        retlbrace <- leftBraceToken
        rettype <- generalType
        retrbrace <- rightBraceToken

        let ttype = getRetType rettype -- Type
        let return_type = RetType (NatSet ttype)

        return (return_type)

arrayType :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
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