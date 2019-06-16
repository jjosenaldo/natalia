module Lexical.Tokens where

-- natalia's modules
import Expressions.Grammar
import Lexical.Lexemes
import Memory.Memory
import TypeValue.TypeValue
import Types.Typedef
import Types.Types

-- Haskell modules
import Text.Parsec
import Control.Monad.IO.Class
import System.IO.Unsafe

data ReturnObject = 
    RetBinOperator BinOperator |
    RetBlock Block |
    RetExpression Expression |
    RetMemoryCell MemoryCell |
    RetNothing |
    RetStatement Statement | 
    RetStatementList [Statement] | 
    RetStructStructure [(Type, String)] |
    RetStructValues [(String, Value)] |
    RetToken Token | 
    RetType Type | 
    RetValue Value | 
    RetVarInit VarInit |
    RetUnOperator UnOperator |
    RetParamList [Parameter] |
    RetExpressionList [Expression]
    deriving (Eq, Show)


getRetStatementList :: ReturnObject -> [Statement]
getRetStatementList (RetStatementList x) = x
getRetStatementList _ = error "Invalid conversion from ReturnObject to StatementList"

getRetVarInit :: ReturnObject -> VarInit
getRetVarInit (RetVarInit x) = x
getRetVarInit _ = error "Invalid conversion from ReturnObject to VarInit"

getRetStatement :: ReturnObject -> Statement
getRetStatement (RetStatement x) = x
getRetStatement _ = error "Invalid conversion from ReturnObject to Statement"

getRetBlock :: ReturnObject -> Block
getRetBlock (RetBlock x) = x
getRetBlock _ = error "Invalid conversion from ReturnObject to Block"

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

getRetParamList :: ReturnObject -> [Parameter]
getRetParamList (RetParamList x) = x
getRetParamList _ = error "Invalid conversion from ReturnObject to RetParamList"

getRetExpressionList :: ReturnObject -> [Expression]
getRetExpressionList (RetExpressionList x) = x
getRetExpressionList _ = error "Invalid conversion from ReturnObject to RetExpressionList"

procToken :: ParsecT [Token] st IO (ReturnObject)
procToken = tokenPrim show updatePos get_token where
    get_token (Proc p)  = Just (RetToken (Proc p))
    get_token _         = Nothing

_funcToken :: ParsecT [Token] st IO (Token)
_funcToken = tokenPrim show updatePos get_token where
    get_token (Func p)  = Just (Func p)
    get_token _         = Nothing

funcToken :: ParsecT [Token] st IO (ReturnObject)
funcToken = tokenPrim show updatePos get_token where
    get_token (Func p)  = Just (RetToken (Func p))
    get_token _         = Nothing

mainToken :: ParsecT [Token] st IO (ReturnObject)
mainToken = tokenPrim show updatePos get_token where
    get_token (Main p) = Just (RetToken (Main p))
    get_token _       = Nothing
-- Pre-defined block (main)
_mainToken :: ParsecT [Token] st IO (Token)
_mainToken = tokenPrim show updatePos get_token where
    get_token (Main p) = Just (Main p)
    get_token _       = Nothing

-- Pre-defined block (typedefs)
typedefsToken :: ParsecT [Token] st IO (ReturnObject)
typedefsToken = tokenPrim show updatePos get_token where
    get_token (Typedefs p) = Just (RetToken (Typedefs p))
    get_token _       = Nothing

-- Pre-defined block (subprograms)
_subprogramsToken :: ParsecT [Token] st IO (Token)
_subprogramsToken = tokenPrim show updatePos get_token where
    get_token (Subprograms p) = Just (Subprograms p)
    get_token _       = Nothing


subprogramsToken :: ParsecT [Token] st IO (ReturnObject)
subprogramsToken = tokenPrim show updatePos get_token where
    get_token (Subprograms p) = Just (RetToken (Subprograms p))
    get_token _       = Nothing

-- Pre-defined block (globals)
globalsToken :: ParsecT [Token] st IO (ReturnObject)
globalsToken = tokenPrim show updatePos get_token where
    get_token (Globals p) = Just (RetToken (Globals p))
    get_token _       = Nothing

ifToken :: ParsecT [Token] st IO (ReturnObject)
ifToken = tokenPrim show updatePos get_token where
    get_token (If p) = Just (RetToken (If p))
    get_token _      = Nothing

whileToken :: ParsecT [Token] st IO (ReturnObject)
whileToken = tokenPrim show updatePos get_token where
    get_token (While p) = Just (RetToken (While p))
    get_token _      = Nothing

elseToken :: ParsecT [Token] st IO (ReturnObject)
elseToken = tokenPrim show updatePos get_token where
    get_token (Else p) = Just (RetToken (Else p))
    get_token _      = Nothing

printToken :: ParsecT [Token] st IO (ReturnObject)
printToken = tokenPrim show updatePos get_token where
    get_token (Print p) = Just (RetToken (Print p))
    get_token _       = Nothing

-- Block opening character
leftBraceToken :: ParsecT [Token] st IO (ReturnObject)
leftBraceToken = tokenPrim show updatePos get_token where
    get_token (LBrace p) = Just (RetToken (LBrace p))
    get_token _       = Nothing

-- terminal: block closing character
rightBraceToken :: ParsecT [Token] st IO (ReturnObject)
rightBraceToken = tokenPrim show updatePos get_token where
    get_token (RBrace p) = Just (RetToken (RBrace p))
    get_token _       = Nothing
    
leftBracketToken :: ParsecT [Token] st IO (ReturnObject)
leftBracketToken = tokenPrim show updatePos get_token where
    get_token (LBracket p) = Just (RetToken (LBracket p))
    get_token _       = Nothing

rightBracketToken :: ParsecT [Token] st IO (ReturnObject)
rightBracketToken = tokenPrim show updatePos get_token where
    get_token (RBracket p) = Just (RetToken (RBracket p))
    get_token _       = Nothing


-- Left parenthesis
leftParenToken :: ParsecT [Token] st IO (ReturnObject)
leftParenToken = tokenPrim show updatePos get_token where
    get_token (LParen p) = Just (RetToken (LParen p))
    get_token _       = Nothing

-- Right parenthesis
rightParenToken :: ParsecT [Token] st IO (ReturnObject)
rightParenToken = tokenPrim show updatePos get_token where
    get_token (RParen p) = Just (RetToken (RParen p))
    get_token _       = Nothing

plusToken :: ParsecT [Token] st IO (ReturnObject)
plusToken = tokenPrim show updatePos get_token where
    get_token (Plus p) = Just (RetToken (Plus p))
    get_token _       = Nothing


lessThanToken :: ParsecT [Token] st IO (ReturnObject)
lessThanToken = tokenPrim show updatePos get_token where
    get_token (LessThan p)  = Just (RetToken (LessThan p))
    get_token _             = Nothing

greaterThanToken :: ParsecT [Token] st IO (ReturnObject)
greaterThanToken = tokenPrim show updatePos get_token where
    get_token (GreaterThan p)  = Just (RetToken (GreaterThan p))
    get_token _             = Nothing

lessEqualsToken :: ParsecT [Token] st IO (ReturnObject)
lessEqualsToken = tokenPrim show updatePos get_token where
    get_token (LessEquals p)  = Just (RetToken (LessEquals p))
    get_token _ = Nothing

greaterEqualsToken :: ParsecT [Token] st IO (ReturnObject)
greaterEqualsToken = tokenPrim show updatePos get_token where
    get_token (GreaterEquals p)  = Just (RetToken (GreaterEquals p))
    get_token _ = Nothing   

minusToken :: ParsecT [Token] st IO (ReturnObject)
minusToken = tokenPrim show updatePos get_token where
    get_token (Minus p) = Just (RetToken (Minus p))
    get_token _       = Nothing

timesToken :: ParsecT [Token] st IO (ReturnObject)
timesToken = tokenPrim show updatePos get_token where
    get_token (Times p) = Just (RetToken (Times p))
    get_token _       = Nothing

expoToken :: ParsecT [Token] st IO (ReturnObject)
expoToken = tokenPrim show updatePos get_token where
    get_token (Expo p) = Just (RetToken (Expo p))
    get_token _       = Nothing

divToken :: ParsecT [Token] st IO (ReturnObject)
divToken = tokenPrim show updatePos get_token where
    get_token (Div p) = Just (RetToken (Div p))
    get_token _       = Nothing

modToken :: ParsecT [Token] st IO (ReturnObject)
modToken = tokenPrim show updatePos get_token where
    get_token (Mod p) = Just (RetToken (Mod p))
    get_token _       = Nothing


-- terminal: token for a primitive type
lexicalTypeToken :: ParsecT [Token] st IO (ReturnObject)
lexicalTypeToken = tokenPrim show updatePos get_token where
    get_token (Type x p) = Just (RetToken (Type x p))
    get_token _        = Nothing 

-- Identifier (of a variable/function/procedure/etc)
idToken :: ParsecT [Token] st IO (ReturnObject)
idToken = tokenPrim show updatePos get_token where
    get_token (Id x p) = Just (RetToken (Id x p))
    get_token _      = Nothing

-- terminal: assignment symbol
assignToken :: ParsecT [Token] st IO (ReturnObject)
assignToken = tokenPrim show updatePos get_token where
    get_token (Assign p) = Just (RetToken (Assign p))
    get_token _      = Nothing

-- terminal: literal of type int
intToken :: ParsecT [Token] st IO (ReturnObject)
intToken = tokenPrim show updatePos get_token where
    get_token (Int x _) = Just (RetValue (ConsNatInt x))
    get_token _       = Nothing

nullToken :: ParsecT [Token] st IO (ReturnObject)
nullToken = tokenPrim show updatePos get_token where
    get_token (Null _)  = Just (RetValue (ConsNatNull))
    get_token _         = Nothing

stringToken :: ParsecT [Token] st IO (ReturnObject)
stringToken = tokenPrim show updatePos get_token where
    get_token (String x _) = Just (RetValue (ConsNatString x))
    get_token _            = Nothing

-- literal of type double
doubleToken :: ParsecT [Token] st IO (ReturnObject)
doubleToken = tokenPrim show updatePos get_token where
    get_token (Double x p) = Just (RetValue (ConsNatDouble x))
    get_token _       = Nothing

boolToken :: ParsecT [Token] st IO (ReturnObject)
boolToken = tokenPrim show updatePos get_token where
    get_token (Bool x p) = Just (RetValue (ConsNatBool x))
    get_token _ = Nothing     

equalsToken :: ParsecT [Token] st IO (ReturnObject)
equalsToken = tokenPrim show updatePos get_token where
    get_token (Equals p) = Just (RetToken (Equals p))
    get_token _            = Nothing

differentToken :: ParsecT [Token] st IO (ReturnObject)
differentToken = tokenPrim show updatePos get_token where
    get_token (Different p) = Just (RetToken (Different p))
    get_token _ = Nothing

andToken :: ParsecT [Token] st IO (ReturnObject)
andToken = tokenPrim show updatePos get_token where
    get_token (And p) = Just (RetToken (And p))
    get_token _         = Nothing

orToken :: ParsecT [Token] st IO (ReturnObject)
orToken = tokenPrim show updatePos get_token where
    get_token (Or p) = Just (RetToken (Or p))
    get_token _         = Nothing

-- operator ?
inToken :: ParsecT [Token] st IO (ReturnObject)
inToken = tokenPrim show updatePos get_token where
    get_token (In p) = Just (RetToken (In p))
    get_token _         = Nothing

-- terminal: command terminator
_semiColonToken :: ParsecT [Token] st IO (Token)
_semiColonToken = tokenPrim show updatePos get_token where
    get_token (SemiColon p) = Just (SemiColon p)
    get_token _         = Nothing

semiColonToken :: ParsecT [Token] st IO (ReturnObject)
semiColonToken = tokenPrim show updatePos get_token where
    get_token (SemiColon p) = Just (RetToken (SemiColon p))
    get_token _         = Nothing

colonToken :: ParsecT [Token] st IO (ReturnObject)
colonToken = tokenPrim show updatePos get_token where
    get_token (Colon p) = Just (RetToken (Colon p))
    get_token _         = Nothing

negationToken :: ParsecT [Token] st IO (ReturnObject)
negationToken = tokenPrim show updatePos get_token where
    get_token (Negation p)  = Just (RetToken (Negation p))
    get_token _             = Nothing

commaToken :: ParsecT [Token] st IO (ReturnObject)
commaToken = tokenPrim show updatePos get_token where
    get_token (Comma p)  = Just (RetToken (Comma p))
    get_token _ = Nothing
    
dotToken :: ParsecT [Token] st IO (ReturnObject)
dotToken = tokenPrim show updatePos get_token where
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
        let typedefReturnType = getMemoryCellType (memoryGet idName pos memory)

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


_intToken :: ParsecT [Token] st IO (Exp)
_intToken = tokenPrim show updatePos get_token where
    get_token (Int x p) = Just  (  CONSExpLit NatInt (Int x p) )
    get_token _       = Nothing

_doubleToken :: ParsecT [Token] st IO (Exp)
_doubleToken = tokenPrim show updatePos get_token where
    get_token (Double x p) = Just  (  CONSExpLit NatDouble (Double x p) )
    get_token _       = Nothing

_stringToken :: ParsecT [Token] st IO (Exp)
_stringToken = tokenPrim show updatePos get_token where
    get_token (String x p) = Just  (  CONSExpLit NatString (String x p) )
    get_token _       = Nothing

_nullToken :: ParsecT [Token] st IO (Exp)
_nullToken = tokenPrim show updatePos get_token where
    get_token (Null p) = Just  (  CONSExpLit NatNull (Null p) )
    get_token _       = Nothing

_minusUnToken :: ParsecT [Token] st IO (Exp -> Exp)
_minusUnToken = tokenPrim show updatePos get_token where
    get_token (Minus p) = Just  (  CONSExpUn NatNothing (CONSUnOp (Minus p))   )
    get_token _       = Nothing

_uppersandToken :: ParsecT [Token] st IO (Exp -> Exp)
_uppersandToken = tokenPrim show updatePos get_token where
    get_token (Uppersand p) = Just  (  CONSExpUn NatNothing (CONSUnOp (Uppersand p))   )
    get_token _       = Nothing
    
_minusBinToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_minusBinToken = tokenPrim show updatePos get_token where
    get_token (Minus p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Minus p)) )
    get_token _       = Nothing

_plusToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_plusToken = tokenPrim show updatePos get_token where
    get_token (Plus p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Plus p)) )
    get_token _       = Nothing

_timesToken :: ParsecT [Token] st IO (Token)
_timesToken = tokenPrim show updatePos get_token where
    get_token (Times p) = Just  (  Times p )
    get_token _       = Nothing

_timesTokenAsNumOp = 
    do 
        times <- _timesToken
        return $ CONSExpBin NatNothing (CONSBinOp times)

_divToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_divToken = tokenPrim show updatePos get_token where
    get_token (Div p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Div p)) )
    get_token _       = Nothing

_expoToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_expoToken = tokenPrim show updatePos get_token where
    get_token (Expo p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Expo p)) )
    get_token _       = Nothing

_modToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_modToken = tokenPrim show updatePos get_token where
    get_token (Mod p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Mod p)) )
    get_token _       = Nothing


_boolToken :: ParsecT [Token] st IO (Exp)
_boolToken = tokenPrim show updatePos get_token where
    get_token (Bool x p) = Just  (  CONSExpLit NatBool (Bool x p) )
    get_token _       = Nothing

_andToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_andToken = tokenPrim show updatePos get_token where
    get_token (And p) = Just  (  CONSExpBin NatNothing (CONSBinOp (And p)) )
    get_token _       = Nothing

_orToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_orToken = tokenPrim show updatePos get_token where
    get_token (Or p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Or p)) )
    get_token _       = Nothing

_negationToken :: ParsecT [Token] st IO (Exp -> Exp)
_negationToken = tokenPrim show updatePos get_token where
    get_token (Negation p) = Just  (  CONSExpUn NatNothing (CONSUnOp (Negation p))   )
    get_token _       = Nothing

_lessThanToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_lessThanToken = tokenPrim show updatePos get_token where
    get_token (LessThan p) = Just  (  CONSExpBin NatNothing (CONSBinOp (LessThan p)) )
    get_token _       = Nothing

_greaterThanToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_greaterThanToken = tokenPrim show updatePos get_token where
    get_token (GreaterThan p) = Just  (  CONSExpBin NatNothing (CONSBinOp (GreaterThan p)) )
    get_token _       = Nothing

_lessEqualsToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_lessEqualsToken = tokenPrim show updatePos get_token where
    get_token (LessEquals p) = Just  (  CONSExpBin NatNothing (CONSBinOp (LessEquals p)) )
    get_token _       = Nothing

_greaterEqualsToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_greaterEqualsToken = tokenPrim show updatePos get_token where
    get_token (GreaterEquals p) = Just  (  CONSExpBin NatNothing (CONSBinOp (GreaterEquals p)) )
    get_token _       = Nothing

_interrogationToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_interrogationToken = tokenPrim show updatePos get_token where
    get_token (Interrogation p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Interrogation p)) )
    get_token _       = Nothing

_equalsToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_equalsToken = tokenPrim show updatePos get_token where
    get_token (Equals p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Equals p)) )
    get_token _       = Nothing

_differentToken :: ParsecT [Token] st IO (Exp -> Exp -> Exp)
_differentToken = tokenPrim show updatePos get_token where
    get_token (Different p) = Just  (  CONSExpBin NatNothing (CONSBinOp (Different p)) )
    get_token _       = Nothing

_leftParenToken :: ParsecT [Token] st IO (Token)
_leftParenToken = tokenPrim show updatePos get_token where
    get_token (LParen p) = Just  (  LParen p ) 
    get_token _       = Nothing

_rightParenToken :: ParsecT [Token] st IO (Token)
_rightParenToken = tokenPrim show updatePos get_token where
    get_token (RParen p) = Just  (  RParen p ) 
    get_token _       = Nothing

_leftBraceToken :: ParsecT [Token] st IO (Token)
_leftBraceToken = tokenPrim show updatePos get_token where
    get_token (LBrace p) = Just  (  LBrace p ) 
    get_token _       = Nothing

_rightBraceToken :: ParsecT [Token] st IO (Token)
_rightBraceToken = tokenPrim show updatePos get_token where
    get_token (RBrace p) = Just  (  RBrace p ) 
    get_token _       = Nothing

_leftBracketToken :: ParsecT [Token] st IO (Token)
_leftBracketToken = tokenPrim show updatePos get_token where
    get_token (LBracket p) = Just  (  LBracket p ) 
    get_token _       = Nothing

_rightBracketToken :: ParsecT [Token] st IO (Token)
_rightBracketToken = tokenPrim show updatePos get_token where
    get_token (RBracket p) = Just  (  RBracket p ) 
    get_token _       = Nothing

_assignToken :: ParsecT [Token] st IO (Token)
_assignToken = tokenPrim show updatePos get_token where
    get_token (Assign p) = Just  (  Assign p ) 
    get_token _       = Nothing

_idToken :: ParsecT [Token] st IO (Token)
_idToken = tokenPrim show updatePos get_token where
    get_token (Id x p) = Just  ( Id x p ) 
    get_token _       = Nothing

_dotToken :: ParsecT [Token] st IO (Token)
_dotToken = tokenPrim show updatePos get_token where
    get_token (Dot p) = Just  ( Dot p ) 
    get_token _       = Nothing

_commaToken :: ParsecT [Token] st IO (Token)
_commaToken = tokenPrim show updatePos get_token where
    get_token (Comma p) = Just  ( Comma p ) 
    get_token _       = Nothing

_toDoubleToken :: ParsecT [Token] st IO (Token)
_toDoubleToken = tokenPrim show updatePos get_token where
    get_token (ToDouble p) = Just  ( ToDouble p ) 
    get_token _       = Nothing

_toIntToken :: ParsecT [Token] st IO (Token)
_toIntToken = tokenPrim show updatePos get_token where
    get_token (ToInt p) = Just  ( ToInt p ) 
    get_token _       = Nothing

_toStringToken :: ParsecT [Token] st IO (Token)
_toStringToken = tokenPrim show updatePos get_token where
    get_token (ToString p) = Just  ( ToString p ) 
    get_token _       = Nothing

_toBoolToken :: ParsecT [Token] st IO (Token)
_toBoolToken = tokenPrim show updatePos get_token where
    get_token (ToBool p) = Just  ( ToBool p ) 
    get_token _       = Nothing

_readToken :: ParsecT [Token] st IO (Token)
_readToken = tokenPrim show updatePos get_token where
    get_token (Read p) = Just  ( Read p ) 
    get_token _       = Nothing

_lexicalTypeToken :: ParsecT [Token] st IO (Token)
_lexicalTypeToken = tokenPrim show updatePos get_token where
    get_token (Type x p) = Just (Type x p)
    get_token _        = Nothing 

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos _ (tok:_) = pos -- necessita melhoria
updatePos pos _ []      = pos  
