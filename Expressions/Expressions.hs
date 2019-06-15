module Expressions.Expressions where

-- natalia's modules
import Expressions.Evaluation
import Expressions.Operations
import Lexical.Lexemes
import Lexical.Tokens
import Memory.Memory
import Types.Types
import Types.Typedef
import TypeValue.TypeValue

-- Haskell's modules
import Text.Parsec
import Control.Monad.IO.Class

-- General expression
expression :: ParsecT [Token] [MemoryCell] IO(ReturnObject)     
expression = expGroup9

expGroup9 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
expGroup9 =
    try
    (do
        -- this group contains only expression of attribution
        l <- lValue -- RetMemoryCell
        op <- group9OpToken
        r <- expGroup9
        let var = getRetMemoryCell l
        let val = getRetValue r
        updateState(memoryUpdate (setValue var val))
        return (r))
    <|>
    expGroup8

expGroup8 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
expGroup8 =
    (do
        -- this group contains only the operation ||
        l <- expGroup7
        result <- evalRemainingGroup8 (l)
        return (result))

evalRemainingGroup8 :: ReturnObject -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
evalRemainingGroup8 l =
    try
    (do
        op <- group8OpToken
        r <- expGroup7
        result <- evalRemainingGroup8 (RetValue (binaryEval (getRetValue l) (getRetToken op) (getRetValue r)))
        return (result))
    <|>
    (do
        return (l))

expGroup7 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
expGroup7 =
    (do
        -- this group contains only the operation &&
        l <- expGroup6
        result <- evalRemainingGroup7 (l)
        return (result))

evalRemainingGroup7 :: ReturnObject -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
evalRemainingGroup7 l =
    try
    (do
        op <- group7OpToken
        r <- expGroup6
        result <- evalRemainingGroup7 (RetValue (binaryEval (getRetValue l) (getRetToken op) (getRetValue r)))
        return (result))
    <|>
    (do
        return (l))

expGroup6 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
expGroup6 =
    (do
        -- this group contains only the operation == (potentially will contain !=)
        l <- expGroup5
        result <- evalRemainingGroup6 (l)
        return (result))

evalRemainingGroup6 :: ReturnObject -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
evalRemainingGroup6 l =
    try
    (do
        op <- group6OpToken
        r <- expGroup5
        result <- evalRemainingGroup6 (RetValue (binaryEval (getRetValue l) (getRetToken op) (getRetValue r)))
        return (result))
    <|>
    (do
        return (l))

expGroup5 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
expGroup5 =
    (do
        -- this group contains only the operation < (potentially will contain >, <=, >=)
        l <- expGroup4
        result <- evalRemainingGroup5 l
        return (result))

evalRemainingGroup5 :: ReturnObject -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
evalRemainingGroup5 l =
    try
    (do
        op <- group5OpToken
        r <- expGroup4
        result <- evalRemainingGroup5 (RetValue (binaryEval (getRetValue l) (getRetToken op) (getRetValue r)))
        return (result))
    <|>
    (do
        return (l))

expGroup4 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
expGroup4 =
    try
        (do
            -- this group contains only operation **
            l <- expGroup3 -- RetValue
            op <- group4OpToken -- RetToken Op
            r <- expGroup4 -- RetValue
            return (RetValue (binaryEval (getRetValue l) (getRetToken op) (getRetValue r))))
        <|>
        expGroup3

expGroup3 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
expGroup3 =
    (do
        -- this group contains only the binary operations + and -
        l <- expGroup2 -- RetValue
        result <- evalRemainingGroup3 (l)
        return (result))

evalRemainingGroup3 :: ReturnObject -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
evalRemainingGroup3 l =
    try
    (do
        op <- group3OpToken
        r <- expGroup2
        result <- evalRemainingGroup3 (RetValue (binaryEval (getRetValue l) (getRetToken op) (getRetValue r)))
        return (result))
    <|>
    (do
        return (l))

expGroup2 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
expGroup2 =
    (do
        -- this group contains only the binary operations *, / and %
        l <- expGroup1
        result <- evalRemainingGroup2 (l)
        return (result))

evalRemainingGroup2 :: ReturnObject -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
evalRemainingGroup2 l =
    try
    (do
        op <- group2OpToken -- RetToken
        r <- expGroup1
        result <- evalRemainingGroup2 (RetValue (binaryEval (getRetValue l) (getRetToken op) (getRetValue r)))
        return (result))
    <|>
    (do
        return (l))

expGroup1 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
expGroup1 =
    try
    (do
        -- this group contains only operations !, unary -, reference access (&) and value access (*) 
        op <- group1OpToken -- RetToken Op
        a <- expGroup1 -- RetValue
        return (RetValue (unaryEval (getRetToken op) (getRetValue a))))
    <|>
    (do 
        a <- expGroup0
        return (a))

expGroup0 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
expGroup0 = 
    try structFieldRead
    <|>
    try structValue 
    <|> 
    memoryAccess 
    <|> 
    expSet 
    <|> 
    expArray 
    <|> 
    boolToken 
    <|> 
    intToken
    <|> 
    doubleToken 
    <|>
    nullToken
    <|> 
    stringToken 
    <|> 
    localVariable 
    <|> 
    expParenthesized 

-- STRUCTS -----------------------------------------------------------------------------------------------------------------------------

structValue :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
structValue =
    do
        memory <- getState -- [MemoryCell]
        retId <- id_token -- RetToken 
        -- liftIO(print(show(getRetToken retId)))
        -- liftIO(print("structValue"))
        retLeftBrace <- leftBraceToken


        let id = getRetToken retId -- Id
        let typedefStructStructure = memoryGet (get_id_name id) (get_pos id) memory -- Typedef (StructDef String [(Type, String)])
        let structDefStructure = getMemoryCellType typedefStructStructure -- StructDef String [(Type, String)]
        let structure = getStructStructure structDefStructure -- [(Type, String)]
        
        retStructValues <- structValues structure -- RetStructValues 
        retRightBrace <- rightBraceToken
        liftIO(print(">>retstructvalues" ++ show(retStructValues)))

        return (    RetValue ( ConsNatStruct (get_id_name id) (getRetStructValues retStructValues))         )

structValues :: [(Type, String)] -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
structValues [] = 
    do 
        return (RetStructValues [])

structValues (field:fields) = 
    do
        retValue <- expression
        let expressionValue = getRetValue retValue -- Value
        let typeOfExpression = getTypeFromValue expressionValue
        let expectedType = fst field

        if (not (checkCompatibleTypes expectedType typeOfExpression)) then error ("ERROR: type mismatch in the initialization of a struct field. Expected: " ++ show(expectedType) ++", got: " ++ show(typeOfExpression))
        else
            do
                allValues <- remainingStructValues [(snd field, expressionValue)] fields
                return (RetStructValues(getRetStructValues allValues))

remainingStructValues :: [(String, Value)] -> [(Type, String)] -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
remainingStructValues inValueList [] = 
    do
        return (RetStructValues inValueList)

remainingStructValues inValueList (field:fields) = 
    do
        -- liftIO(print(show(inValueList)))
        -- liftIO(print(show(field:fields)))
        retComma <- commaToken
        retValue <- expression
        let expressionValue = getRetValue retValue -- Value
        --liftIO(print(show(expressionValue)))
        let typeOfExpression = getTypeFromValue expressionValue
        let expectedType = fst field

        if (not (checkCompatibleTypes expectedType typeOfExpression)) then error ("ERROR: type mismatch in the initialization of a struct field. Expected: " ++ show(expectedType) ++", got: " ++ show(typeOfExpression))
        else
            do
                allValues <- remainingStructValues (inValueList ++ [(snd field, expressionValue)]) fields
                return (RetStructValues (getRetStructValues allValues))

structFieldRead :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
structFieldRead = 
    do
        retId <- id_token
        -- liftIO(print((get_id_name.getRetToken) retId))
        -- liftIO(print("structFieldRead"))
        retDot <- dotToken
        retField <- id_token
        state <- getState
        let id = (get_id_name.getRetToken) retId
        let pos = get_pos (getRetToken retId)
        let values = getStructValues (getValue (memoryGet id pos state) pos)
        let value = getStructFieldValue (  (get_id_name.getRetToken) retField  ) values -- Value

        return (RetValue value)

--structRemainingFields :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
--structRemainingFields 


-- ARRAYS --------------------------------------------------------------------------------------------------------------------------------

editArray :: MemoryCell -> [Value] -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
editArray arr list =
    try
    (do
        retlbracket <- leftBracketToken
        exprRetVal <- expression
        retrbracket <- rightBracketToken
        let exprVal = getRetValue exprRetVal
        if (checkCompatibleTypes NatInt (getTypeFromValue exprVal)) then 
            do
                newarr <- editArray arr (list++[exprVal])
                return (newarr)
        else error "ERROR index of array must be an integer")
    <|>
    (do
        assignRetToken <- assignToken
        exprRetVal <- expression
        let exprVal = getRetValue exprRetVal
        updateState(memoryUpdate (setValueArray arr list exprVal))
        return (RetValue exprVal)
    )



-- Assignment of a value to a variable
var_attribution :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
var_attribution = 
    try
    -- when the attribution is of type arr[p1][p2] = expr
    (do
        mem <- getState
        nameRetToken <- id_token -- RetToken
        let name = get_id_name (getRetToken nameRetToken) -- "arr"
        let var = memoryGet name (get_pos (getRetToken nameRetToken)) mem
        res <- editArray var [] -- RetValue
        return (res))
    <|>
    -- when the attribution is 'simple': a = expr
    (do
    a <- id_token -- RetToken
    --liftIO(print(show(getRetToken a)))
    b <- assignToken -- RetToken
    expr_val <- expression -- RetValue
    s <- getState -- [MemoryCell]
    let pos = get_pos (getRetToken a)
    let var = memoryGet (get_id_name (getRetToken a)) pos s --MemoryCell
    let var_type = getTypeFromValue (getValue var pos)
    let expr_type = getTypeFromValue (getRetValue expr_val)
    if (not (checkCompatibleTypes var_type expr_type)) then error ("ERROR at " ++ show(get_pos (getRetToken a))  ++ ": type mismatch in the attribution of a value to a variable.")
    else
        do
            let updatedVar = setValue var (getRetValue expr_val)
            updateState(memoryUpdate updatedVar)
            
            -- optional: print symbols_table content
            s <- getState
            --liftIO (print s)
            return (expr_val))

-- Parenthesized expression
expParenthesized :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
expParenthesized = 
    do
        lparen <- left_paren_token
        expr <- expression
        rparen <- right_paren_token
        return (expr)

        -- Expression that consists of a local variable

memoryAccess :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
memoryAccess =
    (do -- array variable
        mem <- getState
        id <- id_token -- RetToken
        if (memoryHasName (get_id_name (getRetToken id)) mem) then 
            do 
                let pos = get_pos (getRetToken id)
                let name = (get_id_name (getRetToken id))
                --liftIO(print(">>>getValue de: " ++ show(memoryGet name pos mem)))
                res <- getChainedAccess (getValue (memoryGet name pos mem) pos)
                return res

        else error ("ERROR " ++ (get_id_name (getRetToken id)) ++ " is not accessible"))

localVariable :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
localVariable = 
    do -- simple variable
        liftIO(print("trying to read a local variable"))
        mem <- getState
        id <- id_token
        liftIO(print(id))
        let name = (get_id_name.getRetToken) id
        let pos = get_pos (getRetToken id)
        if (memoryHasName name mem) then do return (RetValue (getValue (memoryGet name pos mem) pos))
        else error ("ERROR " ++ name ++ " is not accessible")

getChainedAccess :: Value -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
getChainedAccess var =
    try
    (do
        mem <- getState
        retlbracket <- leftBracketToken
        exprRetVal <- expression
        retrbracket <- rightBracketToken
        let exprVal = getRetValue exprRetVal
        if (checkCompatibleTypes NatInt (getTypeFromValue exprVal)) then 
            do 
                res <- getChainedAccess (arrayAccess var (getRetValue exprRetVal))
                return res
        else error ("ERROR value inside [] operator has to be an integer")
        )
    <|>
    (return (RetValue var))


parseSetElements :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
parseSetElements = 
    try
    (do
        ret_value <- expression -- RetValue Value
        let value = getRetValue ret_value -- Value
        let current_type = getTypeFromValue value -- Type
        result_value <- parseNextSetElement [value] current_type
        return (result_value))
    <|>
    (do
        return (RetValue(ConsNatSet NatGenType [])))


parseNextSetElement :: [Value] -> Type -> ParsecT [Token] [MemoryCell] IO (ReturnObject)
parseNextSetElement elements lastType =
    try
    (do
        ret_comma <- commaToken -- RetToken Comma
        ret_value <- expression -- RetValue Value
        let value = getRetValue ret_value -- Value
        let current_type = getTypeFromValue value -- Type

        if(checkCompatibleTypes current_type lastType ) then 
            do
                ret <- parseNextSetElement (elements ++ [value]) current_type
                return (ret) 
        else if (checkCompatibleTypes lastType current_type  ) then
            do
                ret <- parseNextSetElement (elements ++ [value]) lastType
                return (ret) 
        else 
            fail ("ERROR: " ++ show(current_type) ++ " was provided when " ++ show(lastType) ++ " was expected." ))
    <|>
    (do
        return (RetValue(ConsNatSet lastType elements)))

parseArrayElements :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
parseArrayElements = 
    try
    (do
        ret_value <- expression -- RetValue Value
        let value = getRetValue ret_value -- Value
        let current_type = getTypeFromValue value -- Type
        result_value <- parseNextArrayElement [value] current_type
        return (result_value))
    <|>
    (do
        return (RetValue(ConsNatArray NatGenType [])))

parseNextArrayElement :: [Value] -> Type -> ParsecT [Token] [MemoryCell] IO (ReturnObject)
parseNextArrayElement elements lastType =
    try
    (do
        ret_comma <- commaToken -- RetToken Comma
        ret_value <- expression -- RetValue Value
        let value = getRetValue ret_value -- Value
        let current_type = getTypeFromValue value -- Type

        if(checkCompatibleTypes current_type lastType ) then 
            do
                ret <- parseNextArrayElement (elements ++ [value]) current_type
                return (ret) 
        else if (checkCompatibleTypes lastType current_type  ) then
            do
                ret <- parseNextArrayElement (elements ++ [value]) lastType
                return (ret) 
        else 
            fail ("ERROR: " ++ show(current_type) ++ " was provided when " ++ show(lastType) ++ " was expected." ))
    <|>
    (do
        return (RetValue(ConsNatArray lastType elements)))

expSet :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
expSet = 
    do 
        retlbrace <- leftBraceToken -- RetToken LBrace
        ret_value <- parseSetElements -- RetValue NatSet Type
        retrbrace <- rightBraceToken -- RetToken RBrace
        let actual_value =  getRetValue ret_value -- Value

        return(RetValue actual_value)

expArray :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
expArray = 
    do 
        retlbracket <- leftBracketToken -- RetToken LBrace
        ret_value <- parseArrayElements -- RetValue NatArray Type
        retlbracket <- rightBracketToken -- RetToken RBrace
        let actual_value =  getRetValue ret_value -- Value

        return(RetValue actual_value)


lValue :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
lValue = 
    do
        mem <- getState -- [MemoryCell]
        name <- id_token -- RetToken Id
        let nameRetToken = getRetToken name -- Token Id
        let var = memoryGet (get_id_name nameRetToken) (get_pos nameRetToken) mem -- Variable
        if (not (isVariable var)) then fail ("ERROR name INSERT NAME HERE LATER doesn't correspond to a variable")
        else do return (RetMemoryCell var)
