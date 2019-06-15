module Blocks.Blocks where

-- natalia's modules
import Lexical.Lexemes
import Lexical.Tokens
import Memory.Memory
import Statements.Statements
import Types.Typedef
import TypeValue.TypeValue
import Syntax.SubprogramBody

-- Haskell's modules
import Control.Monad.IO.Class
import Data.Bits
import Text.Parsec

predefinedBlocks :: Value -> ParsecT [Token] [MemoryCell] IO ()
predefinedBlocks currentBlocks =
    try
    (do
        retPredefinedBlock <- predefinedBlock currentBlocks
        let res = getRetValue retPredefinedBlock
        retPredefinedBlocks <- predefinedBlocks (ConsNatInt (getIntFromNatInt res))
        return ()
    )
    <|>
    (do
        -- let valueCurrentBlocks = getIntFromNatInt currentBlocks

        -- if(valueCurrentBlocks .&. 1 == 0) then error ("Your program must have a @main block!")
        --else return()
        return ())


predefinedBlock :: Value -> ParsecT [Token] [MemoryCell] IO (ReturnObject)
predefinedBlock currentBlocks =
    try
    -- (do
    --     retCurrentBlock <- mainBlock
    --     let valueCurrentBlocks = getIntFromNatInt currentBlocks

    --     if valueCurrentBlocks .&. 1 == 1 then error ("You can't have more than one @main block!")
    --     else 
    --         do
    --             let natInt = ConsNatInt (valueCurrentBlocks .|. 1) 
    --             return (RetValue natInt))
    -- <|>
    (do
        retCurrentBlock <- typedefsBlock
        let valueCurrentBlocks = getIntFromNatInt currentBlocks
        

        if valueCurrentBlocks .&. 2 == 2 then error ("You can't have more than one @typedefs block!")
        else 
            do
                let natInt = ConsNatInt (valueCurrentBlocks .|. 2) 
                return (RetValue natInt))
    <|>
    (do
        retCurrentBlock <- subprogramsBlock
        let valueCurrentBlocks = getIntFromNatInt currentBlocks
        --liftIO(print(show(valueCurrentBlocks)))

        if valueCurrentBlocks .&. 4 == 4 then error ("You can't have more than one @subprograms block!")
        else 
            do
                let natInt = ConsNatInt (valueCurrentBlocks .|. 4) 
                return (RetValue natInt))
    <|>
    (do
        retCurrentBlock <- globalsBlock
        let valueCurrentBlocks = getIntFromNatInt currentBlocks
        -- liftIO(print(show(valueCurrentBlocks)))

        if valueCurrentBlocks .&. 8 == 8 then error ("You can't have more than one @globals block!")
        else 
            do
                let natInt = ConsNatInt (valueCurrentBlocks .|. 8) 
                return (RetValue natInt))


mainBlock :: ParsecT [Token] [MemoryCell] IO ()
mainBlock = 
    do
        a <- mainToken
        b <- leftBraceToken
        c <- statements
        d <- rightBraceToken
        return ()

subprogramsBlock :: ParsecT [Token] [MemoryCell] IO ()
subprogramsBlock = 
    do
        a <- subprogramsToken
        b <- leftBraceToken
        retNothing <- subprograms
        d <- rightBraceToken
        return ()

globalsBlock :: ParsecT [Token] [MemoryCell] IO ()
globalsBlock = 
    do
        a <- globalsToken
        b <- leftBraceToken
        d <- rightBraceToken
        return ()

typedefsBlock :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
typedefsBlock =
    do
        retTypeDefToken <- typedefsToken
        retleftbrace <- leftBraceToken
        retnothing <- typedefs 
        retrightbrace <- rightBraceToken
        return (RetNothing)


subprograms :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
subprograms =
    try
    (do
        subprogramDef <- subprogramDefinition
        retRemainingSubprogramDef <- subprograms
        return (RetNothing))
    <|>
    (do
        return (RetNothing))

subprogramDefinition :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
subprogramDefinition = try procDef <|> funcDef

funcDef :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
funcDef = 
    (do
        funcRetToken <- funcToken
        funcNameRetToken <- idToken
        lParenRetToken <- leftParenToken
        retParamList <- getParams []
        
        let funcName = get_id_name (getRetToken funcRetToken)
        let paramList = getRetParamList retParamList

        retColon <- colonToken
        retReturnType <- generalType

        let returnType = getRetType retReturnType

        updateState(memoryInsert (SubprogramProtocol (ConstructFunctionProtocol funcName paramList returnType)))

        retBlock <- subprogramBodyParser

        updateState(memoryInsert (ConstructFunction funcName paramList (getRetBlock retBlock) returnType))

        return (RetNothing))


procDef :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
procDef = 
    (do
        procRetToken <- procToken
        procNameRetToken <- idToken
        lParenRetToken <- leftParenToken
        retParamList <- getParams []
        
        let procName = get_id_name (getRetToken procRetToken)
        let paramList = getRetParamList retParamList
        updateState(memoryInsert (ConstructProcedureProtocol procName paramList))

        retBlock <- subprogramBodyParser

        updateState(memoryInsert (ConstructProcedure procName paramList (getRetBlock retBlock)))

        return (RetNothing))


getParams :: [Parameter] -> ParsecT [Token] [MemoryCell] IO (ReturnObject)
getParams [] =
    try
    (do
        paramTypeRetType <- generalType
        paramIdRetToken <- idToken
        let paramId = get_id_name (getRetToken paramIdRetToken)
        let paramType = getRetType paramTypeRetType
        retParamList <- getParams ([ConsParameter paramType paramId])
        return (retParamList))
    <|>
    (do
        return (RetParamList []))

getParams paramList =
    try
    (do
        commaRetToken <- commaToken
        paramTypeRetType <- generalType
        paramIdRetToken <- idToken
        let paramId = get_id_name (getRetToken paramIdRetToken)
        let paramType = getRetType paramTypeRetType
        retParamList <- getParams (paramList ++ [ConsParameter paramType paramId])
        return (retParamList))
    <|>
    (do
        return paramList)

-- receives name of subprogram, list of parameters, return type and list of tokens (the body of the function)
-- getBody :: String -> [Parameter] -> Type -> [Token] -> ParsecT [Token] [MemoryCell] IO (ReturnObject)
-- getBody procName paramList retType body =
--     try 
--     (do
--         )
--     <|>
--     (do
--         rBraceRetToken <- rightBraceToken
--         s <- getState
--         if (retType == NatNull) then 
--             do
--                 updateState(memoryInsert (Subprogram (ConstructProcedure procName paramList body))))
--         else 
--             do
--                 updateState(memoryInsert (Subprogram (ConstructFunction procName paramList retType body))))


typedefs :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
typedefs = 
    try
    (do
        rettypedef <- typeDef 
        retremainingtypedefs <- remainingTypeDefs
        return (RetNothing))
    <|>
    (do
        return (RetNothing))

remainingTypeDefs :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
remainingTypeDefs = 
    try
    (do
        retcomma <- commaToken
        rettypedef <- typeDef 
        retremainingtypedefs <- remainingTypeDefs
        return (RetNothing)
        )
    <|>
    (do
        return (RetNothing))

typeDef :: ParsecT [Token] [MemoryCell] IO (ReturnObject)        
typeDef = try typeAliasDef <|> structTypeDef

typeAliasDef :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
typeAliasDef = 
    do
        rettypedefname <- idToken -- RetToken
        let typedefname = get_id_name (getRetToken rettypedefname) -- String 
        rettype <- generalType -- RetType
        let typeRead = getRetType rettype -- Type
        updateState (memoryInsert (Typedef (ConsTypedef typedefname typeRead)) )
        return (RetNothing)

structTypeDef :: ParsecT [Token] [MemoryCell] IO (ReturnObject)        
structTypeDef = 
    do
        retStructName <- idToken -- RetToken
        let structName = get_id_name (getRetToken retStructName) -- String 
        retLeftBrace <- leftBraceToken -- RetToken
        retStructInits <- structInits structName []-- RetStructStructure 
        retRightBrace <- rightBraceToken -- RetToken
        let allStructInits = getRetStructStructure retStructInits -- [(Type, String)]
        updateState (memoryInsert (Typedef (StructDef structName allStructInits)  ))
        
        return (RetNothing)

structInits :: String -> [(Type, String)] -> ParsecT [Token] [MemoryCell] IO (ReturnObject)        
structInits structName inits = 
    try
    (do
        init <- structInit structName -- RetStructStructure
        let actualInit = getRetStructStructure init -- [(Type, String)]
        retAllInits <- structInits structName (inits ++ actualInit) -- RetStructStructure
        let allInits = getRetStructStructure retAllInits -- [(Type, String)]
        return (RetStructStructure allInits))
    <|>
    (do
        return (RetStructStructure inits))

structInit :: String -> ParsecT [Token] [MemoryCell] IO (ReturnObject)
structInit structName = 
    try 
    (do -- when type of field is recursive
        retFieldType <- idToken
        retFieldName <- idToken
        retSemiColon <- semiColonToken
        let fieldType = get_id_name (getRetToken retFieldType)
        let fieldName = get_id_name (getRetToken retFieldName)
        if fieldType == structName then 
            do 
                return (RetStructStructure [(NatStruct structName, fieldName)])
        else fail ("ERROR type " ++ fieldType ++ " at "++ show(get_pos(getRetToken retFieldType)) ++" is not defined"))
    <|>
    (do -- when type of field is not recursive
        retType <- generalType
        retId <- idToken
        retSemiColon <- semiColonToken
        
        let actualType = getRetType retType 
        let actualName = get_id_name (getRetToken retId)

        return ( RetStructStructure [ (actualType, actualName)  ]  ))


-- blockOfCommandsWithoutReturn :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
-- blockOfCommandsWithoutReturn =
--     do
--         retLeftBrace <- leftBraceToken
--         c <- statements
--         retRightBrace <- rightBraceToken

-- natIf :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
-- natIf 
