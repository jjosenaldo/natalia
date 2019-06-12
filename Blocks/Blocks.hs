module Blocks.Blocks where

-- natalia's modules
import Lexical.Lexemes
import Lexical.Tokens
import Memory.Memory
import Statements.Statements
import Types.Typedef
import TypeValue.TypeValue

-- Haskell's modules
import Control.Monad.IO.Class
import Data.Bits
import Text.Parsec

predefinedBlocks :: Value -> ParsecT [Token] [MemoryCell] IO ()
predefinedBlocks currentBlocks =
    try
    (do
        retPredefinedBlock <- predefinedBlock currentBlocks
        let valueCurrentBlocks = getIntFromNatInt (getRetValue retPredefinedBlock)

        if(valueCurrentBlocks .&. 1 == 1) then
            do
                return ()
        else
            do
                retPredefinedBlocks <- predefinedBlocks (ConsNatInt valueCurrentBlocks)
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
    (do
        retCurrentBlock <- mainBlock
        let valueCurrentBlocks = getIntFromNatInt currentBlocks

        if valueCurrentBlocks .&. 1 == 1 then error ("You can't have more than one @main block!")
        else 
            do
                let natInt = ConsNatInt (valueCurrentBlocks .|. 1) 
                return (RetValue natInt))
    <|>
    (do
        retCurrentBlock <- typedefsBlock
        let valueCurrentBlocks = getIntFromNatInt currentBlocks
        --liftIO(print(show(valueCurrentBlocks)))

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
typeDef = typeAliasDef -- <|> structTypeDef

typeAliasDef :: ParsecT [Token] [MemoryCell] IO (ReturnObject)
typeAliasDef = 
    do
        rettypedefname <- id_token -- RetToken
        let typedefname = get_id_name (getRetToken rettypedefname) -- String 
        rettype <- generalType -- RetType
        let typeRead = getRetType rettype -- Type
        updateState (memory_insert (Typedef (ConsTypedef typedefname typeRead)) )
        return (RetNothing)