module Blocks.Blocks where

import Lexical.Lexemes
import Lexical.Tokens
import Memory.Memory
import Types.Typedef

import Text.Parsec

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