module Types.Typedef where

-- natalia's modules
import TypeValue.TypeValue

data Typedef = 
    ConsTypedef String Type  |
    StructDef String [(Type, String)] deriving (Show, Eq)

getTypedefType :: Typedef -> Type
getTypedefType (ConsTypedef _ x) = x
getTypedefType (StructDef x _) = NatStruct x

getTypedefName :: Typedef -> String
getTypedefName (ConsTypedef x _ ) = x

getStructStructure :: Typedef -> [(Type, String)]
getStructStructure (StructDef _ x) = x

getStructName :: Typedef -> String
getStructName (StructDef x _) = x