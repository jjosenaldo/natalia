module Types.Typedef where

-- natalia's modules
import TypeValue.TypeValue

data Typedef = 
    ConsTypedef String Type  |
    StructDef String [(Type, String)] deriving (Show, Eq)

getTypedefType :: Typedef -> Type
getTypedefType (ConsTypedef _ x) = x

getTypedefName :: Typedef -> String
getTypedefName (ConsTypedef x _ ) = x