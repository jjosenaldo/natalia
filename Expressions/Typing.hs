module Expressions.Typing where 

-- natalia's modules
import Expressions.Grammar
import Expressions.Operations
import Lexical.Lexemes    
import Memory.Memory
import TypeValue.TypeValue

-- Haskell modules




setExpType :: [MemoryCell] -> Exp -> Exp

setExpType _ (CONSExpLit t tok ) = 
    CONSExpLit t tok 

setExpType memory (CONSExpBin _ binOp exp1 exp2) = 
    CONSExpBin t binOp retExp1 retExp2
    where 
        retExp1 = setExpType memory exp1
        retExp2 = setExpType memory exp2
        t1 = getExpType retExp1
        t2 = getExpType retExp1
        t = getReturnTypeOfBinOp binOp t1 t2

setExpType memory (CONSExpUn _ unOp exp1) = 
    CONSExpUn t unOp retExp1
    where 
        retExp1 = setExpType memory exp1 
        t1 = getExpType retExp1
        t = getReturnTypeOfUnOp unOp t1

setExpType memory (CONSExpAssign _ lval exp2) 
    | checkCompatibleTypes t1 t2 = CONSExpAssign t2 lval newExp2
    | otherwise = error ("ERROR: you cant assign " ++ show(t2) ++ " to "  ++ show(t1))
    
    where
        t1 = getLValueType memory lval 
        newExp2 = setExpType memory exp2
        t2 = getExpType newExp2
    
setExpType memory (CONSExpLValue _ lval) =
    CONSExpLValue (getLValueType memory lval) lval

setExpType memory (CONSExpStruct _ name exps) 
    | elem False resultList = error ("ERROR: type error in the struct fields")
    | otherwise = (CONSExpStruct (NatStruct name) name newExps)

    where
        structure = getStructStructure memory name -- [(Type, String)] 
        structTypes = map fst structure -- [Type]
        newExps = map (setExpType memory) exps -- [Exp]
        newExpsTypes = map getExpType exps -- [Type]
        resultList = applyBinFunctionInTwoLists checkCompatibleTypes structTypes newExpsTypes -- [Bool]

setExpType memory (CONSExpSet _ exps) = 
    CONSExpSet (bestType NatGenType newExpsTypes) newExps 
    where 
        newExps = map (setExpType memory) exps -- [Exp]
        newExpsTypes = map getExpType exps -- [Type]

        
setExpType memory (CONSExpFuncCall _ name exps) 
    | elem False resultList = error ("ERROR: type error in the parameter passing to " ++ name) 
    | otherwise = CONSExpFuncCall (snd prot) name newExps
    where 
        newExps = map (setExpType memory) exps -- [Exp]
        newExpsTypes = map getExpType exps -- [Type]
        prot = getFunctionProtocol memory name -- ([Type], Type)
        protTypes = fst prot -- [Type]
        resultList = applyBinFunctionInTwoLists checkCompatibleTypes protTypes newExpsTypes -- [Bool]

setExpType memory (CONSExpCmdZero _ (Read p)) = 
    CONSExpCmdZero NatString (Read p)

setExpType memory (CONSExpCmdUn _ (ToString p) exp1) = 
    CONSExpCmdUn NatString (ToString p) (setExpType memory exp1 )

setExpType memory (CONSExpCmdUn _ (ToDouble p) exp1) 
    | (newExpType == NatDouble) || (newExpType == NatString) || (newExpType == NatInt) = CONSExpCmdUn newExpType (ToDouble p) newExp
    | otherwise = error ("ERROR: the toDouble() function expects a " ++ getTypeName(NatDouble) ++ ", a " ++ (getTypeName NatInt) ++ " or a " ++ (getTypeName NatString ) ++ ", but a " ++ (getTypeName newExpType) ++ " was provided.")

    where 
        newExp = setExpType memory exp1 
        newExpType = getExpType newExp

setExpType memory (CONSExpCmdUn _ (ToInt p) exp1) 
    | (newExpType == NatString) || (newExpType == NatInt) = CONSExpCmdUn newExpType (ToInt p) newExp
    | otherwise = error ("ERROR: the toInt() function expects a " ++ (getTypeName NatInt) ++ " or a " ++ (getTypeName NatString ) ++ ", but a " ++ (getTypeName newExpType) ++ " was provided.")

    where 
        newExp = setExpType memory exp1 
        newExpType = getExpType newExp

setExpType memory (CONSExpCmdUn _ (ToBool p) exp1)     
    | (newExpType == NatString) || (newExpType == NatBool) = CONSExpCmdUn newExpType (ToBool p) newExp 
    | otherwise = error ("ERROR: the toBool() function expects a " ++ (getTypeName NatBool) ++ " or a " ++ (getTypeName NatString ) ++ ", but a " ++ (getTypeName newExpType) ++ " was provided.")
    | otherwise = error ("ERROR: this error shouldn't have appeared.")

    where 
        newExp = setExpType memory exp1 
        newExpType = getExpType newExp

-- alias to getNameOfType
getTypeName = getNameOfType 

-- TODO
getFunctionProtocol :: [MemoryCell] -> String -> ([Type], Type)
getFunctionProtocol _ _ = ([], NatNothing)

bestType :: Type -> [Type] -> Type 
bestType t [] = t 
bestType t1 (t2 : ts)
    | checkCompatibleTypes t1 t2 = bestType t2 ts
    | otherwise = error ("ERROR: your set is not homogeneous")

applyBinFunctionInTwoLists :: (u -> v -> w) -> [u] -> [v] -> [w]
applyBinFunctionInTwoLists _ (x:xs) [] = error ("lists with different sizes")
applyBinFunctionInTwoLists _ [] (y:ys) = error ("lists with different sizes")
applyBinFunctionInTwoLists f [] [] = []
applyBinFunctionInTwoLists f (x:xs) (y:ys) = 
    (f x y) : (applyBinFunctionInTwoLists  f xs ys)

-- TODO
getExpType :: Exp -> Type
getExpType _ = NatNothing

-- TODO
checkCompatibleTypes :: Type -> Type -> Bool 
checkCompatibleTypes _ _ = False

-- TODO
getVarTypeInMemory :: [MemoryCell] -> String -> Type 
getVarTypeInMemory _ _ = NatNothing

getStructFieldType :: [MemoryCell] -> String -> [String] -> Type 
getStructFieldType memory name [] = 
    NatStruct name

getStructFieldType memory name (field : fields) 
    | fields == [] = actualFieldType
    | otherwise = getStructFieldType memory field fields
    
    where 
        structure = getStructStructure memory name
        actualFieldType = keyFromVal structure field

keyFromVal :: (Eq v) => [(u, v)] -> v -> u
keyFromVal [] _ = error ("not found")
keyFromVal (x:xs) val 
    | snd x == val = fst x
    | otherwise = keyFromVal xs val

-- TODO
getStructStructure :: [MemoryCell] -> String -> [(Type, String)]
getStructStructure memory x = []


getLValueType :: [MemoryCell] -> LValue -> Type
getLValueType memory (CONSLValueId id) = 
    getVarTypeInMemory memory id 

getLValueType memory (CONSLValueStruct name fields ) = 
    getStructFieldType memory name fields

getLValueType memory (CONSLValueArray name fields) 
    | not (elem NatInt  (f fields)  ) = error ("the arr index must be int")
    | otherwise = unwrapArrayType (length fields) t

    where 
        t = getVarTypeInMemory memory name 
        f xs = map (setAndGetExpType memory) xs

getLValueType memory (CONSLValueDerref name x) = 
    unwrapPointerType x t

    where 
        t = getVarTypeInMemory memory name 

setAndGetExpType :: [MemoryCell] -> Exp -> Type 
setAndGetExpType memory exp1 = 
    getExpType (setExpType memory exp1) 

unwrapArrayType :: Int -> Type -> Type 
unwrapArrayType 0 t = t
unwrapArrayType x (NatArray t) =  unwrapArrayType (x-1) t
unwrapArrayType _ _ = error ("unwrappable arr")

unwrapPointerType :: Int -> Type -> Type 
unwrapPointerType 0 t = t
unwrapPointerType x (NatPointer t) =  unwrapPointerType (x-1) t
unwrapPointerType _ _ = error ("unwrappable pointer")