module Syntax.Definition where

-- natalia's modules
import Expressions.Operations
import Lexical.Lexemes
import Memory.Memory
import TypeValue.TypeValue

data Block = 
    CONSBlock [Statement]
    deriving (Eq, Show)

data Statement = 
    CONSStatementVarInit VarInit                            |
    CONSStatementVarAssign VarAssign                        |
    CONSStatementPrint Print                                |
    CONSStatementBlock Block                                |
    CONSStatementIf Expression Block                        |
    CONSStatementIfElse Expression Block Block              |
    CONSStatementWhile Expression Block
    deriving (Eq, Show)

data Print = 
    CONSPrint Expression
    deriving (Eq, Show)

data VarInit = 
    CONSVarInit Type Id Expression
    deriving (Eq, Show)

data VarAssign = 
    CONSVarAssign Id Expression
    deriving (Eq, Show)

data UnOperator = 
    CONSTokenUnOperator Token
    deriving (Eq, Show)

data BinOperator =
    CONSTokenBinOperator Token 
    deriving (Eq, Show)

data Id = 
    CONSTokenId Token -- Id
    deriving (Eq, Show)

data Expression = 
    CONSValue Value Type | -- literals
    CONSId Id Type |
    CONSUnOperation UnOperator Expression Type |
    CONSBinOperation BinOperator Expression Expression Type |
    CONSExprVarAssignment Id Expression Type
    deriving (Eq, Show)

getSyntacticalUnitPos :: Expression -> (Int, Int)
getSyntacticalUnitPos (CONSValue x _) = getPosValue x 

getTypeOfExpression :: Expression -> Type
getTypeOfExpression (CONSValue _ x) = x
getTypeOfExpression (CONSId _ x) = x
getTypeOfExpression (CONSUnOperation _ _ x) = x
getTypeOfExpression (CONSBinOperation _ _ _ x) = x
getTypeOfExpression (CONSExprVarAssignment _ _ x ) = x

getBlockStatements :: Block -> [Statement]
getBlockStatements (CONSBlock x) = x

-- TODO: this function should search in the memory for the local variable.
-- | Returns the type of a local variable.
getTypeOfLocalVar :: String -- ^ the name of the local variable
                  -> Type -- ^ the type of the local variable
getTypeOfLocalVar idName = NatInt


getBinOperatorExpectedSecondType :: BinOperator -> Type -> Type
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Times p)) NatInt = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Times p)) NatDouble = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Times p)) _ = 
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator * must be a " ++ (getNameOfType NatInt) ++ " or a " ++ (getNameOfType NatDouble))

getBinOperatorExpectedSecondType (CONSTokenBinOperator (And p)) NatBool = NatBool
getBinOperatorExpectedSecondType (CONSTokenBinOperator (And p)) _ = 
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator && must be a " ++ (getNameOfType NatBool))

getBinOperatorExpectedSecondType (CONSTokenBinOperator (Equals p)) _ = NatGenType
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Different p)) _ = NatGenType

getBinOperatorExpectedSecondType (CONSTokenBinOperator (Or p)) NatBool = NatBool
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Or p)) _ = 
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator || must be a " ++ (getNameOfType NatBool))    

getBinOperatorExpectedSecondType (CONSTokenBinOperator (LessThan p)) NatInt = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (LessThan p)) NatDouble = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (LessThan p)) (NatSet t) = (NatSet t)
getBinOperatorExpectedSecondType (CONSTokenBinOperator (LessThan p)) _ = 
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator < must be a " ++ (getNameOfType NatInt) ++ " or a " ++ (getNameOfType NatDouble))

getBinOperatorExpectedSecondType (CONSTokenBinOperator (LessEquals p)) NatInt = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (LessEquals p)) NatDouble = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (LessEquals p)) _ = 
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator <= must be a " ++ (getNameOfType NatInt) ++ " or a " ++ (getNameOfType NatDouble))

getBinOperatorExpectedSecondType (CONSTokenBinOperator (GreaterThan p)) NatInt = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (GreaterThan p)) NatDouble = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (GreaterThan p)) _ = 
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator > must be a " ++ (getNameOfType NatInt) ++ " or a " ++ (getNameOfType NatDouble))

getBinOperatorExpectedSecondType (CONSTokenBinOperator (GreaterEquals p)) NatInt = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (GreaterEquals p)) NatDouble = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (GreaterEquals p)) _ = 
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator >= must be a " ++ (getNameOfType NatInt) ++ " or a " ++ (getNameOfType NatDouble))
    

getBinOperatorExpectedSecondType (CONSTokenBinOperator (Plus p)) NatInt = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Plus p)) NatDouble = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Plus p)) NatString = NatString
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Plus p)) _ =
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator + must be a " ++ (getNameOfType NatInt) ++ " or "++(getNameOfType NatDouble)++" or a " ++ (getNameOfType NatString))

getBinOperatorExpectedSecondType (CONSTokenBinOperator (Minus p)) NatInt = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Minus p)) NatDouble = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Minus p)) _ =
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator - must be a " ++ (getNameOfType NatInt) ++ " or "++(getNameOfType NatDouble))

getBinOperatorExpectedSecondType (CONSTokenBinOperator (Div p)) NatInt = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Div p)) NatDouble = NatDouble
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Div p)) _ = 
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator / must be a " ++ (getNameOfType NatInt) ++ " or a " ++ (getNameOfType NatDouble))

getBinOperatorExpectedSecondType (CONSTokenBinOperator (Mod p)) NatInt = NatInt
getBinOperatorExpectedSecondType (CONSTokenBinOperator (Mod p)) _ = 
    error ("ERROR at " ++ show(p) ++ ": the first arg of the operator % must be a " ++ (getNameOfType NatInt))

buildExpressionFromBinaryOperation :: BinOperator -> Expression -> Expression -> Expression
buildExpressionFromBinaryOperation op exp1 exp2 = 
    CONSBinOperation op exp1 exp2 (getBinOperatorReturnType op leftType rightType)
    where
        leftType = getTypeOfExpression exp1 
        rightType = getTypeOfExpression exp2


getUnOperatorExpectedType :: UnOperator -> Type 
getUnOperatorExpectedType (CONSTokenUnOperator (Negation p)) = NatBool 
getUnOperatorExpectedType (CONSTokenUnOperator (Minus p)) = NatDouble 

getUnOperatorReturnType :: UnOperator -> Type -> Type
getUnOperatorReturnType (CONSTokenUnOperator (Negation p)) NatBool = 
    NatBool
getUnOperatorReturnType (CONSTokenUnOperator (Negation p)) other = 
    error ("ERROR at " ++ show(p) ++ ": the ! operator expects a " ++  (getNameOfType NatBool) ++ " but you passed a " ++ (getNameOfType other))

getUnOperatorReturnType (CONSTokenUnOperator (Minus p)) NatDouble = 
    NatDouble
getUnOperatorReturnType (CONSTokenUnOperator (Minus p)) NatInt = 
    NatInt
getUnOperatorReturnType (CONSTokenUnOperator (Minus p)) other = 
    error ("ERROR at " ++ show(p) ++ ": the unary - operator expects a " ++  (getNameOfType NatDouble) ++ " or a " ++ (getNameOfType NatInt) ++ " but you passed a " ++ (getNameOfType other))

getBinOperatorReturnType :: BinOperator -> Type -> Type -> Type 
getBinOperatorReturnType (CONSTokenBinOperator (Times p)) NatInt NatInt = NatInt
getBinOperatorReturnType (CONSTokenBinOperator (Times p)) NatDouble NatInt = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Times p)) NatInt NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Times p)) NatDouble NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Times p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator * expects two " ++ (getNameOfType NatDouble))

getBinOperatorReturnType (CONSTokenBinOperator (Div p)) NatInt NatInt = NatInt
getBinOperatorReturnType (CONSTokenBinOperator (Div p)) NatDouble NatInt = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Div p)) NatInt NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Div p)) NatDouble NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Div p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator / expects two " ++ (getNameOfType NatDouble))

getBinOperatorReturnType (CONSTokenBinOperator (Minus p)) NatInt NatInt = NatInt
getBinOperatorReturnType (CONSTokenBinOperator (Minus p)) NatDouble NatInt = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Minus p)) NatInt NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Minus p)) NatDouble NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Minus p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator - expects two " ++ (getNameOfType NatInt))

getBinOperatorReturnType (CONSTokenBinOperator (Plus p)) NatInt NatInt = NatInt
getBinOperatorReturnType (CONSTokenBinOperator (Plus p)) NatDouble NatInt = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Plus p)) NatInt NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Plus p)) NatDouble NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Plus p)) NatString NatString = NatString
getBinOperatorReturnType (CONSTokenBinOperator (Plus p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator + expects two " ++ (getNameOfType NatInt) ++ " or " ++ (getNameOfType NatString))

getBinOperatorReturnType (CONSTokenBinOperator (LessThan p)) NatInt NatInt = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (LessThan p)) NatDouble NatInt = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (LessThan p)) NatInt NatDouble = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (LessThan p)) NatDouble NatDouble = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (LessThan p)) (NatSet _) (NatSet _) = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (LessThan p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator < expects two " ++ (getNameOfType NatDouble) ++ " or two " ++ (getNameOfType (NatSet NatGenType)))

getBinOperatorReturnType (CONSTokenBinOperator (LessEquals p)) NatInt NatInt = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (LessEquals p)) NatDouble NatInt = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (LessEquals p)) NatInt NatDouble = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (LessEquals p)) NatDouble NatDouble = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (LessEquals p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator <= expects two " ++ (getNameOfType NatDouble) ++ " or two " ++ (getNameOfType (NatSet NatGenType)))

getBinOperatorReturnType (CONSTokenBinOperator (GreaterThan p)) NatInt NatInt = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (GreaterThan p)) NatDouble NatInt = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (GreaterThan p)) NatInt NatDouble = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (GreaterThan p)) NatDouble NatDouble = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (GreaterThan p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator > expects two " ++ (getNameOfType NatDouble) ++ " or two " ++ (getNameOfType (NatSet NatGenType)))

getBinOperatorReturnType (CONSTokenBinOperator (GreaterEquals p)) NatInt NatInt = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (GreaterEquals p)) NatDouble NatInt = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (GreaterEquals p)) NatInt NatDouble = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (GreaterEquals p)) NatDouble NatDouble = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (GreaterEquals p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator >= expects two " ++ (getNameOfType NatDouble) ++ " or two " ++ (getNameOfType (NatSet NatGenType)))

getBinOperatorReturnType (CONSTokenBinOperator (And p)) NatBool NatBool = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (And p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator && expects two " ++ (getNameOfType NatBool) )

getBinOperatorReturnType (CONSTokenBinOperator (Or p)) NatBool NatBool = NatBool
getBinOperatorReturnType (CONSTokenBinOperator (Or p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator && expects two " ++ (getNameOfType NatBool) )

getBinOperatorReturnType (CONSTokenBinOperator (Equals p)) _ _ = NatBool

getBinOperatorReturnType (CONSTokenBinOperator (Different p)) _ _ = NatBool

getBinOperatorReturnType (CONSTokenBinOperator (Expo p)) NatInt NatInt = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Expo p)) NatDouble NatInt = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Expo p)) NatInt NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Expo p)) NatDouble NatDouble = NatDouble
getBinOperatorReturnType (CONSTokenBinOperator (Expo p)) _ _ = 
    error ("ERROR at " ++ show(p) ++ ": the operator ^ expects two " ++ (getNameOfType NatDouble))

getBinOperatorReturnType (CONSTokenBinOperator (Mod p)) NatInt NatInt = 
    error ("ERROR at " ++ show(p) ++ ": the operator % expects two " ++ (getNameOfType NatInt))