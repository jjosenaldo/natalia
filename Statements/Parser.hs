module Statements.Parser where 

-- natalia's modules
import Expressions.Parser
import Lexical.Lexemes
import Lexical.Tokens
import Statements.Grammar
import Types.Types


_statement = 
    do 
        init <- _varInit -- Statement
        return $ CONSStatementVarInit init

_varInit = 
    do 
        ttype  <- _lexicalTypeToken -- Token
        id <- _idToken
        ass <- _assignToken -- Token
        expr <- _expr -- Exp
        return $ CONSVarInit (getTypeFromTypeToken ttype) (get_id_name id) expr