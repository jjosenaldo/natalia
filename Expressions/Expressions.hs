module Expressions.Expressions where

    import Expressions.Evaluation
    import Expressions.Operations
    import Lexical.Lexemes
    import Lexical.Tokens
    import Memory.Memory
    import Types.Types
    
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
            updateState(memory_update (setValue var val))
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
            result <- evalRemainingGroup8 (RetValue (binary_eval (getRetValue l) (getRetToken op) (getRetValue r)))
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
            result <- evalRemainingGroup7 (RetValue (binary_eval (getRetValue l) (getRetToken op) (getRetValue r)))
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
            result <- evalRemainingGroup6 (RetValue (binary_eval (getRetValue l) (getRetToken op) (getRetValue r)))
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
            result <- evalRemainingGroup5 (RetValue (binary_eval (getRetValue l) (getRetToken op) (getRetValue r)))
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
                return (RetValue (binary_eval (getRetValue l) (getRetToken op) (getRetValue r))))
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
            result <- evalRemainingGroup3 (RetValue (binary_eval (getRetValue l) (getRetToken op) (getRetValue r)))
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
            result <- evalRemainingGroup2 (RetValue (binary_eval (getRetValue l) (getRetToken op) (getRetValue r)))
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
            return (RetValue (unary_eval (getRetToken op) (getRetValue a))))
        <|>
        (do 
            a <- expGroup0
            return (a))
    
    expGroup0 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
    expGroup0 = int_token <|> double_token <|> stringToken <|> localVariable <|> exp_parenthesized

    localVariable :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
    localVariable = 
        do
            mem <- getState
            id <- id_token
            if (memory_has_name (get_id_name (getRetToken id)) mem) then do return (RetValue (getValue (memory_get (get_id_name (getRetToken id)) (get_pos (getRetToken id)) mem)))
            else error ("ERROR " ++ (get_id_name (getRetToken id)) ++ " is not a variable")
    
    
    -- Assignment of a value to a variable
    var_attribution :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
    var_attribution = do
        a <- id_token -- RetToken
        b <- assignToken -- RetToken
        expr_val <- expression -- RetValue
        s <- getState -- [MemoryCell]
        
        let var = memory_get (get_id_name (getRetToken a)) (get_pos (getRetToken a)) s --MemoryCell
        let var_type = getTypeFromValue (getValue var)
        let expr_type = getTypeFromValue (getRetValue expr_val)
        if (not (checkCompatibleTypes var_type expr_type)) then fail ("ERROR at " ++ show(get_pos (getRetToken a))  ++ ": type mismatch in the attribution of a value to a variable.")
        else
            do
                let updatedVar = setValue var (getRetValue expr_val)
                updateState(memory_update updatedVar)
                
                -- optional: print symbols_table content
                s <- getState
                --liftIO (print s)
                return (expr_val)
    
    -- Constant expression
    exp_const :: ParsecT [Token] [MemoryCell] IO(ReturnObject)     
    exp_const = int_token -- <|> double_token
    
    -- Parenthesized expression
    exp_parenthesized :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
    exp_parenthesized = 
        do
            lparen <- left_paren_token
            expr <- expression
            rparen <- right_paren_token
            return (expr)
    
    -- Expression that consists of a local variable

    lValue :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
    lValue = 
        do
            mem <- getState -- [MemoryCell]
            name <- id_token -- RetToken Id
            let nameRetToken = getRetToken name -- Token Id
            let var = memory_get (get_id_name nameRetToken) (get_pos nameRetToken) mem -- Variable
            if (not (isVariable var)) then fail ("ERROR name INSERT NAME HERE LATER doesn't correspond to a variable")
            else do return (RetMemoryCell var)

    -- exp_local_var :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
    -- exp_local_var = 
    --     do
    --         mem <- getState
    --         name <- id_token -- RetToken Id
    --         let value = getValue (memory_get (get_id_name (getRetToken name)) (get_pos (getRetToken name)) mem)
    --         return (RetValue value)