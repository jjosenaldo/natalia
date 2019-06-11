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
            result <- evalRemainingGroup8 (getRetValue l)
            return (result))

    evalRemainingGroup8 :: Value -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
    evalRemainingGroup8 l =
        try
        (do
            op <- group8OpToken
            r <- expGroup7
            result <- evalRemainingGroup8 (binary_eval l (getRetToken op) (getRetValue r))
            return (result))
        <|>
        (do
            return (RetValue l))

    expGroup7 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
    expGroup7 =
        (do
            -- this group contains only the operation &&
            l <- expGroup6
            result <- evalRemainingGroup8 (getRetValue l)
            return (result))

    evalRemainingGroup7 :: Value -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
    evalRemainingGroup7 l =
        try
        (do
            op <- group7OpToken
            r <- expGroup6
            result <- evalRemainingGroup7 (binary_eval l (getRetToken op) (getRetValue r))
            return (result))
        <|>
        (do
            return (RetValue l))

    expGroup6 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
    expGroup6 =
        (do
            -- this group contains only the operation == (potentially will contain !=)
            l <- expGroup5
            result <- evalRemainingGroup8 (getRetValue l)
            return (result))

    evalRemainingGroup6 :: Value -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
    evalRemainingGroup6 l =
        try
        (do
            op <- group6OpToken
            r <- expGroup5
            result <- evalRemainingGroup6 (binary_eval l (getRetToken op) (getRetValue r))
            return (result))
        <|>
        (do
            return (RetValue l))

    expGroup5 :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
    expGroup5 =
        (do
            -- this group contains only the operation < (potentially will contain >, <=, >=)
            l <- expGroup4
            result <- evalRemainingGroup8 (getRetValue l)
            return (result))

    evalRemainingGroup5 :: Value -> ParsecT [Token] [MemoryCell] IO(ReturnObject)
    evalRemainingGroup5 l =
        try
        (do
            op <- group5OpToken
            r <- expGroup4
            result <- evalRemainingGroup5 (binary_eval l (getRetToken op) (getRetValue r))
            return (result))
        <|>
        (do
            return (RetValue l))

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
    expGroup0 = int_token <|> localVariable <|> exp_parenthesized

    localVariable :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
    localVariable = 
        do
            mem <- getState
            id <- id_token
            if (memory_has_name (get_id_name (getRetToken id)) mem) then do return (RetValue (getValue (memory_get (get_id_name (getRetToken id)) (get_pos (getRetToken id)) mem)))
            else error ("ERROR " ++ (get_id_name (getRetToken id)) ++ " is not a variable")
    
    -- Expression that has precedence 4
    -- exp4 :: ParsecT [Token] [MemoryCell] IO(Value)
    -- exp4 = 
    --     do
    --         n1 <- exp3
    --         result <- eval_remaining_exp4 n1 
    --         return (result)
    
    -- Evaluates the remainder of a expression that has precedence 4
    -- eval_remaining_exp4 :: Value -> ParsecT [Token] [MemoryCell] IO(Value)
    -- eval_remaining_exp4 n1 = 
    --     try
    --     (do
    --         op <- bin_op_left_4_token
    --         n2 <- exp3
    --         result <- eval_remaining_exp4 (binary_eval n1 op n2)
    --         return (result))
    --     <|>
    --     (do
    --         return (n1))
    
    -- Expression that has precedence 3
    -- exp3 :: ParsecT [Token] [MemoryCell] IO(Value)
    -- exp3 = 
    --     do
    --         n1 <- exp2
    --         result <- eval_remaining_exp3 n1 
    --         return (result)
    
    -- Evaluates the remainder of a expression that has precedence 3
    -- eval_remaining_exp3 :: Value -> ParsecT [Token] [MemoryCell] IO(Value)
    -- eval_remaining_exp3 n1 = 
    --     try
    --     (do
    --         op <- bin_op_left_3_token
    --         n2 <- exp2
    --         result <- eval_remaining_exp3 (binary_eval n1 op n2)
    --         return (result))
    --     <|>
    --     (do
    --         return (n1))
    
    -- Expression that has precedence 2
    -- exp2 :: ParsecT [Token] [MemoryCell] IO(Value)
    -- exp2 = 
    --     try
    --     (do
    --         n1 <- exp1
    --         op <- expo_token
    --         n2 <- exp2
    --         return (binary_eval n1 op n2))
    --     <|>
    --     exp1
    
    -- Expression that has precedence 1
    -- exp1 :: ParsecT [Token] [MemoryCell] IO(Value)
    -- exp1 = 
    --     try
    --     (do
    --         op <- minus_token
    --         n2 <- exp1
    --         return (unary_eval op n2))
    --     <|>
    --     exp0
    
    -- Expression that has precedence 0
    -- exp0 :: ParsecT [Token] [MemoryCell] IO(Value)
    -- exp0 = try var_attribution <|> exp_const -- <|> exp_parenthesized -- <|> exp_local_var
    
    
    
    
    -- Assignment of a value to a variable
    -- var_attribution :: ParsecT [Token] [MemoryCell] IO(ReturnObject)
    -- var_attribution = do
    --     a <- id_token
    --     let var = memory_get a
    --     b <- assignToken
    --     expr_val <- expression
    --     s <- getState
    --     let var_type = var_type_from_name a s
    --     let expr_type = getTypeFromValue expr_val
    
    --     if (not (attr_compatible_types var_type expr_type)) then fail ("ERROR at " ++ show(get_pos expr_val)  ++ ": type mismatch in the attribution of a value to a variable.")
    --     else
    --         do
    --             updateState(memory_update(a,s))
                
    --             -- optional: print symbols_table content
    --             s <- getState
    --             liftIO (print s)
    --             return (RetValue expr_val)
    
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