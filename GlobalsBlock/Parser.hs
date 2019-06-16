_globalsBlock = 
    do 
        initList <- many (_initialization)
        return (CONSGlobalsBlock initList)

_initialization = 
    do
        retType <- generalType
        let actualType = getRetType retType
        id <- _idToken
        assignToken <- _assignToken
        expr <- expression
        semiColon <- _semiColonToken

