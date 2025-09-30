#------------------------------
# RunBugCheckSimple: unified entrypoint
# - Accepts function object/name/path and list/path
# - Optionally assigns BuggedFunction, lInputData, and result in caller env
#------------------------------
RunBugCheckSimple <- function(functionInput, paramsInput,
                              assignResultName = "res",
                              assignVars = TRUE,
                              assignEnv = parent.frame()) {
    fn <- ResolveFunction(functionInput)
    lI <- ResolveParams(paramsInput)
    
    if (isTRUE(assignVars)) {
        assign("BuggedFunction", fn, envir = assignEnv)
        assign("lInputData",     lI, envir = assignEnv)
    }
    
    res <- RunBugCheck(fn, lI)
    
    if (is.character(assignResultName) && length(assignResultName) == 1L && nzchar(assignResultName)) {
        assign(assignResultName, res, envir = assignEnv)
    }
    
    return(res)
}