#' Error Check (Public Entry Point)
#'
#' Provides a simplified bug-check entrypoint with only two inputs: an RDS file (parameters) and
#' an R file (functions). Automatically selects the best function/argument list pairing, builds a
#' reproducible call string, and executes the check. Prints summary and returns structured results.
#'
#' @param strRds Character path to an .rds file containing candidate parameter lists.
#' @param strRFile Character path to an .R file containing candidate functions.
#' @param bPrintCall Logical; if TRUE (default), prints the reproducible call string.
#'
#' @return A list containing fields: \code{ok}, \code{value}, \code{error}, \code{call_string}, \code{fn}, \code{fn_name}, and \code{args}.
#'
#' @examples
#' \dontrun{
#'   ErrorCheck("test_inputs.rds", "analysis_code.R")
#' }
#'
#' @export
ErrorCheck <- function( strRds, strRFile, bPrintCall = TRUE ) {
    #----------------------------------------------------------------------------- -
    # Step 1: Load candidate parameter lists from .rds
    #----------------------------------------------------------------------------- -
    lAll <- readRDS( strRds )
    
    #----------------------------------------------------------------------------- -
    # Step 2: Auto-select the best (function, args) pair from .R file
    #----------------------------------------------------------------------------- -    pick <- .AutoSelectBestPair( lAll, strRFile )
    
    #----------------------------------------------------------------------------- -
    # Step 3: Bind into a reproducible call structure (includes function name)
    #----------------------------------------------------------------------------- -
    bound <- BindFunctionCall( pick$fn, pick$args, strFnName = pick$fn_name )
    
    #----------------------------------------------------------------------------- -
    # Step 4: Optionally print reproducible call string
    #----------------------------------------------------------------------------- -
    if ( isTRUE( bPrintCall ) ) {
        cat( "Reproducible call:\n", bound$call_string, "\n\n", sep = "" )
    }
    
    #----------------------------------------------------------------------------- -
    # Step 5: Run function safely, prefer RunBugCheck if available
    #----------------------------------------------------------------------------- -
    res <- if ( exists( "RunBugCheck", mode = "function" ) ) {
        RunBugCheck( bound$fn, pick$args )
    } else {
        SafeCall( bound$fn, pick$args )
    }
    
    #----------------------------------------------------------------------------- -
    # Step 6: Print outcome summary
    #----------------------------------------------------------------------------- -
    if ( isTRUE( res$ok ) ) {
        cat( "OK: function executed without error.\n" )
    } else {
        cat( "ERROR: ", res$error_message %||% "", "\n", sep = "" )
    }
    
    #----------------------------------------------------------------------------- -
    # Step 7: Return structured result bundle
    #----------------------------------------------------------------------------- -
    lOut <- list(
        ok          = res$ok,
        value       = res$value,
        error       = res$error_message,
        call_string = bound$call_string,
        fn          = pick$fn,
        fn_name     = pick$fn_name,
        args        = pick$args
    )
    return( lOut )
}


