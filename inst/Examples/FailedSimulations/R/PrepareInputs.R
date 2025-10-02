#' Run Auto Bug Check from Files Safely
#'
#' Reads a candidate .rds file of parameters and a .R file containing function definitions,
#' auto-selects the best function + argument list pairing, builds a reproducible call string,
#' and runs the check safely. Prints a reproducible call and execution status.
#'
#' @param strRds Character path to an .rds file containing candidate parameter lists.
#' @param strRFile Character path to an .R file containing candidate functions.
#' @param bPrintCall Logical; if TRUE (default), prints a reproducible call string before running.
#'
#' @return A list containing fields: \code{ok}, \code{value}, \code{error}, \code{call_string}, \code{fn}, \code{fn_name}, and \code{args}.
#'
#' @examples
#' \dontrun{
#'   RunAutoFromFilesSafe("test_inputs.rds", "analysis_code.R")
#' }
#'
#' @export
RunAutoFromFilesSafe <- function( strRds, strRFile, bPrintCall = TRUE ) {
    #---------------------------------------------------------------------- -
    # Step 1: Load candidate parameter lists from .rds
    #---------------------------------------------------------------------- -
    lAll <- readRDS( strRds )
    
    #---------------------------------------------------------------------- -
    # Step 2: Auto-select the best (function, args) from .R file
    #---------------------------------------------------------------------- -
    pick <- AutoSelectFunctionAndArgs( lAll, strRFile )
    
    #---------------------------------------------------------------------- -
    # Step 3: Bind into a reproducible call structure (includes name)
    #---------------------------------------------------------------------- -
    bound <- BindFunctionCall( pick$fn, pick$args, fn_name = pick$fn_name )
    
    #---------------------------------------------------------------------- -
    # Step 4: Optionally print reproducible call string
    #---------------------------------------------------------------------- -
    if ( isTRUE( bPrintCall ) ) {
        cat( "Reproducible call:\n", bound$call_string, "\n\n", sep = "" )
    }
    
    #---------------------------------------------------------------------- -
    # Step 5: Prefer RunBugCheck if available, else SafeCall
    #---------------------------------------------------------------------- -
    res <- if ( exists( "RunBugCheck", mode = "function" ) ) {
        RunBugCheck( bound$fn, bound$args )
    } else {
        SafeCall( bound$fn, bound$args )
    }
    
    #---------------------------------------------------------------------- -
    # Step 6: Print outcome summary
    #---------------------------------------------------------------------- -
    if ( isTRUE( res$ok ) ) {
        cat( "OK: function executed without error.\n" )
    } else {
        cat( "ERROR: ", if ( is.null( res$error_message ) ) "" else res$error_message, "\n", sep = "" )
    }
    
    #---------------------------------------------------------------------- -
    # Step 7: Return structured result bundle
    #---------------------------------------------------------------------- -
    lOut <- list(
        ok          = res$ok,
        value       = res$value,
        error       = res$error_message,
        call_string = bound$call_string,
        fn          = bound$fn,
        fn_name     = pick$fn_name,
        args        = bound$args
    )
    return( lOut )
}
