#' Run a simplified bug check
#'
#' Provides a unified entrypoint for executing \code{RunBugCheck} on a function and its inputs.
#' Accepts a function specification (object, name, or path) and parameter specification (list or path).
#' Optionally assigns the resolved function, input data, and result into the caller environment.
#'
#' @param functionInput Function object, name, or file path specifying the function to test.
#' @param paramsInput List object or file path specifying the input parameters for the function.
#' @param strAssignResultName Character scalar name to use when assigning the result into the caller environment.
#'   Defaults to \code{"res"}.
#' @param bAssignVars Logical; if \code{TRUE}, assigns the resolved function (\code{BuggedFunction}) and
#'   inputs (\code{lInputData}) into the caller environment. When \code{TRUE}, the result is also assigned
#'   under \code{strAssignResultName}.
#' @param envAssign Environment where variables should be assigned (default: parent frame).
#'
#' @return The result object returned by \code{RunBugCheck}.
#'
#' @details
#' This function is a lightweight wrapper that calls three helpers:
#' \itemize{
#'   \item \code{ResolveFunction( functionInput )}: resolves the function specification.
#'   \item \code{ResolveParams( paramsInput )}: resolves the input data specification.
#'   \item \code{RunBugCheck( fn, lInputData )}: executes the bug check itself.
#' }
#'
#' @section Side Effects:
#' When \code{bAssignVars} is \code{TRUE}, the following variables are created in \code{envAssign}:
#' \itemize{
#'   \item \code{BuggedFunction}: the resolved function object.
#'   \item \code{lInputData}: the resolved input list.
#'   \item \code{<strAssignResultName>}: the result of \code{RunBugCheck}.
#' }
#'
#' @examples
#' \dontrun{
#'   out <- RunBugCheckSimple( "myfun", list( x = 1, y = 2 ) )
#'   print( out )
#' }
#'
#' @seealso \code{RunBugCheck}, \code{ResolveFunction}, \code{ResolveParams}
#'
#' @export
RunBugCheckSimple <- function( functionInput, paramsInput,
                               strAssignResultName = "res",
                               bAssignVars = TRUE,
                               envAssign = parent.frame() ) {
    fn <- ResolveFunction( functionInput )
    lInput <- ResolveParams( paramsInput )
    
    if ( isTRUE( bAssignVars ) ) {
        assign( "BuggedFunction", fn, envir = envAssign )
        assign( "lInputData",     lInput, envir = envAssign )
    }
    
    res <- RunBugCheck( fn, lInput )
    
    if ( isTRUE( bAssignVars ) &&
         is.character( strAssignResultName ) && length( strAssignResultName ) == 1L && nzchar( strAssignResultName ) ) {
        assign( strAssignResultName, res, envir = envAssign )
    }
    
    return( res )
}
