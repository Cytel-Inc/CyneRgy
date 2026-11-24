######################################################################################################################## .
#' @name LoadNlme
#' @title Initialize the R simulation environment
#' @description Set the R random seed and load nlme and stats for repeated-measures analyses with
#'   `nlme::gls()` and related model functions.
#' @author Shubham Lahoti
#' @param Seed Numeric scalar containing the integer-valued random seed supplied by the engine.
#'   Setting the seed here affects the R random number generator.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Integer execution status: 0 = successful initialization. A nonzero value reports an initialization
#'   error; initialization occurs before the simulation loop.
######################################################################################################################## .

LoadNlme <- function( Seed, UserParam = NULL ) {
    nErrorCode <- 0
    set.seed( Seed )
    library( nlme )
    library( stats )
    return( as.integer( nErrorCode ) )
}