######################################################################################################################## .
#' @name LoadMass
#'
#' @title Initialize the R simulation environment
#'
#' @description Set the R random seed and load MASS for multivariate normal response generation with
#'   `MASS::mvrnorm()` in the repeated-measures example.
#'
#' @author Shubham Lahoti
#'
#' @param Seed Integer random seed supplied by the engine. Setting the seed here affects the R random number
#'   generator.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' @return Integer execution status: 0 = no error; a positive value aborts the current simulation but allows
#'   subsequent simulations to run; a negative value is fatal and stops all further simulations.
######################################################################################################################## .

LoadMass <- function( Seed, UserParam = NULL ) {
    nErrorCode <- 0
    set.seed( Seed )
    library( MASS )
    return( as.integer( nErrorCode ) )
}
