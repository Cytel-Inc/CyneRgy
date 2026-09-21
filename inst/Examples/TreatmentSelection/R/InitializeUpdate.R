######################################################################################################################## .
#' @name Initialize
#' @title Initialize the Treatment Selection Example
#' @description
#' Sets the R random-number seed supplied by the simulation engine.
#' @author Sydney Ringold, J. Kyle Wathen
#' @param Seed Integer randomization seed supplied by East Horizon to initialize R's random-number generator. It may be `NULL`.
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#' @return Integer error code `0` after the seed is set.
######################################################################################################################## .

Initialize <- function( Seed, UserParam = NULL )
{
    nError <-  0
    set.seed( Seed )  # Note: Setting the seed here only impacts whatever is done in R.
    return( as.integer( nError ) )
}
