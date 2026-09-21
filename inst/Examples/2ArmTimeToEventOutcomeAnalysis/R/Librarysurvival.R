######################################################################################################################## .
#' @name Loadsurvival
#' @title Initialize the survival Package
#' @description Sets the simulation seed and loads survival for time-to-event analysis callbacks.
#' @author Anoop Singh Rawat and Shubham Lahoti
#' @param Seed Integer randomization seed supplied by East Horizon to initialize R's random-number generator. It may be `NULL`.
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#' @return Integer error code `0` after initialization.
######################################################################################################################## .

Loadsurvival <- function( Seed, UserParam = NULL )
{
    nError <- 0
    set.seed( Seed )
    library( survival )
    return( as.integer( nError ) )
}
