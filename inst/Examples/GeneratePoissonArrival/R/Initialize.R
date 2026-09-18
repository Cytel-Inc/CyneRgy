######################################################################################################################## .
#' @name Initialize
#' @title Initialize Poisson Arrival Simulations
#' @description Sets the simulation seed and loads the `survival` package for callbacks used by the example.
#' @author J. Kyle Wathen
#' @param Seed Integer randomization seed supplied by the engine.
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#' @return An integer error code where 0 indicates successful initialization.
######################################################################################################################## .

Initialize <- function( Seed, UserParam = NULL )
{
    nError <- 0
    set.seed( Seed )
    library( survival )
    return( as.integer( nError ) )
}
