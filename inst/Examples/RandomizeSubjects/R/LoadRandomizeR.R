######################################################################################################################## .
#' @name LoadRandomizeR
#' @title Initialize the Randomization Example
#' @description
#' Sets the simulation seed and loads `randomizeR` so later block-randomization
#' callbacks can call the package during the simulation.
#' @author Shubham Lahoti and Anoop Singh Rawat
#' @param Seed Integer randomization seed supplied by the engine.
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#' @return Integer error code `0` after initialization completes.
######################################################################################################################## .

LoadRandomizeR <- function( Seed, UserParam = NULL )
{
    nError <- 0
    set.seed( Seed )
    library( randomizeR )
    return( as.integer( nError ) )
}
