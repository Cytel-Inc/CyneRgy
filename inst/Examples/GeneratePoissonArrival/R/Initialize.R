######################################################################################################################## .
#' @name Initialize
#' @title Initialize Poisson Arrival Simulations
#' @description Sets the simulation seed and loads the `survival` package for callbacks used by the example.
#' @author J. Kyle Wathen
#' @param Seed Integer randomization seed supplied by the engine.
#' @return An integer error code where 0 indicates successful initialization.
######################################################################################################################## .

Initialize <- function( Seed )
{
    nError <- 0
    set.seed( Seed )
    library( survival )

    return( as.integer( nError ) )
}
