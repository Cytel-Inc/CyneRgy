######################################################################################################################## .
#' @name Initialize
#' @title Initialize the Treatment Selection Example
#' @description
#' Sets the R random-number seed supplied by the simulation engine.
#' @author Sydney Ringold, J. Kyle Wathen
#' @param Seed Integer randomization seed supplied by the engine.
#' @return Integer error code `0` after the seed is set.
######################################################################################################################## .

Initialize <- function( Seed )
{

    nError <-  0
    set.seed( Seed )  # Note: Setting the seed here only impacts whatever is done in R.

    return( as.integer( nError ) )
}
