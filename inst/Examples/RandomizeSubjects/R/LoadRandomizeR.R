######################################################################################################################## .
#' @name LoadRandomizeR
#' @title Initialize the Randomization Example
#' @description
#' Sets the simulation seed and loads `randomizeR` so later block-randomization
#' callbacks can call the package during the simulation.
#' @author Shubham Lahoti, Gabriel Potvin, Anoop Singh Rawat
#' @param Seed Integer randomization seed supplied by the engine.
#' @return Integer error code `0` after initialization completes.
######################################################################################################################## .

LoadRandomizeR <- function( Seed )
{
    nError <- 0
    set.seed( Seed )
    library( randomizeR )
    return( as.integer( nError ) )

}
