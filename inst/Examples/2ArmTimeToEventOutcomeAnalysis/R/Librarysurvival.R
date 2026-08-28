######################################################################################################################## .
#' @name Loadsurvival
#' @title Initialize the survival Package
#' @description Sets the simulation seed and loads survival for time-to-event analysis callbacks.
#' @author Anoop Singh Rawat, Shubham Lahoti, and Gabriel Potvin
#' @param Seed Integer randomization seed supplied by the engine.
#' @return Integer error code `0` after initialization.
######################################################################################################################## .

Loadsurvival <- function( Seed )
{
    nError <- 0
    set.seed( Seed )
    library( survival )
    return( as.integer( nError ) )

}
