######################################################################################################################## .
#' @name LoadRM
#' @title Initialize Repeated-Measures Analysis Packages
#' @description Sets the simulation seed and loads the packages required by the repeated-measures analysis example.
#' @author Gabriel Potvin and Anoop Singh Rawat
#' @param Seed Integer randomization seed supplied by the engine.
#' @return Integer error code `0` after initialization.
######################################################################################################################## .

LoadRM <- function( Seed )
{
    nError <- 0
    set.seed( Seed )
    library( nlme )
    library( stats )
    library( rpact )
    return( as.integer( nError ) )
}
