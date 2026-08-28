######################################################################################################################## .
#' @name LoadMass
#' @title Initialize MASS for Repeated-Measures Simulation
#' @description Sets the simulation seed and loads MASS for multivariate normal response generation.
#' @author Shubham Lahoti
#' @param Seed Integer randomization seed supplied by the engine.
#' @return Integer error code `0` after initialization.
######################################################################################################################## .

LoadMass <- function( Seed )
{
  nError <- 0
  set.seed( Seed )
  library( MASS )
  return( as.integer( nError ) )

}
