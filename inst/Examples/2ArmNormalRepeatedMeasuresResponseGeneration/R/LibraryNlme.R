######################################################################################################################## .
#' @name LoadNlme
#' @title Initialize Repeated-Measures Modeling Packages
#' @description Sets the simulation seed and loads nlme and stats for repeated-measures modeling.
#' @author Shubham Lahoti
#' @param Seed Integer randomization seed supplied by the engine.
#' @return Integer error code `0` after initialization.
######################################################################################################################## .

LoadNlme <- function( Seed )
{
  nError <- 0
  set.seed( Seed )
  library( nlme )
  library( stats )
  return( as.integer( nError ) )

}
