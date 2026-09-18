######################################################################################################################## .
#' @name LoadMass
#' @title Initialize MASS for Repeated-Measures Simulation
#' @description Sets the simulation seed and loads MASS for multivariate normal response generation.
#' @author Shubham Lahoti
#' @param Seed Integer randomization seed supplied by the engine.
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#' @return Integer error code `0` after initialization.
######################################################################################################################## .

LoadMass <- function( Seed, UserParam = NULL )
{
  nError <- 0
  set.seed( Seed )
  library( MASS )
  return( as.integer( nError ) )
}
