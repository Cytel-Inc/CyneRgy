######################################################################################################################## .
#' @name LoadNlme
#' @title Initialize Repeated-Measures Modeling Packages
#' @description Sets the simulation seed and loads nlme and stats for repeated-measures modeling.
#' @author Shubham Lahoti
#' @param Seed Integer randomization seed supplied by East Horizon to initialize R's random-number generator. It may be `NULL`.
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#' @return Integer error code `0` after initialization.
######################################################################################################################## .

LoadNlme <- function( Seed, UserParam = NULL )
{
  nError <- 0
  set.seed( Seed )
  library( nlme )
  library( stats )
  return( as.integer( nError ) )

}
