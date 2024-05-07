#' Initialize is used to load required libraries and create any global variables that other integration points need
#' @param Seed An integer value to set the seed used in generating random numbers in R. Default is NULL.
#' @param UserParam A named list to pass custom scalar variables defined by users. Users should access the variables using names, 
#'                  for example UserParam$Var1. Default is NULL.
#'
#' @description
#' Performs initialization for all simulations that use R. This optional function will be executed before executing any of the other user-defined functions. 
#' It can be used for various reasons, such as:
#'    Setting seed for R environment
#'    Loading packages 
#' @note Do not use install.package or attempt to install new R packages in Solara as this will fail. Please contact help to install libraries. 
#' @return An integer as follows:   
#'           0 – No Error
#'           Positive Integer – Non Fatal Error – Particular Simulation will be aborted but Next Simulation will be performed.
#'           Negative Integer – Fatal Error – No further simulation will be attempted.
#' @export
InitLoadCyneRgy <- function( Seed , UserParam = NULL )
{
    library( CyneRgy )
   
    ######################################################################################################################## .
    # Load any data sets libraries that are needed ####
    ######################################################################################################################## .
   
    Error <- 0
    return(as.integer(Error))
}
