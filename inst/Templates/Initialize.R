#' {{FUNCTION_NAME}}
#'
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
#'
#' @examples
{{FUNCTION_NAME}} <- function( Seed = NULL, UserParam = NULL)
{
    # Step 1 - Set default return values ####
    Error <- 0
    
    # Step 2 - If the user provided a seed then use it to set the seed in R  ####
    if( !is.null(Seed) )
    {
        # User may use other options in set.seed like setting the Random Number Generator, this example only sets the seed
        set.seed(Seed)
    }
   
    # Step 3 - Common tasks include,  initialize global variables, load required libraries, create source any additional files  ####
    
    # Step 4 - Do the error handling Modify Error appropriately ####
    return(as.integer(Error))
}