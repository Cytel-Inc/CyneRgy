#################################################################################################### .
#' @title Initialize CyneRgy Library for Simulations
#' 
#' @description
#' The Initialize function initializes the R environment for all simulations. It is optional and is executed before any user-defined functions. 
#' Key functionalities include:
#' - Setting a seed for random number generation
#' - Loading required packages
#' 
#' Here, the CyneRgy library is initialized.
#' 
#' @note 
#' Do not use `install.packages` or attempt to install new R packages in East Horizon, as this will fail. Please contact support to install libraries.
#' 
#' @param Seed An integer value to set the seed used for generating random numbers in R. Default is NULL.
#' @param UserParam A named list to pass custom scalar variables defined by users. Variables can be accessed using names, 
#'                  e.g., `UserParam$Var1`. Default is NULL.
#' 
#' @return A list containing:
#'   \item{ErrorCode}{An integer indicating success or error status:
#'     \describe{
#'       \item{ErrorCode = 0}{No error.}
#'       \item{ErrorCode > 0}{Nonfatal error, current simulation aborted but subsequent simulations will run.}
#'       \item{ErrorCode < 0}{Fatal error, no further simulations attempted.}
#'     }
#'   }
#' 
#' @export
#################################################################################################### .

InitLoadCyneRgy <- function( Seed , UserParam = NULL )
{
    library( CyneRgy )
   
    ######################################################################################################################## .
    # Load any data sets or libraries that are needed ####
    ######################################################################################################################## .
   
    Error <- 0
    return(as.integer(Error))
}
