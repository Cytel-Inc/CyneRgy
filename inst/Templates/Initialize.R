######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template: Initialize the R simulation environment
#' @description Initialize the R simulation environment. Use this template as a starting point for custom logic.
#'   Preserve the engine-supplied argument names and access named list elements by name. Supply additional
#'   user-defined inputs through UserParam where that argument is supported.
#' @param Seed Integer random seed supplied by the engine. Setting the seed here affects the R random number
#'   generator.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Integer execution status: 0 = no error; a positive value aborts the current simulation but allows
#'   subsequent simulations to run; a negative value is fatal and stops all further simulations.
#' @details Do not use \code{install.packages} or attempt to install new R packages in East Horizon as this will fail. Please
#'   contact help to install libraries.
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( Seed = NULL, UserParam = NULL ) {
    # Step 1 - Set default return values ####
    nErrorCode <- 0

    # Step 2 - If the user provided a seed then use it to set the seed in R  ####
    if ( !is.null( Seed ) ) {
        # User may use other options in set.seed like setting the Random Number Generator, this example only sets the
        #   seed
        set.seed( Seed )
    }

    # Step 3 - Common tasks include: initialize global variables, load required libraries, source any additional files
    #   ####

    # Step 4 - Do the error handling Modify Error appropriately ####
    return( as.integer( nErrorCode ) )
}
