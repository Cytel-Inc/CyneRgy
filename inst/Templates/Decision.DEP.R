######################################################################################################################## .
#' Last Modified Date: {{CREATION_DATE}}
#'
#' @name {{FUNCTION_NAME}}
#'
#' @title Generate Dual-Endpoint Decisions
#'
#' @param SimData Data frame with subject data generated in current simulation with one row per patient.
#' @param DesignParam Input Parameters which user may need to compute test statistic and perform test. Refer to the DEP analysis template (Analyze.DEP.R) for details of this list.
#' @param LookInfo List Input Parameters related to multiple looks which user may need to compute test statistic and perform test. Refer to the DEP analysis template (Analyze.DEP.R) for details of this list.
#' @param UserParam User can pass custom scalar variables defined by them as a member of this list.
#' @param TestStat List of test statistics for both the endpoints. These test statistics will be on the Z-scale. Access using the actual endpoint names specified by the user,
#'                            e.g., TestStat[EndpointName[1]] or TestStat[EndpointName[2]]
#' @param OutList List of outputs that was returned by the user in the previous look. Only relevant for Group Sequential Design and set to NULL for first look.
#' Supported data types are lists, and scalar and vector of type numeric, integer and character.
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{Decision}{Required value. A list of Decisions on both Endpoints: 0 - No Boundary Crossed, 1 - Lower Efficacy Boundary Crossed, 2 - Upper Efficacy Boundary Crossed, 4- Futility Boundary Crossed.
#'                  \item{Outlist}{Optional list of quantities to pass to the next look. This will be available as inputs to this function in the next look.
#'                            Only applicable for Group Sequential Design. Supported data types are lists, and scalar and vector of type numeric, integer and character.}
#'                  \item{ErrorCode}{Optional integer value \describe{
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }}
#'             }
#'
#'
#' @description
#' Generate endpoint decisions and optional persistent output for a dual-endpoint analysis.
#' The function signature must remain the same.
#' If your custom logic requires use of additional parameters that are not listed above, add them to UserParam.
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( SimData, DesignParam, LookInfo = NULL, TestStat = NULL, OutList = NULL, UserParam = NULL )
{
    EndpointName  <- DesignParam$EndpointName
    lDecision <- list()
    lDecision[ EndpointName[[ 1 ] ] ] <- lDecision[ EndpointName[[ 2 ] ] ] <- 0
    nError          <- 0
    Retval          <- 0
    OutList         <- list()
    OutList$OutVal  <- Retval

    # Write logic to implement a particular multiplicity adjustment method and update Decisions

    return( list( Decision = as.list( lDecision ), OutList = as.list( OutList ), ErrorCode = as.integer( nError ) ) )
}
