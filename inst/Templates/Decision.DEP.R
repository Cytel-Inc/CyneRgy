########################################################################################################################
#' Last Modified Date: {{CREATION_DATE}}
#'
#' @name {{FUNCTION_NAME}}
#'
#' @title Computing Decisions for DEP Fixed Sample design.
#'
#' @description Compute decisions for DEP given test statistic and total Alpha using Bonferroni multiplicity adjustment method.
#'
#' @param SimData Data frame with subject data generated in current simulation with one row per patient. 
#' @param DesignParam Input Parameters which user may need to compute test statistic and perform test. 
#' @param LookInfo List Input Parameters related to multiple looks which user may need to compute test statistic 
#' @param UserParam User can pass custom scalar variables defined by users as a member of this list. 
#' @param TestStat List of test statistics for both the endpoints. These test statistics will be on the Z-scale.
#' @param OutList List of outputs that we want to pass across looks. Only relevant for Group Sequential Design. 
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{Decision}{Required value. A list of Decisions on both Endpoints}
#'                  \item{Outlist}{Optional list of quantities to pass to the next look.
#'                            Only applicable Group Sequential Design.}
#'                  \item{ErrorCode}{Optional integer value \describe{
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Non fatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }}
#'             }
#'
#'
#' @description
#' This template can be used as a starting point for developing custom functionality.  The function signature must remain the same.  
#' However, you may choose to ignore the parameters ArrivalTime, SurvMethod, NumPrd, PrdTime, SurvParam, PropResp, and Correlation if the patient simulator.
#' If you are creating task that requires use of additional parameters that listed above, add that as element to to UserParam.
{{FUNCTION_NAME}} <- function(SimData, DesignParam, LookInfo = NULL, TestStat, OutList = NULL, UserParam = NULL)
{
    EndpointName  <- DesignParam$EndpointName
    Decision      <- list()
    Decision[EndpointName[[1]]] <- Decision[EndpointName[[2]]] <- 0
    Error           <- 0
    Retval          <- 0
    OutList         <- list()
    OutList$OutVal  <- Retval
    
    # Write logic to implement a particular multiplicity adjustment method and update Decisions
    
    
    return(list(Decision = as.list(Decision), OutList = as.list(OutList), ErrorCode = as.integer(Error)))
}