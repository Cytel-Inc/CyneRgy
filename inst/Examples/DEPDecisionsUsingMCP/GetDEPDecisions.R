######################################################################################################################## .
#  Last Modified Date: 24/09/2025
#' @name GetDEPDecisionsFSD
#' @author Pradip Maske
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
#'@note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.
#' @note The current code assumes there are no dropouts. Modify the code accordingly for dropout case.
######################################################################################################################## .

# Function Template for performing Multiplicity Adjustment for One Look Tests
GetDEPDecisionsFSD <- function(SimData, DesignParam, LookInfo = NULL, TestStat, OutList = NULL, UserParam = NULL)
{
    Decision      <- list()
    EndpointName  <- DesignParam$EndpointName
    
    if(DesignParam$TailType[[1]] == 0)
    {
        Decision[EndpointName[[1]]] <- ifelse(pnorm(TestStat[[1]]) < DesignParam$Alpha/2, 1, 0)            
    }
    else 
    {
        Decision[EndpointName[[1]]] <- ifelse(pnorm(TestStat[[1]], lower.tail = FALSE) < DesignParam$Alpha/2, 1, 0)            
    }
    
    if(DesignParam$TailType[[2]] == 0)
    {
        Decision[EndpointName[[2]]] <- ifelse(pnorm(TestStat[[2]]) < DesignParam$Alpha/2, 1, 0)            
    }
    else 
    {
        Decision[EndpointName[[2]]] <- ifelse(pnorm(TestStat[[2]], lower.tail = FALSE) < DesignParam$Alpha/2, 1, 0)            
    }
    
    Error           <- 0
    Retval          <- 0
    OutList         <- list()
    OutList$OutVal  <- Retval
    
    return(list(Decision = as.list(Decision), OutList = as.list(OutList), ErrorCode = as.integer(Error)))
}