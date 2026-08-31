######################################################################################################################## .
# Last Modified Date: 24/09/2025
#' @name GetDEPDecisionsFSD
#' @author Gabriel Potvin, Anoop Singh Rawat, Pradip Maske
#' @title Computing Decisions for DEP Fixed Sample design.
#'
#' @description Compute decisions for DEP given test statistic and total Alpha using Bonferroni multiplicity adjustment method.
#'
#' @param SimData Data frame with subject data generated in current simulation with one row per patient.
#' @param DesignParam Input Parameters which user may need to compute test statistic and perform test. Refer to the DEP analysis template (Analyze.DEP.R) for details of this list.
#' @param LookInfo List containing information for multiple-look designs. This is `NULL` for a fixed sample design.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
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
#' @note The current code assumes there are no dropouts. Modify the code accordingly for dropout case.
######################################################################################################################## .

# Function Template for performing Multiplicity Adjustment for One Look Tests
GetDEPDecisionsFSD <- function( SimData, DesignParam, LookInfo = NULL, TestStat, OutList = NULL, UserParam = NULL )
{
    lDecision      <- list()
    vEndpointName  <- DesignParam$vEndpointName

    if( DesignParam$TailType[[ 1 ] ] == 0 )
    {
        lDecision[ vEndpointName[[ 1 ] ] ] <- ifelse( pnorm( TestStat[[ 1 ] ] ) < DesignParam$Alpha / 2, 1, 0 )
    }
    else
    {
        lDecision[ vEndpointName[[ 1 ] ] ] <- ifelse( pnorm( TestStat[[ 1 ] ], lower.tail = FALSE ) < DesignParam$Alpha / 2, 1, 0 )
    }

    if( DesignParam$TailType[[ 2 ] ] == 0 )
    {
        lDecision[ vEndpointName[[ 2 ] ] ] <- ifelse( pnorm( TestStat[[ 2 ] ] ) < DesignParam$Alpha / 2, 1, 0 )
    }
    else
    {
        lDecision[ vEndpointName[[ 2 ] ] ] <- ifelse( pnorm( TestStat[[ 2 ] ], lower.tail = FALSE ) < DesignParam$Alpha / 2, 1, 0 )
    }

    nError          <- 0
    nRetval         <- 0
    lOutList        <- list()
    lOutList$OutVal <- nRetval

    return( list( Decision = as.list( lDecision ), OutList = as.list( lOutList ), ErrorCode = as.integer( nError ) ) )
}
