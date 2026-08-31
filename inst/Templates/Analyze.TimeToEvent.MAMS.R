######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}

#' @name {{FUNCTION_NAME}}
#' @title Analyze Multi-Arm Time-to-Event Outcomes
#' @param SimData A data frame containing simulated patient level data.
#'        Required variables in the data frame include:
#'        \describe{
#'        \item{ArrivalTime}{Patient enrollment time, numeric vector}
#'        \item{TreatmentID}{Treatment assignment where 0 = control and 1,2,... represent treatment arms}
#'        \item{SurvivalTime}{Observed or simulated survival time for each patient}
#'        }
#'
#' @param DesignParam A list containing design parameters supplied from East Horizon.
#'        Common parameters include:
#'        \describe{
#'        \item{Alpha}{One-sided significance level}
#'        \item{TailType}{Tail direction, 1 = upper tail, 0 = lower tail}
#'        \item{NumTreatments}{Number of treatment arms excluding control}
#'        \item{MaxEvents}{Maximum number of events required for analysis}
#'        \item{CriticalPoint}{Critical boundary for fixed sample designs}
#'        \item{IsArmPresent}{Vector indicating which treatment arms remain active}
#'        }
#'
#' @param LookInfo Optional list containing interim analysis information.
#'        If supplied, common elements include:
#'        \describe{
#'        \item{NumLooks}{Total number of analyses}
#'        \item{CurrLookIndex}{Current analysis index}
#'        \item{InfoFrac}{Information fraction for each look}
#'        \item{EffBdry}{Efficacy boundaries for each look}
#'        }
#'
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default of NULL, as in this example.
#'        If UserParam are supplied, they will be available as elements in the list UserParam.
#'
#' @return The function must return a list in the return statement of the function. The information below lists
#'         elements of the list, if the element is required or optional and a description of the return values if needed.
#'         \describe{
#'         \item{Decision}{Required integer vector containing decision for each treatment arm
#'                         \describe{
#'                         \item{0}{Continue trial}
#'                         \item{2}{Reject null hypothesis / efficacy success}
#'                         \item{3}{Futility at final analysis}
#'                         }
#'                         }
#'         \item{HR}{Required numeric vector containing observed hazard ratios for each treatment arm versus control}
#'         \item{AnalysisTime}{Required numeric value containing the calendar time of the current analysis}
#'         \item{ErrorCode}{Optional integer value
#'                         \describe{
#'                         \item{ErrorCode = 0}{No Error}
#'                         \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                         \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                         }
#'                         }
#'         }
#'
#' @description Analyze simulated time-to-event outcomes for a multiple-arm confirmatory design at the current look.
#'
#' The function signature must remain unchanged. However, additional user-defined logic
#' and parameters may be incorporated through the UserParam list if needed.
#' @keywords Multi-Arm, time-to-event endpoints analysis.
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Step 1 - Initialization
    vDecision       <- rep( 0, DesignParam$NumTreatments )
    nError          <- 0
    vHRRatio        <- rep( NA, DesignParam$NumTreatments )
    dTimeOfAnalysis <- NA

    # Step 2 - Retrieve design and interim analysis information ####
    # If interim look information is supplied use the current look specific
    # efficacy boundaries and event counts. Otherwise use the fixed sample settings
    if( !is.null( LookInfo ) )
    {

        # Example interim design setup
        nQtyOfLooks             <- LookInfo$NumLooks
        nLookIndex              <- LookInfo$CurrLookIndex
        vEfficacyBoundary       <- LookInfo$EffBdry[ nLookIndex ]

    } else {

        # Example fixed sample setup
        nQtyOfLooks             <- 1
        nLookIndex              <- 1
        vEfficacyBoundaryPScale <- DesignParam$Alpha
    }

    # Step 3 - Implement the analysis logic ####

    # Step 4 - Error checking ####
    # Add any required validation checks and update the error code if needed

    # Step 5 - Build the return object ####
    lReturn <- list( Decision     = as.integer( vDecision ),
                     ErrorCode    = as.integer( nError ),
                     HR           = as.double( vHRRatio ),
                     AnalysisTime = as.double( dTimeOfAnalysis ) )

    return( lReturn )

}
