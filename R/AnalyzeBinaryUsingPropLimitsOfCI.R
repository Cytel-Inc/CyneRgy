######################################################################################################################## .
#' @name AnalyzeBinaryUsingPropLimitsOfCI
#' @title Analyze Binary Data Using Proportion Limits of Confidence Interval
#' 
#' @description This function analyzes binary data using a simplified confidence interval (CI) limits design. 
#' It determines whether to make a "Go" or "No Go" decision based on the treatment difference 
#' and user-specified thresholds for the CI lower and upper limits. The analysis uses the `prop.test` 
#' function from base R to compute CIs at a user-defined confidence level.
#'
#' The decision logic is as follows:
#' 
#' - If the lower limit (LL) of the CI is greater than `UserParam$dLowerLimit`, a "Go" decision is made.
#' - If a "Go" decision is not made, and the upper limit (UL) of the CI is less than `UserParam$dUpperLimit`, a "No Go" decision is made.
#' - Otherwise, continue to the next analysis.
#' - At the final analysis:
#'      - If LL > `UserParam$dLowerLimit`, a "Go" decision is made.
#'      - Otherwise, a "No Go" decision is made.
#'              
#' @param SimData A data frame containing the data generated in the current simulation.
#' @param DesignParam A list of design and simulation parameters required for the analysis.
#' @param LookInfo A list containing input parameters related to multiple looks, which are used to compute test statistics 
#' and perform tests. Important variables include:
#'
#' - `LookInfo$NumLooks`: Integer, number of looks in the study.
#' - `LookInfo$CurrLookIndex`: Integer, current look index (starting from 1).
#' - `LookInfo$CumEvents`: Vector, cumulative number of events at each look.
#' - `LookInfo$RejType`: Code representing rejection types. Possible values include:
#'  - **Efficacy Only:**
#'      - `0`: 1-Sided Efficacy Upper.
#'      - `2`: 1-Sided Efficacy Lower.
#'  - **Futility Only:**
#'      - `1`: 1-Sided Futility Upper.
#'      - `3`: 1-Sided Futility Lower.
#'  - **Efficacy and Futility:**
#'      - `4`: 1-Sided Efficacy Upper and Futility Lower.
#'      - `5`: 1-Sided Efficacy Lower and Futility Upper.
#'
#' @param UserParam A list of user-defined parameters with the following required elements:
#' \describe{
#'   \item{dLowerLimit}{A value (0,1) specifying the lower limit, e.g., Minimum Acceptable Value (MAV).}
#'   \item{dUpperLimit}{A value (0,1) specifying the upper limit for the confidence interval, e.g., Target Value (TV).}
#'   \item{dConfLevel}{A value (0,1) specifying the confidence level for the `prop.test` function.}
#' }
#' 
#' @return A list containing the following elements:
#'  \describe{
#'      \item{TestStat}{A double representing the computed test statistic.}
#'      \item{Decision}{Required integer value indicating the decision made:
#'                      \describe{
#'                        \item{0}{No boundary crossed (neither efficacy nor futility).}
#'                        \item{1}{Lower efficacy boundary crossed.}
#'                        \item{2}{Upper efficacy boundary crossed.}
#'                        \item{3}{Futility boundary crossed.}
#'                        \item{4}{Equivalence boundary crossed.}
#'                      }}
#'      \item{ErrorCode}{Optional integer value:
#'                      \describe{
#'                        \item{0}{No error.}
#'                        \item{> 0}{Non-fatal error; current simulation is aborted but subsequent simulations continue.}
#'                        \item{< 0}{Fatal error; no further simulations are attempted.}
#'                      }}
#'      \item{Delta}{Estimated difference between experimental and control treatments.}
#'  }
#'  
#' @note In this example, the boundary information computed and sent from East or East Horizon is ignored 
#'       to implement this decision approach.
#' @export
######################################################################################################################## .

AnalyzeBinaryUsingPropLimitsOfCI <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Step 1: Retrieve necessary information from the objects East or East Horizon sent. You may not need all the variables ####
    if(  !is.null( LookInfo )  )
    {
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfLooks          <- LookInfo$NumLooks
        nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        RejType              <- LookInfo$RejType
        TailType             <- DesignParam$TailType
    }
    else
    {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfEvents         <- DesignParam$MaxCompleters
        nQtyOfPatsInAnalysis <- nrow( SimData )
        TailType             <- DesignParam$TailType
    }
    
    # Create a fatal error when user parameters are missing to avoid misleading results
    vRequiredParams <- c( "dLowerLimit", "dUpperLimit", "dConfLevel" )
    vMissingParams <- vRequiredParams[ !vRequiredParams %in% names( UserParam ) ]
    
    if( is.null( UserParam ) || length( vMissingParams ) > 0 )
    {
        return( list( TestStat  = as.double( 0 ), 
                      ErrorCode = as.integer( -1 ), 
                      Decision  = as.integer( 0 ),
                      Delta     = as.double( 0 ) ) )
    }
    
    # Create the vector of simulated data for this IA - East or East Horizon sends all of the simulated data ####
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Create vectors of data for each treatment  ####
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]
    
    # Perform the desired analysis, then determine if the lower limit of the confidence interval is greater than the user-specified value ####
    mData                <- cbind( table( vOutcomesS ), table( vOutcomesE ) ) 
    lAnalysisResult      <- prop.test( mData, alternative = "two.sided", correct = FALSE, conf.level = UserParam$dConfLevel )
    dLowerLimitCI        <- lAnalysisResult$conf.int[ 1 ]
    dUpperLimitCI        <- lAnalysisResult$conf.int[ 2 ]
    
    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, 
                                               bIAEfficacyCondition = dLowerLimitCI > UserParam$dLowerLimit,
                                               bIAFutilityCondition = dUpperLimitCI < UserParam$dUpperLimit,
                                               bFAEfficacyCondition = dLowerLimitCI > UserParam$dLowerLimit )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    
    Error 	<- 0
    
    return( list( TestStat  = as.double( dLowerLimitCI ), 
                  ErrorCode = as.integer( Error ), 
                  Decision  = as.integer( nDecision ),
                  Delta     = as.double( lAnalysisResult$estimate[ 1 ] - lAnalysisResult$estimate[ 2 ] ) ) )
}