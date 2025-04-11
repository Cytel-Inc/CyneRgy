######################################################################################################################## .
#' @name AnalyzeContinuousUsingMeanLimitsOfCI
#' @title Analyze Continuous Data Using Mean Limits of Confidence Interval
#' 
#' @description This function performs analysis using a simplified limits of confidence interval design for continuous outcomes. 
#' The analysis determines whether a "Go" or "No-Go" decision is made based on the lower and upper limits of 
#' a user-specified confidence interval. It uses the `t.test` function from the base R library to compute the 
#' confidence interval. The decision-making process is based on the following logic:
#'
#' - If the lower limit of the confidence interval (LL) is greater than the Minimum Acceptable Value (MAV), a "Go" decision is made.
#' - If a "Go" decision is not made, and the upper limit of the confidence interval (UL) is less than the Target Value (TV), a "No-Go" decision is made.
#' - Otherwise, continue to the next analysis.
#' - At the final analysis, if LL > MAV, a "Go" decision is made; otherwise, a "No-Go" decision is made.
#'
#' This function assumes MAV ≤ TV and ignores boundary information sent from East or East Horizon to implement this decision approach.
#'
#' @param SimData Data frame consisting of data generated in the current simulation.
#' @param DesignParam List of design and simulation parameters required to perform the analysis.
#' @param LookInfo A list of input parameters related to multiple looks in group sequential designs. 
#' Variables should be accessed by names (e.g., `LookInfo$NumLooks`). Important variables include:
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
#' @param UserParam A list of user-defined parameters. Must contain the following named elements:
#' \describe{
#'   \item{UserParam$dMAV}{Numeric; specifies the Minimum Acceptable Value (MAV).}
#'   \item{UserParam$dTV}{Numeric; specifies the Target Value (TV).}
#'   \item{UserParam$dConfLevel}{Numeric (0,1); specifies the confidence level for the `t.test()` function.}
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
#' @note This function is applicable only when MAV ≤ TV. 
#' @export
######################################################################################################################## .

AnalyzeContinuousUsingMeanLimitsOfCI <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Step 1: Retrieve necessary information from the objects East or East Horizon sent. You may not need all the variables ####
    if(  !is.null( LookInfo )  )
    {
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfLooks          <- LookInfo$NumLooks
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        RejType              <- LookInfo$RejType
        TailType             <- DesignParam$TailType
    }
    else
    {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
        TailType             <- DesignParam$TailType
    }
    
    # Create a fatal error when user parameters are missing to avoid misleading results
    vRequiredParams <- c( "dMAV", "dTV", "dConfLevel" )
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
    # delta = mean(E) - mean(S). Change the alternative if delta < 0, put alternative = "less"
    # var.equal = TRUE assumes the variances of both arms to be same, so the intermediate computations uses Pooled std deviation estimate. This will be consistent with Example - 1 & 2.
    
    lAnalysisResult       <- t.test( vOutcomesE, vOutcomesS, alternative = "greater",
                                     var.equal = TRUE, conf.level = UserParam$dConfLevel )
    dLowerLimitCI        <- lAnalysisResult$conf.int[ 1 ]
    dUpperLimitCI        <- lAnalysisResult$conf.int[ 2 ]
    
    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, 
                                               bIAEfficacyCondition = dLowerLimitCI > UserParam$dMAV,
                                               bIAFutilityCondition = dUpperLimitCI < UserParam$dTV,
                                               bFAEfficacyCondition = dLowerLimitCI > UserParam$dMAV )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    
    Error 	<- 0
    
    return( list( TestStat  = as.double( dLowerLimitCI ), 
                  ErrorCode = as.integer( Error ), 
                  Decision  = as.integer( nDecision ),
                  Delta     = as.double( lAnalysisResult$estimate[ 1 ] - lAnalysisResult$estimate[ 2 ] ) ) )
}