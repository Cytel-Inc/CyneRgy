######################################################################################################################## .
#' @name AnalyzeTTEUsingHazardRatioLimitsOfCI
#' @title Analyze Time-to-Event Data Using Hazard Ratio Limits of Confidence Interval
#' 
#' @description This function analyzes time-to-event data using a simplified design based on upper and lower confidence boundaries for hazard ratios (HR). 
#' It determines whether to make a "Go" or "No Go" decision by assessing the likelihood of the HR being below specified limits. 
#' Specifically, the function utilizes the `coxph()` model from the survival package to estimate the log hazard ratio and its standard error.
#' The decision-making process is as follows:
#' 
#' - If the upper limit (UL) of the confidence interval is below the Minimum Acceptable Value (MAV), a "Go" decision is made.
#' - If the lower limit (LL) of the confidence interval is above the Target Value (TV), a "No Go" decision is made.
#' - Otherwise, the analysis continues to the next look.
#' 
#' At the final analysis:
#' - If UL < MAV, a "Go" decision is made.
#' - Otherwise, a "No Go" decision is made.
#' 
#' HR and log HR are monotonically related. Since `coxph()` outputs results for log HR, the function uses the log HR scale for decision-making.
#' 
#' @param SimData Data frame consisting of data generated in the current simulation.
#' @param DesignParam List of design and simulation parameters required to perform the analysis.
#' @param LookInfo A list containing input parameters related to multiple looks. Users should access variables using their names, such as:
#' 
#' - `LookInfo$NumLooks`: Integer representing the number of looks in the study.
#' - `LookInfo$CurrLookIndex`: Integer representing the current index look, starting from 1.
#' - `LookInfo$CumEvents`: Vector of cumulative number of events at each look.
#' - `LookInfo$RejType`: Code representing rejection types, with possible values:
#'   - **Efficacy Only**:
#'     - `0`: 1-Sided Efficacy Upper.
#'     - `2`: 1-Sided Efficacy Lower.
#'   - **Futility Only**:
#'     - `1`: 1-Sided Futility Upper.
#'     - `3`: 1-Sided Futility Lower.
#'   - **Efficacy and Futility**:
#'     - `4`: 1-Sided Efficacy Upper and Futility Lower.
#'     - `5`: 1-Sided Efficacy Lower and Futility Upper.
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
#'      \item{HazardRatio}{A double representing the computed hazard ratio.}
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
#'  }
#' @export
######################################################################################################################## .

AnalyzeTTEUsingHazardRatioLimitsOfCI <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{   
    # Step 1: Retrieve necessary information from the objects East or East Horizon sent. You may not need all the variables ####
    if( !is.null( LookInfo ) )
    {
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        CumEvents            <- LookInfo$InfoFrac*DesignParam$MaxEvents
        nQtyOfEvents         <- CumEvents[ nLookIndex ]
        dEffBdry             <- LookInfo$EffBdryLower[ nLookIndex ]
        RejType              <- LookInfo$RejType
        TailType             <- DesignParam$TailType
    }
    else
    {
        nQtyOfLooks          <- 1
        nLookIndex           <- 1
        nQtyOfEvents         <- DesignParam$MaxEvents
        dEffBdry             <- DesignParam$CriticalPoint
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
                      Delta     = as.double( 0 )))
    }
    
    SimData$TimeOfEvent      <- SimData$ArrivalTime + SimData$SurvivalTime    # This is the calendar time in the trial that the patients event is observed
    
    # Compute the time of analysis 
    SimData                  <- SimData[ order( SimData$TimeOfEvent), ]
    dTimeOfAnalysis          <- SimData[ nQtyOfEvents, ]$TimeOfEvent
    
    # Add the Observed Time variable 
    SimData                  <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ]   # Exclude any patients that were not enrolled by the time of the analysis
    SimData$Event            <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )  # If the event is observed after the analysis it is not observed, eg censored 
    SimData$ObservedTime     <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )
    
    # Order the data by observed time for the remainder of the computations
    SimData                  <- SimData[ order( SimData$ObservedTime ), ]
    
    # Compute Observed HR
    cCoxModel                <- survival::coxph( survival::Surv( ObservedTime, Event ) ~ TreatmentID, data = SimData )
    
    # find the value of observed Log(HR) and SE(log(HR))
    dLogHR                   <- summary( cCoxModel )$coefficients[ 1 ]
    dStdError                <- summary( cCoxModel )$coefficients[ 3 ]
    
    # Log HR follows Normal distribution with mean = observed log HR on line no 83 and Std error given on line no 84
    # Critical value for Z test is given as,
    dAlpha                   <- 1 - UserParam$dConfLevel
    dZalpha                  <- qnorm( 1 - dAlpha/2 )
    
    # Confidence Interval for log HR is given as,
    dLowerLimitCI                   <- dLogHR - dZalpha * dStdError
    dUpperLimitCI                   <- dLogHR + dZalpha * dStdError
    
    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, 
                                               bIAEfficacyCondition = dUpperLimitCI < log( UserParam$dMAV ),
                                               bIAFutilityCondition = dLowerLimitCI > UserParam$dTV,
                                               bFAEfficacyCondition = dUpperLimitCI < log( UserParam$dMAV ) )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    
    Error 	<- 0
    
    return( list( HazardRatio  = as.double( exp( dLogHR ) ), 
                  ErrorCode = as.integer( Error ), 
                  Decision  = as.integer( nDecision ) ) )
}