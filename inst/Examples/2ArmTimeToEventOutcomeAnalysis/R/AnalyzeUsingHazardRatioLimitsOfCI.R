#' @param AnalyzeUsingHazardRatioLimitsOfCI
#' @title Analyze using a simplified limits of confidence interval design
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East or East Horizon. UserParam must be supplied, the list must contain the following named elements:
#' \describe{
#'   \item{UserParam$dMAV}{A value (0, Inf) that specifics the lower limit, eg  Minimum Acceptable Value (MAV). }
#'   \item{UserParam$dTV}{A value (0 Inf) that specifies the upper limit for the confidence interval, eg Target Value (TV).}
#'   \item{UserParam$dConfLevel}{A value (0,1) that specifies the confidence level for the t.test() function in base R library.}
#' }
#' @description  In this simplified example of upper and lower confidence boundary designs, if it is likely that the HR (Hazard Ratio) is below the Minimum Acceptable Value (MAV) then a Go decision is made.  
#'               If a Go decision is not made, then if it is unlikely that the Hazard ratio is below the Target Value (TV) a No Go decision is made.      
#'               In this example, the coxph() from survival package in R is utilized to analyze the data and compute estimate of log HR and Std error of log HR. 
#'               The team would like to make a Go decision if there is at least a 90% chance that HR is below than the MAV.  
#'               If a Go decision is not made, then a No Go decision is made if there is less than a 10% chance the HR is less than the TV.  
#'          
#'               Specifically, if user provides upper and lower limit in Hazard ratio scale then,
#'               1. For Hazard Ratio 
#'                  if UL < UserParam$dMAV --> Go 
#'                  if LL > UserParam$dTV --> No Go
#'                  Otherwise, continue to the next analysis. 
#'                  
#'              2. For log Hazard Ratio 
#'                  if UL < UserParam$dMAV --> Go 
#'                  if LL > UserParam$dTV --> No Go
#'               Otherwise, continue to the next analysis. 
#'  Note - HR and log HR are monotonically related.
#'  In coxph() function, we get the analysis for log HR and hence we make the use of 2) in decision making. 
#'               
#'  At the Final Analysis: If UL < UserParam$dMAV  then a Go decision is made, otherwise, a No Go decision is made.
#'               
#'               
#' @return Hazard Ratio :  A double value of the computed or observed Hazard ratio
#' @return Decision An integer value: Decision = 0 --> No boundary crossed
#'                                    Decision = 1 --> Lower Efficacy Boundary Crossed
#'                                    Decision = 2 --> Upper Efficacy Boundary Crossed
#'                                    Decision = 3 --> Futility Boundary Crossed
#'                                    Decision = 4 --> Equivalence Boundary Crossed
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Nonfatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted.

#'@note In this example, the boundary information that is computed and sent from East is ignored in order to implement this decision approach.
#'@note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.

################################################################################################################################################################################################
AnalyzeUsingHazardRatioLimitsOfCI <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL)
{   
    library(CyneRgy)
    library(survival)
    
    # Step 1: Retrieve necessary information from the objects East sent. You may not need all the variables ####
    if( !is.null( LookInfo ) )
    {
        # Look info was provided so use it
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        CumEvents            <- LookInfo$InfoFrac*DesignParam$MaxEvents
        nQtyOfEvents         <- CumEvents[ nLookIndex ]
        dEffBdry             <- LookInfo$EffBdryLower[ nLookIndex ]
        RejType              <- LookInfo$RejType
        TailType             <- DesignParam$TailType
    }else
    {   # Look info is not provided for fixed sample designs so fetch the information appropriately
        nQtyOfLooks          <- 1
        nLookIndex           <- 1
        nQtyOfEvents         <- DesignParam$MaxEvents
        dEffBdry             <- DesignParam$CriticalPoint
        TailType             <- DesignParam$TailType
    }
    
    SimData$TimeOfEvent      <- SimData$ArrivalTime + SimData$SurvivalTime    # This is the calendar time in the trial that the patients event is observed
    
    # Compute the time of analysis 
    SimData                  <- SimData[ order( SimData$TimeOfEvent), ]
    dTimeOfAnalysis          <- SimData[ nQtyOfEvents, ]$TimeOfEvent
    
    # Add the Observed Time variable 
    SimData                  <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis ,]   # Exclude any patients that were not enrolled by the time of the analysis
    SimData$Event            <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )  # If the event is observed after the analysis it is not observed, eg censored 
    SimData$ObservedTime     <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )
    
    # Order the data by observed time for the remainder of the computations
    SimData                  <- SimData[ order( SimData$ObservedTime ), ]
    
    # Compute Observed HR
    cCoxModel                <- coxph( Surv( ObservedTime, Event ) ~ TreatmentID, data = SimData )
    
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
                                               bIAEfficacyCondition = dUpperLimitCI < log(UserParam$dMAV),
                                               bIAFutilityCondition = dLowerLimitCI > UserParam$dTV,
                                               bFAEfficacyCondition = dUpperLimitCI < log(UserParam$dMAV))
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    
    Error 	<- 0
    
    return( list( HazardRatio  = as.double( exp( dLogHR ) ), 
                  ErrorCode = as.integer( Error ), 
                  Decision  = as.integer( nDecision ) ) )
    
}