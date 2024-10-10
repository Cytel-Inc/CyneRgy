
#' @param AnalyzeUsingMeanLimitsOfCI
#' @title Analyze using a simplified limits of confidence interval design
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East or Solara. UserParam must be supplied, the list must contain the following named elements:
#' \describe{
#'   \item{UserParam$dMAV}{A value (-Inf, Inf) that specifics the Minimum Acceptable Value (MAV). }
#'   \item{UserParam$dTV}{A value (-Inf, Inf) that specifies the Target Value (TV).}
#'   \item{UserParam$dConfLevel}{A value (0,1) that specifies the confidence level for the t.test() function in base R library.}
#' }
#' @description  In this simplified example of upper and lower confidence boundary designs, if it is likely that the treatment difference is above 
#'               the Minimum Acceptable Value (MAV) then a Go decision is made.  
#'               If a Go decision is not made, then if is is unlikely that the treatment difference is above the Target Value (TV) a No Go decision is made.      
#'               In this example, the t.test() from base package in R is utilized to analyze the data and compute at user-specified confidence interval using UserParam$dConfLevel.  
#'               The team would like to make a Go decision if there is at least a 90% chance that the difference in treatment is greater than the MAV.  
#'               If a Go decision is not made, then a No Go decision is made if there is less than a 10% chance the difference is greater than the TV.  
#'               Using a frequentist CI an approximation to this design can be done by the logic described below.
#'               At an analysis, if the Lower Limit of the CI, denoted by LL, is greater than user-specified UserParam$dMAV then a Go decision is made.  
#'               
#'               If a Go decision is not made, then if the Upper Limit of the CI, denoted by UL, is less than user-specified UserParam$TV a No Go decision is made.  
#'               Specifically, 
#'                  if LL > UserParam$dMAV --> Go
#'                  if UL < UserParam$dTV --> No Go
#'               Otherwise, continue to the next analysis. 
#'               At the Final Analysis: If LL > UserParam$dMAV  then a Go decision is made, otherwise, a No Go decision is made
#' @return TestStat A double value of the computed test statistic
#' @return Decision An integer value: Decision = 0 --> No boundary crossed
#'                                    Decision = 1 --> Lower Efficacy Boundary Crossed
#'                                    Decision = 2 --> Upper Efficacy Boundary Crossed
#'                                    Decision = 3 --> Futility Boundary Crossed
#'                                    Decision = 4 --> Equivalence Boundary Crossed
#' @return Delta The difference in the estimates, is utilized in Solara to create the observed graph
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted
#'@note This function is only applicable to the case where MAV <= TV.  
#'       In this example, the boundary information that is computed and sent from East is ignored in order to implement this decision approach.
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
AnalyzeUsingMeanLimitsOfCI <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL)
{

    # Step 1 - Retrieve necessary information from the objects Cytel objects sent ####
    if(  !is.null( LookInfo )  )
    {
        # Group sequential design
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfLooks          <- LookInfo$NumLooks
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
    }
    else
    {
        # Fixed Design
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
    }
    
    
    if( is.null( UserParam ) )
    {
        
        # FATAL ERROR AS WE DON'T KNOW WHAT THE USER WANTS TO DO.  
        # Creating a FATAL error will avoid misleading results when UserParam is not supplied
        return(list(TestStat  = as.double(0), 
                    ErrorCode = as.integer(-1), 
                    Decision  = as.integer( 0 ),
                    Delta     = as.double( 0 )))
    }
    
    
    # Create the vector of simulated data for this IA - East sends all of the simulated data ####
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
    
    # Set look decision logic
    if( nLookIndex < nQtyOfLooks ) # Interim Analysis
    {
        if( dLowerLimitCI > UserParam$dMAV )
        {
            strDecision <- "Efficacy"
        }
        else if( dUpperLimitCI < UserParam$dTV )
        {
            strDecision <- "Futility"
        }
        else
        {
            strDecision <- "Continue"
        }
    }
    else # Final Analysis
    {
        if( dLowerLimitCI > UserParam$dMAV )
        {
            strDecision <- "Efficacy"
        }
        else
        {
            strDecision <- "Futility"
        }
    }
    
    nDecision <- GetDecision( strDecision, DesignParam, LookInfo )
    
    Error 	<- 0

    return( list( TestStat  = as.double( dLowerLimitCI ), 
                ErrorCode = as.integer( Error ), 
                Decision  = as.integer( nDecision ),
                Delta     = as.double( lAnalysisResult$estimate[1] - lAnalysisResult$estimate[2] ) ) )
}


