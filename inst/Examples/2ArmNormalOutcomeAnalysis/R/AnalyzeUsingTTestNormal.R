#  Last Modified Date: 04/30/2024
#' @name AnalyzeUsingTTestNormal
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL.
#' @description Use the t.test() function in the base package to compute the statistic. The purpose of this example is to demonstrate how the analysis and decision making can be modified in a simple approach.  
#'              The test statistic is compared to the upper boundary computed and sent by East as an input. This example does NOT include a futility rule. 
#' @return TestStat A double value of the computed test statistic
#' @return Decision An integer value: Decision = 0 --> No boundary crossed
#'                                    Decision = 1 --> Lower Efficacy Boundary Crossed
#'                                    Decision = 2 --> Upper Efficacy Boundary Crossed
#'                                    Decision = 3 --> Futility Boundary Crossed
#'                                    Decision = 4 --> Equivalence Boundary Crossed
#' @return AnalysisTime Optional Numeric value. Estimate of Analysis time. Same as look time for interims. Same as study duration for the final analysis. To be computed and returned by the user.
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Nonfatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted

#'@note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.
#' @export
#' 
#' 
#######################################################################################################################################################################################################################

AnalyzeUsingTTestNormal <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{   
    library(CyneRgy)
    
    # Step 1: Retrieve necessary information from the objects East sent. You may not need all the variables ####
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
    # Create the vector of simulated data for this IA - East sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Create vectors of data for each treatment - E is Experimental and S is Standard of Care 
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]
    
    # compute the estimates for mean and Std. Dev for (E and S)
    dMeanOfResponsesOnE   <- mean( vOutcomesE )
    dStdDevOfResponsesOnE <- sd( vOutcomesE)
    nQtyOfPatsOnE         <- length( vOutcomesE )
    
    dMeanOfResponsesOnS   <- mean( vOutcomesS )
    dStdDevOfResponsesOnS <- sd( vOutcomesS)
    nQtyOfPatsOnS         <- length( vOutcomesS )
    

    # delta = mean(E) - mean(S). Change the alternative if delta < 0, put alternative = "less"
    # var.equal = TRUE assumes the variances of both arms to be same, so the intermediate computations
    # uses Pooled std deviation estimate. This will be consistent with Example - 1.
    lAnalysisResult       <- t.test( vOutcomesE, vOutcomesS, alternative = "greater",
                                     var.equal = TRUE)
    
    dTValue              <- lAnalysisResult$statistic    # extract t-test statistic value
    dBoundary            <- ifelse( is.null( LookInfo ), DesignParam$CriticalPoint, LookInfo$EffBdryUpper[ nLookIndex ] )
    
    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, 
                                               bIAEfficacyCondition = dTValue > dBoundary, 
                                               bFAEfficacyCondition = dTValue > dBoundary)
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    
    Error <-  0
    
    
    lRet <- list( TestStat = as.double( dTValue ),
                  Decision  = as.integer( nDecision ), 
                  ErrorCode = as.integer( Error ))
    return( lRet )
}

