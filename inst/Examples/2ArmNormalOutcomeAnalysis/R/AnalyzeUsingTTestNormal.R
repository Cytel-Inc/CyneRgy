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
#' @details
#' ## CyneRgy Decision Helpers
#'
#' The analysis may use `CyneRgy::GetDecisionString()` and
#' `CyneRgy::GetDecision()` to determine the decision returned to
#' East Horizon Explore.
#'
#' When these helpers are used, the following input fields are required
#' and MUST be included when generating sample/test data:
#'
#' DesignParam:
#'   - TailType: Integer indicating the direction of the statistical test.
#'       0 = Left-tailed
#'       1 = Right-tailed
#'
#' LookInfo (for group sequential designs, NULL for fixed designs):
#' When not NULL, must contain the following fields:
#'   - NumLooks: Total number of looks.
#'   - CurrLookIndex: Current look index, starting at 1.
#'   - RejType: Integer identifying which stopping boundaries are enabled.
#'       0 = 1-Sided Efficacy Upper
#'       1 = 1-Sided Futility Upper
#'       2 = 1-Sided Efficacy Lower
#'       3 = 1-Sided Futility Lower
#'       4 = 1-Sided Efficacy Upper and Futility Lower
#'       5 = 1-Sided Efficacy Lower and Futility Upper
#'       6 = 2-Sided Efficacy Only (not used in East Horizon Explore)
#'       7 = 2-Sided Futility Only (not used in East Horizon Explore)
#'       8 = 2-Sided Efficacy and Futility (not used in East Horizon Explore)
#'       9 = Equivalence (not used in East Horizon Explore)
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

