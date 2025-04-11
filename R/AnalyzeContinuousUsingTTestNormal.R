######################################################################################################################## .
#' @name AnalyzeContinuousUsingTTestNormal
#' @title Analyze Continuous Data Using t-Test
#' 
#' @description Performs hypothesis testing using the `t.test()` function in base R to analyze continuous data under the assumption of a normal distribution. This function demonstrates how analysis and decision-making can be modified in a simple approach. The test statistic is compared to the upper boundary computed and sent by East as an input. Note that this example does not include a futility rule.
#' 
#' @param SimData Data frame that contains simulated data for the current simulation.
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
#' @param UserParam A list of user-defined parameters in East or East Horizon. The default is `NULL`.
#' For this example, user-defined parameters are not included.
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
#'  }
#' @export
######################################################################################################################## .

AnalyzeContinuousUsingTTestNormal <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{   
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
    
    # compute the estimates for mean for (E and S)
    dMeanOfResponsesOnE   <- mean( vOutcomesE )
    dMeanOfResponsesOnS   <- mean( vOutcomesS )
    
    # delta = mean(E) - mean(S). Change the alternative if delta < 0, put alternative = "less"
    delta <- dMeanOfResponsesOnE - dMeanOfResponsesOnS
    alternativeHypothesis <- ifelse( delta < 0, "less", "greater" )
    
    # Assumes the variances of both arms to be same, so the intermediate computations uses Pooled std deviation estimate.
    lAnalysisResult       <- t.test( vOutcomesE, vOutcomesS, alternative = alternativeHypothesis,
                                     var.equal = TRUE )
    
    dTValue              <- lAnalysisResult$statistic    # extract t-test statistic value
    dBoundary            <- ifelse( is.null( LookInfo ), DesignParam$CriticalPoint, LookInfo$EffBdryUpper[ nLookIndex ] )
    
    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, 
                                               bIAEfficacyCondition = dTValue > dBoundary, 
                                               bFAEfficacyCondition = dTValue > dBoundary )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    
    Error <-  0
    
    lRet <- list( TestStat = as.double( dTValue ),
                  Decision  = as.integer( nDecision ), 
                  ErrorCode = as.integer( Error ) )
    return( lRet )
}