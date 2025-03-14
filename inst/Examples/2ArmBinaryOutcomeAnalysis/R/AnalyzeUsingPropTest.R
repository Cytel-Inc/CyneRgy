######################################################################################################################## .
#' @title Analyze Binary Data Using the prop.test Function
#' 
#' @description This function analyzes binary data using the `prop.test` function in base R. The calculated p-value from `prop.test` 
#' is used to compute the Z statistic, which is then compared to the upper boundary provided as an input by East. 
#' Note that this example does not include a futility rule.
#' 
#' @param SimData Data frame containing the data generated in the current simulation.
#' @param DesignParam List of design and simulation parameters required to perform the analysis.
#' @param LookInfo A list containing input parameters related to multiple looks, which the user may need to compute 
#' test statistics and perform tests. Users should access the variables using their names 
#' (e.g., `LookInfo$NumLooks`) rather than by their order. Important variables in group sequential designs include:
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
#' @param UserParam A list of user-defined parameters in East or East Horizon. Default is `NULL`. No user parameters are defined for this example. 
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
######################################################################################################################## .

AnalyzeUsingPropTest <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    library(CyneRgy)

    # Step 1: Retrieve necessary information from the objects East sent. You may not need all the variables ####
    if(  !is.null( LookInfo )  )
    {
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        RejType              <- LookInfo$RejType
        TailType             <- DesignParam$TailType
    }
    else
    {
        nQtyOfLooks          <- 1
        nLookIndex           <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
        TailType             <- DesignParam$TailType
    }
    
    # Create the vector of simulated data for this IA - East sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Create vectors of data for each treatment 
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]
    
    # Perform the desired analysis 
    mData                <- cbind(table(vOutcomesS), table(vOutcomesE))
    lAnalysisResult      <- prop.test(mData, alternative = "greater", correct = FALSE)
    dPValue              <- lAnalysisResult$p.value
    dZValue              <- qnorm( 1 - dPValue )
    dBoundary            <- ifelse( is.null( LookInfo ), DesignParam$CriticalPoint,
                                    LookInfo$EffBdryUpper[ nLookIndex] )

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, 
                                               bIAEfficacyCondition = dZValue > dBoundary, 
                                               bFAEfficacyCondition = dZValue > dBoundary)
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    
    Error 	= 0
    
    
    return(list(TestStat = as.double(dZValue), ErrorCode = as.integer(Error), Decision = as.integer( nDecision ) ))
}


