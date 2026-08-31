######################################################################################################################## .
#' @name AnalyzeUsingPropTest
#' @title Analyze using the prop.test function in base R.
#' @author J. Kyle Wathen and Gabriel Potvin
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo A list containing input parameters related to multiple looks, which the user may need to compute
#'                 test statistics and perform tests. Users should access the variables using their names
#'                 (e.g., `LookInfo$NumLooks`) rather than by their order. Important variables in group sequential designs include:
#'
#'                 - `LookInfo$NumLooks`: An integer representing the number of looks in the study.
#'                 - `LookInfo$CurrLookIndex`: An integer representing the current index look, starting from 1.
#'                 - `LookInfo$CumEvents`: A vector of length `LookInfo$NumLooks`, containing the cumulative number of events at each look.
#'                 - `LookInfo$RejType`: A code representing rejection types. Possible values are:
#'                   - **Efficacy Only:**
#'                     - `0`: 1-Sided Efficacy Upper.
#'                     - `2`: 1-Sided Efficacy Lower.
#'                   - **Futility Only:**
#'                     - `1`: 1-Sided Futility Upper.
#'                     - `3`: 1-Sided Futility Lower.
#'                   - **Efficacy and Futility:**
#'                     - `4`: 1-Sided Efficacy Upper and Futility Lower.
#'                     - `5`: 1-Sided Efficacy Lower and Futility Upper.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' @description This example utilizes the prop.test function in base R to perform the analysis. The p-value from prop.test is used to compute the Z statistic that is compared to the upper boundary computed and sent by East Horizon as an input.
#'              This example does NOT include a futility rule.
#'
#' @return A named list containing `TestStat`, `ErrorCode`, and `Decision`.
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
######################################################################################################################## .

AnalyzeUsingPropTest<- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Step 1: Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
    if( !is.null( LookInfo ) )
    {
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        nRejType             <- LookInfo$RejType
        nTailType            <- DesignParam$TailType
    }
    else
    {
        nQtyOfLooks          <- 1
        nLookIndex           <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
        nTailType            <- DesignParam$TailType
    }

    # Create the vector of simulated data for this IA - East Horizon sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]

    # Create vectors of data for each treatment
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]

    # Perform the desired analysis
    mData                <- cbind( table( vOutcomesS ), table( vOutcomesE ) )
    lAnalysisResult      <- prop.test( mData, alternative = "greater", correct = FALSE )
    dPValue              <- lAnalysisResult$p.value
    dZValue              <- qnorm( 1 - dPValue )
    dBoundary            <- ifelse( is.null( LookInfo ), DesignParam$CriticalPoint,
                                    LookInfo$EffBdryUpper[ nLookIndex ] )

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                               bIAEfficacyCondition = dZValue > dBoundary,
                                               bFAEfficacyCondition = dZValue > dBoundary )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    nError     <- 0

    return( list( TestStat = as.double( dZValue ), ErrorCode = as.integer( nError ), Decision = as.integer( nDecision ) ) )
}
