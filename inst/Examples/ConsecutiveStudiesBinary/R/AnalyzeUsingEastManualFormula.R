######################################################################################################################## .
#' @name AnalyzeUsingEastManualFormula
#' @title Compute the statistic using formula 28.2 in the East manual.
#' @description Use formula 28.2 from the East manual to compute a binary-response test statistic and compare it with
#' the efficacy boundary supplied by East Horizon. This example does not implement a futility rule.
#' @author Gabriel Potvin, Valeria A. G. Mazzanti, J. Kyle Wathen
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
#' @return A named list containing `TestStat`, `ErrorCode`, `Decision`, `Delta`, `TrueDelta`,
#' `TrueProbabilityControl`, and `TrueProbabilityExperimental`.
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

AnalyzeUsingEastManualFormula <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Step 1: Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
    if( !is.null( LookInfo ) )
    {
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfLooks          <- LookInfo$NumLooks
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        nRejType             <- LookInfo$RejType
        nTailType            <- DesignParam$TailType
    }
    else
    {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
        nTailType            <- DesignParam$TailType
    }

    # Create the vector of simulated data for this IA - East Horizon sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]

    # Create vectors of data for each treatment - E is Experimental and S is Standard of Care
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]

    nQtyOfResponsesOnE   <- sum( vOutcomesE )
    nQtyOfPatsOnE        <- length( vOutcomesE )

    nQtyOfResponsesOnS   <- sum( vOutcomesS )
    nQtyOfPatsOnS        <- length( vOutcomesS )

    # Compute the estimates in equation 28.2 from the East user manual
    dPiHatExperimental   <- nQtyOfResponsesOnE / nQtyOfPatsOnE
    dPiHatControl        <- nQtyOfResponsesOnS / nQtyOfPatsOnS

    dPiHatj              <- ( nQtyOfResponsesOnE + nQtyOfResponsesOnS ) / ( nQtyOfPatsOnE + nQtyOfPatsOnS )

    # Equation 28.2 in East manual
    dZj                  <- ( dPiHatExperimental - dPiHatControl ) / sqrt( dPiHatj * ( 1 - dPiHatj ) * ( 1 / nQtyOfPatsOnE + 1 / nQtyOfPatsOnS ) )
    dBoundary            <- ifelse( is.null( LookInfo ), DesignParam$CriticalPoint, LookInfo$EffBdryUpper[ nLookIndex ] )

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                               bIAEfficacyCondition = dZj > dBoundary,
                                               bFAEfficacyCondition = dZj > dBoundary )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    nError <- 0

    # Return TrueDelta, TrueProbabilityControl, TrueProbabilityExperimental
    return( list( TestStat  = as.double( dZj ),
                ErrorCode = as.integer( nError ),
                Decision  = as.integer( nDecision ),
                Delta     = as.double( dPiHatExperimental - dPiHatControl ),
                TrueDelta = as.double( SimData$TrueProbabilityExperimental[ 1 ] - SimData$TrueProbabilityControl[ 1 ] ),
                TrueProbabilityControl = as.double( SimData$TrueProbabilityControl[ 1 ] ),
                TrueProbabilityExperimental = as.double( SimData$TrueProbabilityExperimental[ 1 ] ) ) )
}
