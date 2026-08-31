######################################################################################################################## .
#' @name AnalyzeUsingEastManualFormulaNormal
#' @title Compute a Two-Sample Normal Test Statistic Using the East Manual Formula
#' @author Shubham Lahoti, J. Kyle Wathen, and Gabriel Potvin
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'
#' @description Use the formula Q.3.3 in the East manual to compute the statistic. The purpose of this example is to demonstrate how the analysis and decision making can be modified in a simple approach.
#'              The test statistic is compared to the upper boundary computed and sent by East Horizon as an input. This example does NOT include a futility rule.
#'              Two sample Z test for Normal distribution. Number of Looks > 1.
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

AnalyzeUsingEastManualFormulaNormal <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
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

    # compute the estimates for mean and Std. Dev for (E and S)
    dMeanOfResponsesOnE   <- mean( vOutcomesE )
    dStdDevOfResponsesOnE <- sd( vOutcomesE )
    nQtyOfPatsOnE        <- length( vOutcomesE )

    dMeanOfResponsesOnS   <- mean( vOutcomesS )
    dStdDevOfResponsesOnS <- sd( vOutcomesS )
    nQtyOfPatsOnS        <- length( vOutcomesS )

    # Equation from Appendix Q - 3.3 in East manual for the estimate of Pooled Std. Deviation
    dStdDevPooled        <- sqrt( ( ( nQtyOfPatsOnE - 1 ) * dStdDevOfResponsesOnE ^ 2 + ( nQtyOfPatsOnS - 1 ) * dStdDevOfResponsesOnS ^ 2 ) / ( nQtyOfPatsOnE + nQtyOfPatsOnS - 2 ) )

    # Equation from Appendix Q - 3.3 in East manual
    dZj                  <- ( dMeanOfResponsesOnE - dMeanOfResponsesOnS ) / ( dStdDevPooled * sqrt( 1 / nQtyOfPatsOnE + 1 / nQtyOfPatsOnS ) )
    dBoundary            <- ifelse( is.null( LookInfo ), DesignParam$CriticalPoint, LookInfo$EffBdryUpper[ nLookIndex ] )

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                               bIAEfficacyCondition = dZj > dBoundary,
                                               bFAEfficacyCondition = dZj > dBoundary )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    nError <- 0

    return( list( TestStat = as.double( dZj ), ErrorCode = as.integer( nError ), Decision = as.integer( nDecision ) ) )
}
