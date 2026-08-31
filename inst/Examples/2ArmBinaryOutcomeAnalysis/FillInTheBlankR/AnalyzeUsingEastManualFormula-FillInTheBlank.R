######################################################################################################################## .
#' @name AnalyzeUsingEastManualFormula
#' @title Compute the statistic using formula 28.2 in the East manual.
#' @author J. Kyle Wathen and Gabriel Potvin
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East Horizon. The default must be NULL. For this example, user defined parameters are not included.
#' @description Use the formula 28.2 in the East manual to compute the statistic.  The purpose of this example is to demonstrate how the analysis and decision making can be modified in a simple approach.
#'              The test statistic is compared to the upper boundary computed and sent by East Horizon as an input. This example does NOT include a futility rule.
#' @return After the blanks are completed, a named list containing `TestStat`, `ErrorCode`, and `Decision`.
######################################################################################################################## .

AnalyzeUsingEastManualFormula <- function( SimData, DesignParam, LookInfo, UserParam = NULL )
{
    # In this example, the majority of the code is provided.  The fill in the blank areas are noted by _____________________.
    # This is done to allow you to practice creating these examples. You will need to remove the ____________ and enter the correct code.
    # The fully worked examples are provided in the corresponding example R files.

    # Retrieve necessary information from the objects East Horizon sent
    nLookIndex           <- LookInfo$CurrLookIndex
    nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
    nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]

    # Create the vector of simulated data for this IA - East Horizon sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]

    # Create vectors of data for each treatment - E is Experimental and S is Standard of Care
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- ______[ vPatientTreatment == 1 ]

    nQtyOfResponsesOnE   <- sum( vOutcomesE )
    nQtyOfPatsOnE        <- length( vOutcomesE )

    nQtyOfResponsesOnS   <- sum( ______ )
    nQtyOfPatsOnS        <- length( vOutcomesS )

    # Compute the estimates in equation 28.2 from the East user manual
    dPiHatExperimental   <- nQtyOfResponsesOnE / nQtyOfPatsOnE
    dPiHatControl        <- _______ / nQtyOfPatsOnS

    dPiHatj              <- ( nQtyOfResponsesOnE + nQtyOfResponsesOnS ) / ( nQtyOfPatsOnE + nQtyOfPatsOnS )

    # Equation 28.2 in East manual
    dZj                  <- ( dPiHatExperimental - dPiHatControl ) / sqrt( dPiHatj * ( 1 - dPiHatj ) * ( 1 / nQtyOfPatsOnE + 1 / nQtyOfPatsOnS ) )

    # A decision of 2 means success, 0 means continue the trial
    nDecision            <- ifelse( dZj > LookInfo$EffBdryUpper[ nLookIndex ], 2, 0 )

    if( nDecision == 0 )
    {
        # For this example, there is NO futility check but this is left for consistency with other examples

    }

    nError <- 0

    return( list( TestStat = as.double( dZj ), ErrorCode = as.integer( nError ), ________ = as.integer( nDecision ) ) )
}
