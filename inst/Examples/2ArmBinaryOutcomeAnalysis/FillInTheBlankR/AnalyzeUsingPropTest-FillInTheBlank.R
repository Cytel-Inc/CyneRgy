######################################################################################################################## .
#' @name AnalyzeUsingPropTest
#' @title Analyze using the prop.test function in base R.
#' @author J. Kyle Wathen and Gabriel Potvin
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East. The default must be NULL.
#' @description This example utilizes the prop.test function in base R to perform the analysis. The p-value from prop.test is used to compute the Z statistic that is compared to the upper boundary computed and sent by East as an input.
#'              This example does NOT include a futility rule.
#'
#' @return After the blanks are completed, a named list containing `TestStat`, `ErrorCode`, and `Decision`.
######################################################################################################################## .

AnalyzeUsingPropTest <- function( SimData, DesignParam, LookInfo, UserParam = NULL )
{
    # In this example, the majority of the code is provided.  The fill in the blank areas are noted by _____________________.
    # This is done to allow you to practice creating these examples. You will need to remove the ____________ and enter the correct code.
    # The fully worked examples are provided in the corresponding example R files.

    # Retrieve necessary information from the objects East sent
    nLookIndex           <- LookInfo$CurrLookIndex
    nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
    nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]

    # Create the vector of simulated data for this IA - East sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]

    # Create vectors of data for each treatment
    vOutcomesS           <- vPatientOutcome[ ________ == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]

    # Perform the desired analysis
    mData                <- cbind( table( vOutcomesS ), table( vOutcomesE ) )
    lAnalysisResult      <- prop.test( mData, alternative = "greater", correct = FALSE )
    dPValue              <- lAnalysisResult$p.value
    dZValue              <- qnorm( 1 - ______ )
    nDecision            <- ifelse( dZValue > LookInfo$EffBdryUpper[ nLookIndex ], 2, 0 ) # A decision of 2 means success, 0 means continue the trial

    if( nDecision == 0 )
    {
        # if needed, check futility

    }

    nError <- 0

    return( list( _______ = as.double( dZValue ), ErrorCode = as.integer( nError ), Decision = as.integer( nDecision ) ) )
}
