######################################################################################################################## .
#' @name SelectExpThatAreBetterThanCtrl
#' @title Select Treatments with Response Rates Above Control
#' @description
#' Provides a fill-in exercise that selects experimental arms with observed response
#' rates above control, with a highest-response-rate fallback.
#' @author Sydney Ringold, J. Kyle Wathen
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#' @param LookInfo List containing parameters for the current analysis look.
#' @return A list containing `TreatmentID`, the selected experimental-arm indexes;
#'   `AllocRatio`, their allocation ratios relative to control; and integer `ErrorCode`.
######################################################################################################################## .

SelectExpThatAreBetterThanCtrl <- function( SimData, DesignParam, LookInfo )
{
    # In this example, the majority of the code is provided.  The fill in the blank areas are noted by _____________________.
    # This is done to allow you to practice creating these examples. You will need to remove the ____________ and enter the correct code.
    # The fully worked examples are provided in the corresponding example R files.

    # Calculate the number of responders and treatment failures for each treatment

    # The next lines create a table where each treatment is in a row, number of treatment failures is the first column, and number of responses is the second column.
    tabResults   <- table( SimData$TreatmentID, SimData$Response )

    # Compute the response probability as # of responses/(  # of treatment failures + # of responses )
    vProbabilityResponse            <- as.vector( ____________[ , 2 ] / ( tabResults[ , 1 ] + tabResults[ , 2 ] ) )
    # Create a variable with the probability of response on control to be used in decision making
    dProbabilityOfResponseOnControl     <- ____________[ 1 ]
    # Create vector with only the estimated probability of response on experimentals
    vProbabilityResponseOnExperimental  <- ____________[ 2:length( vProbabilityResponse ) ]

    # Note: vProbabilityResponseOnExperimental now contains only the response rates for the experimental treatments

    # Selection Rule: Any treatment with a response rate that is higher than control is selected for stage 2
    vReturnTreatmentID <- c()
    # Note: Start with row 2, which is experimental treatment 1
    for( nIndex in 1:length( vProbabilityResponseOnExperimental ) )
    {
        # If the response rate > response rate on control, add the treatment ID to the list
        if( vProbabilityResponseOnExperimental[ nIndex ] > _____________ )
            vReturnTreatmentID <- c( vReturnTreatmentID, nIndex )

    }

    # If none of the experimental treatments had a response rate greater than control, select the treatment with the largest response rate
    if( length( ____________ ) == 0 )
    {
        vReturnTreatmentID <- which.max( vProbabilityResponseOnExperimental )
    }

    # We want all treatments to have a randomization ratio of 1
    # The allocation will put twice as many on the treatment with the highest number of responses,
    # eg. the Treatment vReturnTreatmentID[ 1 ] will receive twice as many patients as vReturnTreatmentID[ 2 ]
    vAllocationRatio   <- rep( 1, length( ____________ ) )

    # Treatment vReturnTreatmentID[ 1 ] will have a ratio of 2, vReturnTreatmentID[ 2 ] a ratio of 1, and control is always 1

    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length( vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        #  Fatal error because the R code is incorrect
        nErrorCode <- -1
    }

    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ),
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )

    return( lReturn )

}
