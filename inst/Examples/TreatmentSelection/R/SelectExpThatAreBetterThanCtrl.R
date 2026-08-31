######################################################################################################################## .
#' @name SelectExpThatAreBetterThanCtrl
#' @title Select Treatments with Response Rates Above Control
#' @description
#' Selects every experimental arm with an observed response rate above control.
#' If no arm meets that rule, selects the experimental arm with the highest rate.
#' @author Sydney Ringold, J. Kyle Wathen
#' @param SimData Dataframe which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#' @param LookInfo List containing parameters for the current analysis look.
#' @param UserParam A list of user defined parameters in East Horizon. The default must be NULL.
#' @return A list containing `TreatmentID`, the selected experimental-arm indexes;
#'   `AllocRatio`, their allocation ratios relative to control; and integer `ErrorCode`.
######################################################################################################################## .

SelectExpThatAreBetterThanCtrl <- function( SimData, DesignParam, LookInfo, UserParam = NULL )
{
    # Calculate the number of responders and treatment failures for each treatment

    # The next lines create a table where each treatment is in a row, number of treatment failures is the first column, and number of responses is the second column.
    tabResults   <- table( SimData$TreatmentID, SimData$Response )

    # Compute the response probability as # of responses/(  # of treatment failures + # of responses )
    vProbabilityResponse                <- as.vector( tabResults[ , 2 ]/( tabResults[ , 1 ] + tabResults[ , 2 ] ) )

    # Create a variable with the probability of response on control to be used in decision making
    dProbabilityOfResponseOnControl     <- vProbabilityResponse[ 1 ]
    # Create vector with only the estimated probability of response on experimentals
    vProbabilityResponseOnExperimental  <- vProbabilityResponse[ c( 2:length( vProbabilityResponse ) ) ]

    # Note: vProbabilityResponseOnExperimental now contains only the response rates for the experimental treatments

    # Selection Rule: Any treatment with a response rate that is higher than control is selected for stage 2
    vReturnTreatmentID <- c()
    # Note: Start with row 2, which is experimental treatment 1
    for( nIndex in 1:length( vProbabilityResponseOnExperimental ) )
    {
        # If the response rate > response rate on control, add the treatment ID to the list
        if( vProbabilityResponseOnExperimental[ nIndex ] > dProbabilityOfResponseOnControl )
            vReturnTreatmentID <- c( vReturnTreatmentID, nIndex )

    }

    # If none of the experimental treatments had a response rate greater than control, select the treatment with the largest response rate
    if( length( vReturnTreatmentID ) == 0 )
    {
        vReturnTreatmentID <-  which.max( vProbabilityResponseOnExperimental )
    }

    # We want all treatments to have a randomization ratio of 1
    # The allocation will put twice as many on the treatment with the highest number of responses,
    # eg. the Treatment vReturnTreatmentID[ 1 ] will receive twice as many patients as vReturnTreatmentID[ 2 ]
    vAllocationRatio   <- rep( 1, length( vReturnTreatmentID ) )

    # Treatment vReturnTreatmentID[ 1 ] will have a ratio of 2, vReturnTreatmentID[ 2 ] a ratio of 1, and control is always 1

    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length( vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        #  Fatal error because the R code is incorrect
        nErrorCode <- -1
    }

    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ) ,
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )

    return( lReturn )

}
