######################################################################################################################## .
#' @name SelectSpecifiedNumberOfExpWithHighestResponses
#' @title Select a Specified Number of Highest-Response Treatments
#' @description
#' Provides a fill-in exercise for selecting the requested number of experimental
#' arms with the largest response counts and assigning rank-specific allocation ratios.
#' @author Sydney Ringold, J. Kyle Wathen
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#' @param LookInfo List containing parameters for the current analysis look.
#' @param UserParam Optional list containing `maxSelection`, `highestResponse`,
#'   and `nextHighestResponse`.
#' @return A list containing `TreatmentID`, the selected experimental-arm indexes;
#'   `AllocRatio`, their allocation ratios relative to control; and integer `ErrorCode`.
######################################################################################################################## .

SelectSpecifiedNumberOfExpWithHighestResponses <- function( SimData, DesignParam, LookInfo, UserParam = NULL )
{
    # In this example, the majority of the code is provided.  The fill in the blank areas are noted by _____________________.
    # This is done to allow you to practice creating these examples. You will need to remove the ____________ and enter the correct code.
    # The fully worked examples are provided in the corresponding example R files.

    if( is.null( UserParam ) )
    {
        UserParam <- list( maxSelection = 2, highestResponse = 2, nextHighestResponse = 1 )
    }
    # Calculate the number of responses per arm and select the highest user-specified number (maxSelection) of arms
    tabResults   <- table( SimData$TreatmentID, SimData$Response )

    # Want to select the top user-specified (maxSelection) number of experimental treatments, so drop control from the sorting
    # Now, only the experimental treatments are left
    tabResults   <- ______[ -1, ]

    # Sort in descending order based on the number of responses (column 2)
    # After the sort, the matrix will have the largest number of responses in the first row and the smallest number of responses in the last row

    mSortedMatrix      <- tabResults[ order( tabResults[ , 2 ], decreasing = TRUE ), ]
    # Select the user-specified (maxSelection) number of treatments with the largest number of responses
    vReturnTreatmentID <- as.integer( row.names( mSortedMatrix[ 1:UserParam$max, ] ) )

    # The treatment with the highest number of responses should receive the user-specified highestResponse times as many patients as the next highest.
    # The allocation will put user-specified highestResponse times as many patients on the treatment with the highest number of responses
    # eg the treatment vReturnTreatmentID[ 1 ] will receive user-specified highestResponse times as many patients as vReturnTreatmentID[ 2 ]
    vAllocationRatio   <- c( ______, ______ )

    # Treatment vReturnTreatmentID[ 1 ] will have a ratio of UserParam$highestResponse, vReturnTreatmentID[ 2 ] a ratio of UserParam$nextHighestResponse, and control is always 1

    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length( vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        # Fatal error because the R code is incorrect
        nErrorCode <- -1
    }

    lReturn <- list( ______ = as.integer( vReturnTreatmentID ),
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )

    return( lReturn )

}
