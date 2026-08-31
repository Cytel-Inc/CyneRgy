######################################################################################################################## .
#' @name SelectSpecifiedNumberOfExpWithHighestResponses
#' @title Select a Specified Number of Highest-Response Treatments
#' @description
#' Selects the requested number of experimental arms with the largest observed
#' response counts and assigns rank-specific allocation ratios.
#' @author Sydney Ringold, J. Kyle Wathen
#' @param SimData Dataframe which consists of data generated in current simulation
#' @param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#' @param LookInfo List containing parameters for the current analysis look.
#' @param UserParam A list of user defined parameters in East Horizon. The default must be NULL.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#' \item{UserParam$QtyOfArmsToSelect}{A value that defines how many treatment arms are chosen to advance.
#'                          Note this number must match the number of user-specified allocation values.
#'                          If this value is not specified, the default is 2.}
#' \item{UserParam$Rank1AllocationRatio}{A value that specifies the allocation to the arm with the highest response
#'                             If this value is not specified, the default is 2.}
#' \item{UserParam$Rank2AllocationRatio}{A value that specifies the allocation to the arm with the next highest response
#'                                 If this value is not specified, the default is 1.}
#'          }
#' @return A list containing `TreatmentID`, the selected experimental-arm indexes;
#'   `AllocRatio`, their allocation ratios relative to control; and integer `ErrorCode`.
######################################################################################################################## .

SelectSpecifiedNumberOfExpWithHighestResponses <- function( SimData, DesignParam, LookInfo, UserParam = NULL )
{
    if( !exists( "UserParam" ) | is.null( UserParam ) )
    {
        # Default is to select the treatment with highest number of responses and allocation of 2:1 (Experimental:Control)
        UserParam <- list( QtyOfArmsToSelect = 1, Rank1AllocationRatio = 2 )
    }
    # Calculate the number of responses per arm and select the highest user-specified number (QtyOfArmsToSelect) of arms
    tabResults   <- table( SimData$TreatmentID, SimData$Response )

    # Want to select the top user-specified (QtyOfArmsToSelect) number of experimental treatments, so drop control from the sorting
    # Now, only the experimental treatments are left
    tabResults   <- tabResults[ -1, ]

    # Sort in descending order based on the number of responses (column 2)
    # After the sort, the matrix will have the largest number of responses in the first row and the smallest number of responses in the last row
    mSortedMatrix      <- tabResults[ order( tabResults[ , 2 ], decreasing =  TRUE ), ]

    # Select the user-specified (QtyOfArmsToSelect) number of treatments with the largest number of responses
    vSortedNames       <- row.names( mSortedMatrix )  # Get the names of the treatments in order by number of responses
    vReturnTreatmentID <- as.integer( vSortedNames[ 1:UserParam$QtyOfArmsToSelect ] )  # Select the number of desired treatments.

    # The treatment with the highest number of responses should receive the user-specified Rank1AllocationRatio times as many patients as the next highest.
    # The allocation will put user-specified Rank1AllocationRatio times as many patients on the treatment with the highest number of responses
    # eg the treatment vReturnTreatmentID[ 1 ] will receive user-specified Rank1AllocationRatio times as many patients as vReturnTreatmentID[ 2 ]
    # NOTE: Always pull elements from the list by name rather than assuming a specific order
    vAllocationRatio <- c()
    for( iRank in 1:UserParam$QtyOfArmsToSelect )
    {
        vAllocationRatio <- c( vAllocationRatio, UserParam[[ paste0( "Rank", iRank, "AllocationRatio" ) ] ] )
    }

    # Treatment vReturnTreatmentID[ 1 ] will have a ratio of UserParam$Rank1AllocationRatio and
    # vReturnTreatmentID[ 2 ] a ratio of UserParam$Rank2AllocationRatio, and control is always 1

    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length( vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        #Fatal error because the R code is incorrect
        nErrorCode <- -1
    }
    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ),
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )
    return( lReturn )

}
