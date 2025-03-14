######################################################################################################################## .
#' @title Select Treatments with the Highest Number of Responses
#'
#' @description This function is used for the Multi-Arm Multi-Stage (MAMS) design with a binary outcome and performs treatment selection at the interim analysis (IA). 
#' At the IA, the user-specified number of experimental treatments (`QtyOfArmsToSelect`) that have the largest number of responses are selected. After the IA, randomization is based on user-specified inputs: 1:`Rank1AllocationRatio`:`Rank2AllocationRatio` (control, selected experimental arm with the highest number of responses, selected experimental arm with the second highest number of responses).
#'
#' @param SimData A dataframe consisting of data generated in the current simulation.
#' @param DesignParam A list of design and simulation parameters required to perform treatment selection.
#' @param LookInfo A list containing design and simulation parameters that might be required to perform treatment selection.
#' @param UserParam A list of user-defined parameters in East or East Horizon. The default is `NULL`. 
#' If supplied, the list must contain the following named elements:
#' \describe{
#' \item{QtyOfArmsToSelect}{A value defining how many treatment arms are chosen to advance. Note this number must match the number of user-specified allocation values.}  
#' \item{Rank1AllocationRatio}{A value specifying the allocation to the arm with the highest response.}
#' \item{Rank2AllocationRatio}{A value specifying the allocation to the arm with the next highest response.}
#' }
#'
#' @return A list containing:
#'   \item{TreatmentID}{A vector of experimental treatment IDs selected to advance, e.g., 1, 2, ..., number of experimental treatments.}
#'   \item{AllocRatio}{A vector of allocation ratios for the selected treatments relative to control.}
#'   \item{ErrorCode}{An integer indicating success or error status:
#'     \describe{
#'       \item{ErrorCode = 0}{No error.}
#'       \item{ErrorCode > 0}{Nonfatal error, current simulation aborted but subsequent simulations will run.}
#'       \item{ErrorCode < 0}{Fatal error, no further simulations attempted.}
#'     }
#'   }
#'
#' @note 
#' \itemize{
#' \item The length of `TreatmentID` and `AllocRatio` must be the same.
#' \item The allocation ratio for control is always 1, and `AllocRatio` values are relative to this. For example, an allocation value of 2 means twice as many participants are randomized to the experimental treatment compared to control.
#' \item The order of `AllocRatio` should match `TreatmentID`, with corresponding elements assigned their respective allocation ratios.
#' \item The returned vector includes only `TreatmentID` values for experimental treatments. For example, `TreatmentID = c(0, 1, 2)` is invalid because control (`0`) should not be included.
#' \item At least one treatment and one allocation ratio must be returned.
#' }
#'
#' @examples 
#' # Example 1: Assuming the allocation in the second part of the trial is 1:2:2 for Control:Experimental 1:Experimental 2
#' vSelectedTreatments <- c(1, 2)  # Experimental 1 and 2 both have an allocation ratio of 2.
#' vAllocationRatio    <- c(2, 2)
#' nErrorCode          <- 0
#' lReturn             <- list(
#'   TreatmentID = vSelectedTreatments, 
#'   AllocRatio  = vAllocationRatio,
#'   ErrorCode   = nErrorCode
#' )
#' return(lReturn)
#' 
#' # Example 2: Assuming the allocation in the second part of the trial is 1:1:2 for Control:Experimental 1:Experimental 2
#' vSelectedTreatments <- c(1, 2)  # Experimental 2 will receive twice as many participants as Experimental 1 or Control. 
#' vAllocationRatio    <- c(1, 2)
#' nErrorCode          <- 0
#' lReturn             <- list(
#'   TreatmentID = vSelectedTreatments, 
#'   AllocRatio  = vAllocationRatio,
#'   ErrorCode   = nErrorCode
#' )
#' return(lReturn)
#' @export
######################################################################################################################## .

SelectSpecifiedNumberOfExpWithHighestResponses  <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL)
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
    mSortedMatrix      <- tabResults[order( tabResults[, 2], decreasing =  TRUE), ]
    
    # Select the user-specified (QtyOfArmsToSelect) number of treatments with the largest number of responses
    vSortedNames       <- row.names( mSortedMatrix )  # Get the names of the treatments in order by number of responses
    vReturnTreatmentID <- as.integer( vSortedNames[1:UserParam$QtyOfArmsToSelect ] )  # Select the number of desired treatments.         
    
    # The treatment with the highest number of responses should receive the user-specified Rank1AllocationRatio times as many patients as the next highest.
    # The allocation will put user-specified Rank1AllocationRatio times as many patients on the treatment with the highest number of responses
    # eg the treatment vReturnTreatmentID[ 1 ] will receive user-specified Rank1AllocationRatio times as many patients as vReturnTreatmentID[ 2 ]
    # NOTE: Always pull elements from the list by name rather than assuming a specific order
    vAllocationRatio <- c()
    for( iRank in 1:UserParam$QtyOfArmsToSelect )
    {
        vAllocationRatio <- c( vAllocationRatio, UserParam[[ paste0( "Rank", iRank, "AllocationRatio" )]])
    }
    
    # Treatment vReturnTreatmentID[ 1 ] will have a ratio of UserParam$Rank1AllocationRatio and
    # vReturnTreatmentID[ 2 ] a ratio of UserParam$Rank2AllocationRatio, and control is always 1
    
    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length(vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        #Fatal error because the R code is incorrect
        nErrorCode <- -1  
    }
    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ),
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )
    return( lReturn )
}