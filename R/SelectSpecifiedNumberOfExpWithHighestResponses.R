######################################################################################################################## .
#' Select user-specified number of treatments to advance that have the largest number of responses. 
#'@param SimData Data frame which consists of data generated in current simulation
#'@param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#'@param LookInfo List containing Design and Simulation Parameters, which might be required to perform treatment selection
#'@param UserParam A list of user defined parameters in East. The default must be NULL.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#' \item{UserParam$QtyOfArmsToSelect}{A value that defines how many treatment arms are chosen to advance. 
#'                          Note this number must match the number of user-specified allocation values.
#'                          If this value is not specified, the default is 2.}  
#' \item{UserParam$Rank1AllocationRatio} {A value that specifies the allocation to the arm with the highest response
#'                             If this value is not specified, the default is 2.}
#' \item{UserParam$Rank2AllocationRatio} {A value that specifies the allocation to the arm with the next highest response
#'                                 If this value is not specified, the default is 1.}
#'          }
#'@description
#'This function is used for the MAMS design with a binary outcome and will perform treatment selection at the interim analysis (IA).   
#'At the IA, the user-specified number of experimental treatments (QtyOfArmsToSelect) that have the largest number of responses are selected.
#'After the IA, we would like to randomize based on user specified inputs: 1:Rank1AllocationRatio:Rank2AllocationRatio (control, selected experimental arm with highest number of responses, selected experimental arm with the second highest number of responses)

#' @return TreatmentID  A vector that consists of the experimental treatments that were selected and carried forward. Experimental treatment IDs are 1, 2, ..., number of experimental treatments
#' @return AllocRatio A vector that consists of the allocation for all experimental treatments that continue to the next phase.
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted
#' @note The length of TreatmentID and AllocRatio must be the same.
#' @note The allocation ratio for control will be 1, AllocRatio are relative to this value.  So, a 2 will randomize twice as many to experimental
#' @note The order of AllocRatio should be the same as TreatmentID, and the  corresponding elements will have the assigned allocation ratio
#' @note The returned vector ONLY includes TreatmentIDs for experimental treatments, eg TreatmentID = c( 0, 1, 2 ) is invalid, because you do NOT need to include 0 for control.
#' @note You must return at LEAST one treatment and one allocation ratio
#'@examples  Example Output Object:
#'       Example 1: Assuming the allocation in 2nd part of the trial is 1:2:2 for Control:Experimental 1:Experimental 2
#'       vSelectedTreatments <- c( 1, 2 )  # Experimental 1 and 2 both have an allocation ratio of 2. 
#'       vAllocationRatio    <- c( 2, 2 )
#'       nErrorCode          <- 0
#'       lReturn             <- list( TreatmentID = vSelectedTreatments, 
#'                                    AllocRatio  = vAllocationRatio,
#'                                    ErrorCode   = nErrorCode )
#'       return( lReturn )
#'       
#'      Example 2: Assuming the allocation in 2nd part of the trial is 1:1:2 for Control:Experimental 1:Experimental 2
#'       vSelectedTreatments <- c( 1, 2 )  # Experimental 2 will receive twice as many as Experimental 1 or Control. 
#'       vAllocationRatio    <- c( 1, 2 )
#'       nErrorCode          <- 0
#'       lReturn             <- list( TreatmentID = vSelectedTreatments, 
#'                                    AllocRatio  = vAllocationRatio,
#'                                    ErrorCode   = nErrorCode )
#'       return( lReturn )
#'
#'@note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.
#' @export
SelectSpecifiedNumberOfExpWithHighestResponses  <- function(SimData, DesignParam, LookInfo, UserParam = NULL)
{
          
    #Input objects can be saved through the following lines:
    #setwd( "[ENTERED THE DESIRED LOCATION TO SAVE THE FILE]" )
    #saveRDS( SimData, "SimData.Rds")
    #saveRDS( DesignParam, "DesignParam.Rds" )
    #saveRDS( LookInfo, "LookInfo.Rds" )



    if( !exists( "UserParam" ) | is.null( UserParam ) )
    {
        UserParam <- list( QtyOfArmsToSelect = 3, Rank1AllocationRatio = 3, Rank2AllocationRatio = 2, Rank3AllocationRatio = 1 )
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
    vReturnTreatmentID <- as.integer( row.names( mSortedMatrix[1:UserParam$QtyOfArmsToSelect, ]) )      
    
    # The treatment with the highest number of responses should receive the user-specified Rank1AllocationRatio times as many patients as the next highest.
    # The allocation will put user-specified Rank1AllocationRatio times as many patients on the treatment with the highest number of responses
    # eg the treatment vReturnTreatmentID[ 1 ] will receive user-specified Rank1AllocationRatio times as many patients as vReturnTreatmentID[ 2 ]
    lRankElements  <- UserParam[grep("^Rank", names(UserParam))]
    vAllocationRatio   <- unlist( lRankElements )[1:UserParam$QtyOfArmsToSelect]   
                                      
    # Treatment vReturnTreatmentID[ 1 ] will have a ratio of UserParam$Rank1AllocationRatio, vReturnTreatmentID[ 2 ] a ratio of UserParam$Rank2AllocationRatio, and control is always 1
    
    nErrrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length(vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        #Fatal error because the R code is incorrect
        nErrrorCode <- -1  
    }
    
    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ),
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrrorCode ) )
    
    return( lReturn )
    
}
