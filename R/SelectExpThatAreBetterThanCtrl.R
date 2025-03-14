######################################################################################################################## .
#' @title Select Experimental Treatments Better Than Control
#' 
#' @description Select treatments that are higher than control or, if none are greater, select the treatment with the largest probability of response.
#' At the interim analysis, select any treatment with a response rate that is higher than control for stage 2.
#' If none of the treatments have a higher response rate than control, select the treatment with the largest probability of response.
#' In the second stage, the randomization ratio will be 1:1 (experimental:control).
#' 
#' @param SimData Dataframe which consists of data generated in the current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform treatment selection.
#' @param UserParam A list of user-defined parameters in East or East Horizon. The default must be NULL.
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
#' lReturn             <- list(TreatmentID = vSelectedTreatments, AllocRatio = vAllocationRatio, ErrorCode = nErrorCode)
#' return(lReturn)
#'
#' # Example 2: Assuming the allocation in the second part of the trial is 1:1:2 for Control:Experimental 1:Experimental 2
#' vSelectedTreatments <- c(1, 2)  # Experimental 2 will receive twice as many as Experimental 1 or Control.
#' vAllocationRatio    <- c(1, 2)
#' nErrorCode          <- 0
#' lReturn             <- list(TreatmentID = vSelectedTreatments, AllocRatio = vAllocationRatio, ErrorCode = nErrorCode)
#' return(lReturn)
#' @export

######################################################################################################################## .

SelectExpThatAreBetterThanCtrl  <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Calculate the number of responders and treatment failures for each treatment
    
    # The next lines create a table where each treatment is in a row, number of treatment failures is the first column, and number of responses is the second column.
    tabResults   <- table( SimData$TreatmentID, SimData$Response )
    
    # Compute the response probability as # of responses/(  # of treatment failures + # of responses )
    vProbabilityResponse                <- as.vector( tabResults[,2]/(tabResults[,1] + tabResults[,2] ) )
    
    # Create a variable with the probability of response on control to be used in decision making
    dProbabilityOfResponseOnControl     <- vProbabilityResponse[ 1 ]     
    # Create vector with only the estimated probability of response on experimentals
    vProbabilityResponseOnExperimental  <- vProbabilityResponse[ c(2:length(vProbabilityResponse)) ]     
    
    # Note: vProbabilityResponseOnExperimental now contains only the response rates for the experimental treatments
    
    # Selection Rule: Any treatment with a response rate that is higher than control is selected for stage 2
    vReturnTreatmentID <- c()
    # Note: Start with row 2, which is experimental treatment 1
    for( nIndex in 1:length( vProbabilityResponseOnExperimental ) )  
    {
        # If the response rate > response rate on control, add the treatment ID to the list
        if( vProbabilityResponseOnExperimental[ nIndex ] > dProbabilityOfResponseOnControl )   
            vReturnTreatmentID <- c( vReturnTreatmentID, nIndex  )    
    }
    
    # If none of the experimental treatments had a response rate greater than control, select the treatment with the largest response rate 
    if( length( vReturnTreatmentID ) == 0)
    {
        vReturnTreatmentID <-  which.max( vProbabilityResponseOnExperimental  )
    }
    
    # We want all treatments to have a randomization ratio of 1 
    # The allocation will put twice as many on the treatment with the highest number of responses, 
    # eg. the Treatment vReturnTreatmentID[ 1 ] will receive twice as many patients as vReturnTreatmentID[ 2 ]
    vAllocationRatio   <- rep( 1, length( vReturnTreatmentID ))    
    
    # Treatment vReturnTreatmentID[ 1 ] will have a ratio of 2, vReturnTreatmentID[ 2 ] a ratio of 1, and control is always 1
    
    nErrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length(vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        #  Fatal error because the R code is incorrect 
        nErrorCode <- -1  
    }
    
    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ) ,
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrorCode ) )
    
    return( lReturn )
}