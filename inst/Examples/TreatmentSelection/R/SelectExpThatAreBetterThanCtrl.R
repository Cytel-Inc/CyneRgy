######################################################################################################################## .
#' Select treatments that are higher than control or, if none are greater, select the treatment with the largest probability of response.  
#' @param SimData Dataframe which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform treatment selection
#' @param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL.
#' @description
#'  At the interim analysis, select any treatment with a response rate that is higher than control for stage 2.
#'  If none of the treatments have a higher response rate than control, select the treatment with the largest probability of response.
#'  In the second stage, the randomization ratio will be 1:1 (experimental:control).
#' @return TreatmentID  A vector that consists of the experimental treatments that were selected and carried forward. Experimental treatment IDs are 1, 2, ..., number of experimental treatments
#' @return AllocRatio A vector that consists of the allocation for all experimental treatments that continue to the next phase.
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Nonfatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted
#' @note The length of TreatmentID and AllocRatio must be the same.
#' @note The allocation ratio for control will be 1, AllocRatio are relative to this value.  So, a 2 will randomize twice as many to experimental
#' @note The order of AllocRatio should be the same as TreatmentID, and the  corresponding elements will have the assigned allocation ratio
#' @note The returned vector ONLY includes TreatmentIDs for experimental treatments, eg TreatmentID = c( 0, 1, 2 ) is invalid, because you do NOT need to include 0 for control.
#' @note You must return at LEAST one treatment and one allocation ratio
#' @examples  
#'       # Example Output Object:
#'       # Example 1: Assuming the allocation in 2nd part of the trial is 1:2:2 for Control:Experimental 1:Experimental 2
#'       vSelectedTreatments <- c( 1, 2 )  # Experimental 1 and 2 both have an allocation ratio of 2. 
#'       vAllocationRatio    <- c( 2, 2 )
#'       nErrorCode          <- 0
#'       lReturn             <- list( TreatmentID = vSelectedTreatments, 
#'                                    AllocRatio  = vAllocationRatio,
#'                                    ErrorCode   = nErrorCode )
#'       return( lReturn )
#'       
#'       #Example 2: Assuming the allocation in 2nd part of the trial is 1:1:2 for Control:Experimental 1:Experimental 2
#'       vSelectedTreatments <- c( 1, 2 )  # Experimental 2 will receive twice as many as Experimental 1 or Control. 
#'       vAllocationRatio    <- c( 1, 2 )
#'       nErrorCode          <- 0
#'       lReturn             <- list( TreatmentID = vSelectedTreatments, 
#'                                    AllocRatio  = vAllocationRatio,
#'                                    ErrorCode   = nErrorCode )
#'       return( lReturn )
#'
#' @note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.
#' @export
######################################################################################################################## .
SelectExpThatAreBetterThanCtrl  <- function(SimData, DesignParam, LookInfo, UserParam=NULL)
{
    
    # Input objects can be saved through the following lines:
    #setwd( "[ENTER THE DIRECTORY WHERE YOU WANT TO SAVE DATA]")
    #saveRDS( SimData, "SimData.Rds")
    #saveRDS( DesignParam, "DesignParam.Rds" )
    #saveRDS( LookInfo, "LookInfo.Rds" )
    
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
    
    nErrrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length(vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        #  Fatal error because the R code is incorrect 
        nErrrorCode <- -1  
    }
    
    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ) ,
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrrorCode ) )
    
    return( lReturn )
    
}
