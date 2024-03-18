######################################################################################################################## .
#' @param TreatmentSelectionTemplate
#' @title A template for treatment selection functions in MAMS binary designs.  
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform treatment selection.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform treatment selection
#' @param UserParam A list of user defined parameters in East. The default must be NULL.
#' @description
#' This function is used for the MAMS binary design and will perform treatment selection at the interim analysis (IA).  
#' The example R code below will guide you through what needs to be done. Step 1, Step 2, Step 3, and Step 4 comments are added below
#' to help you find the most likely places to add your new R code.
#' @return TreatmentID  A vector that consists of the experimental treatments that were selected and carried forward. Experimental treatment IDs are 1, 2, ..., number of experimental treatments
#' @return AllocRatio A vector that consists of the allocation for all experimental treatments that continue to the next phase.
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#'                                      ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#'                                      ErrorCode < 0 --> Fatal error, no further simulation will be attempted
#' @note The length of TreatmentID and AllocRatio must be the same.
#' @note The allocation ratio for control will be 1, AllocRatio are relative to this value.  So, a 2 will randomize twice as many to experimental
#' @note The order of AllocRatio should be the same as TreatmentID, and the  corresponding elements will have the assigned allocation ratio
#' @note The returned vector ONLY includes TreatmentIDs for experimental treatments, eg TreatmentID = c( 0, 1, 2 ) is invalid, because you do NOT need to include 0 for control.
#' @note You must return at LEAST one treatment and one allocation ratio
#' @examples  Example Output Object:
#'       Example 1: Assuming the allocation in 2nd part of the trial is 1:2:2 for Control:Experimental 1:Experimental 2
#'      vSelectedTreatments <- c( 1, 2 )  # Experimental 1 and 2 both have an allocation ratio of 2. 
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
######################################################################################################################## .

PerformTreatmentSelection  <- function(SimData, DesignParam, LookInfo, UserParam = NULL)
{
           
    # If you wanted to save the input objects you could use the following to save the files to your working directory
    # setwd( "[ENTERED THE DESIRED LOCATION TO SAVE THE FILE]" )
    # saveRDS( SimData, "SimData.Rds")
    # saveRDS( DesignParam, "DesignParam.Rds" )
    # saveRDS( LookInfo, "LookInfo.Rds" )
    
    # Pulling the important information from the simulated data, SimData, sent from East 
    vTreatmentID    <- SimData$TreatmentID  # TreatmentIDs are 0, 1,..., number of experimental treatments
    vPatientOutcome <- SimData$Response     # Response = 0 or 1
    

    # Step 1 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {
        
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues.   Also, if there is a default value for the parameters you may want to set them here.  Default values usually
        # are applied to have the same functionality as East, see the first example
        
        # EXAMPLE - Set the default if needed
        # UserParam <- list( )
    }
    
    # Step 2: Perform any data analysis to decide which treatment(s) are selected ####
    
    # TODO: Add any code here for analysis
    
    
    # Step 3: Create the vector of experimental treatments that will continue to the next part of the trial ####
    # Example: 
    # vReturnTreatmentID <- c( 1, 2 ) # Always select treatment 1 and 2
    
    # TODO: Add any code here for creating the treatment id vector
    
    
    # Step 4: Create a vector of allocation ratios #### 
    # All ratios are relative to control, which has a ratio of 1, and are for the corresponding treatment in vReturnTreatmentID
    # Example: Put twice as many on experimental treatment 1 as there are on 2
    # vAllocationRatio   <- c( 2, 1 )    # This puts twice as many on Experimental treatment 1 because vReturnTreatmentID = c( 1, 2 ) in this example
    
    # TODO: Add any code necessary for creating the allocation ratio vector.                                    
    
    
    # If you use the variable vReturnTreatmentID and vAllocationRatio above, then the remainder of this code will perform a basic error check 
    # and create the return object.  
    # Modify this code as necessary 
    
    nErrrorCode <- 0
    # Notes: The length( vReturnTreatmentID ) must equal length( vAllocationRatio )
    if( length(vReturnTreatmentID ) != length( vAllocationRatio ) )
    {
        # Fatal error because the R code is incorrect. 
        nErrrorCode <- -1  
    }
    
    lReturn <- list( TreatmentID = as.integer( vReturnTreatmentID ) ,
                     AllocRatio  = as.double( vAllocationRatio ),
                     ErrorCode   = as.integer( nErrrorCode ) )
    
    return( lReturn )
    
}
