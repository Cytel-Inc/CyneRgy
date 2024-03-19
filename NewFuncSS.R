#   Last Modified Date: 03/18/2024
#' @name NewFuncSS
#' @title Template for simulating patient data in R. 
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param NumArm  The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
#' @param TreatmentID A vector of treatment ids, 0 is control treatment and  1 experimental treatment.  length( TreatmentID ) = NumSub
#' @param PropResp A vector of length NumArm with the response probabilities for each arm
#' @param  UserParam A list of user defined parameters in East.   You must have a default of NULL, as in this example.
#' If UseParam are supplied in East, they will be an element in the list, UserParam.    
#' @description
#' This template can be used as a starting point for developing custom functionality when the patient response is binary.  
#' The function signature must remain the same.  
#' However, you may choose to ignore the parameters  PropResp if the patient simulator
#' you are creating only requires use of parameters the user will add to UserParam
NewFuncSS  <- function(NumSub, NumArm, TreatmentID, PropResp, UserParam = NULL )
{

    # Step 1 - Initialize the return variables or other variables needed ####
    Error 	        <- 0
    vPatientOutcome <- rep( 0, NubSub )  # Note, as you simulate the patient data put in in this vector so it can be returned
    
    # Step 2 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {
        
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues.   Also, if there is a default value for the parameters you may want to set them here.  Default values usually
        # are applied to have the same functionality as East, see the first example
        
        # EXMAPLE - Set the default if needed
        #UserParam <- list( dProbOfZeroOutcomeCtrl = 0, dProbOfZeroOutcomeExp = 0 )
    }
    
    # Step 3 - Loop over the patients and simulate the outcome according to the treatment they received ####
    
    #Example 1 - Loop over the patient vector and sample patient outcome using rbinom
    for( nPatIndx in 1:NumSub )
    {
        # Add code here to modify how patient data is generated to fit your need
        
        # EXAMPLE
        # The TreatmentID vector sent from East has the treatments as 0, 1 so need to add 1 to get a vector index
        # nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 
        
        # Make any adjustments to the code as needed, example simulating from for a normal distribution 
        # vPatientOutcome[ nPatIndx ] <- rbinom( 1, 1, PropResp[ nTreatmentID ])
    }
    
    #End of End of example block

    
    # Write the actual code here.
    # Store the generated binary response values 
    # in an array called retval.
    # Use appropriate error handling and modify the
    # Error appropriately
    
    return(list(Response = as.double(vPatientOutcome), ErrorCode = as.integer(Error)))
}
