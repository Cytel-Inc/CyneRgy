#   Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template for simulating patient data in R. 
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param Mean A vector of length = 2 with the means of the two treatments.
#' @param StdDev A vector of length = 2 with the standard deviations of each treatment
#' @param  UserParam A list of user defined parameters in East.   You must have a default of NULL, as in this example.
#' If UseParam are supplied in East, they will be an element in the list, UserParam.    
#' @description
#' This template can be used as a starting point for developing custom functionality.  The function signature must remain the same.  
{{FUNCTION_NAME}} <- function(NumSub, TreatmentID, Mean, StdDev, UserParam = NULL)
{
    # Note: It can be helpful to save to the parameters that East sent.
    # The next two lines show how you could save the UserParam variable to an Rds file
    # setwd( "[ENTERED THE DESIRED LOCATION TO SAVE THE FILE]" )
    # saveRDS(UserParam, "UserParam.Rds")
    # saveRDS(NumSub, "NumSub.Rds" )
    # saveRDS( TreatmentID, "TreatmentID.Rds" )
    # saveRDS( Mean, "Mean.Rds" )
    # saveRDS( StdDev, "StdDev.Rds" )

    # Step 1 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {
        
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues.   Also, if there is a default value for the parameters you may want to set them here.  Default values usually
        # are applied to have the same functionality as East, see the first example
        
        # EXMAPLE - Set the default if needed
        #UserParam <- list( dProbOfZeroOutcomeCtrl = 0, dProbOfZeroOutcomeExp = 0 )
    }
    
    # Step 2 - Initialize variable ####   
    nError           <- 0 # East code for no errors occurred 
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    
    
    # Step 3 - Loop over the patients and simulate the outcome according to the treatment they received ####
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East has the treatments as 0, 1 so need to add 1 to get a vector index
        
        # Make any adjustments to the code as needed, example simulating from for a normal distribution 
        vPatientOutcome[ nPatIndx ] <- rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ])
    }
    
    # Step 4 - Error Checking ####
    if(  any( is.na( vPatientOutcome )==TRUE) )
        nError <- -100
    
    # Step 5 - Build the return object, add other variables to the list as needed
    lReturn <- list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) )
    return( lReturn )
}
