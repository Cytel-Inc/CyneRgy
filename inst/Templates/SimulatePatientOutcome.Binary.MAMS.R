#  Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template for simulating patient data in R when the outcome is binary.
#' 
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param NumArm The number of arms in the trial (including placebo/control), integer value
#' @param ArrivalTime Arrival times of the subjects, numeric vector, length( ArrivalTime ) = NumSub
#' @param TreatmentID A vector of treatment ids, 0 = control, 1,2,...,NumArm-1 for treatment arms. length(TreatmentID) = NumSub
#' @param PropResp A vector of length NumArm with the response probabilities for each arm
#' @param UserParam A list of user defined parameters in East or East Horizon. You must have a default of NULL, as in this example.
#' If UserParam are supplied, they will be an element in the list, UserParam.
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'             \item{Response}{Required numeric value. Contains a vector of generated binary response for all subjects.}
#'             \item{ErrorCode}{Optional integer value \describe{
#'                                         \item{ErrorCode = 0}{No Error}
#'                                         \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                         \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                         }
#'                                     }
#'             }
#'             
#' @description
#' This template can be used as a starting point for developing custom functionality when the patient response is binary.
#' The function signature must remain the same.
#' However, you may choose to ignore the parameter PropResp if the patient simulator
#' you are creating only requires use of parameters the user will add to UserParam
{{FUNCTION_NAME}} <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL )
{
    
    # Step 1 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {
        
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues. Also, if there is a default value for the parameters you may want to set them here. Default values usually
        # are applied to have the same functionality as East, see the first example
        
        # EXAMPLE - Set the default if needed
        #UserParam <- list( dProbOfZeroOutcomeCtrl = 0, dProbOfZeroOutcomeExp = 0 )
    }
    
    # Step 2 - Initialize variable ####
    nError          <- 0 # East Horizon code for no errors occurred
    vPatientOutcome <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a response will be simulated
    
    
    # Step 3 - Loop over the patients and simulate the outcome according to the treatment they received ####
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has treatments as 0, 1, 2,..., so add 1 to get a vector index
        
        # Make any adjustments to the code as needed, example simulating a binary response using rbinom
        vPatientOutcome[ nPatIndx ] <- rbinom( 1, 1, PropResp[ nTreatmentID ] )
    }
    
    # Step 4 - Error Checking ####
    if( any( is.na( vPatientOutcome ) ) )
        nError <- -100
    
    # Step 5 - Build the return object, add other variables to the list as needed
    lReturn <- list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) )
    return( lReturn )
}
