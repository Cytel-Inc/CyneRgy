######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Simulate Binary Patient Outcomes
#' @param NumSub Integer number of subjects in the trial.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param PropResp Numeric vector of length `NumArm`, containing response probabilities for control followed by each experimental arm.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' If UserParam are supplied, they will be an element in the list, UserParam.
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'             \item{Response}{Required numeric value. Contains a vector of generated binary response for all subjects.}
#'             \item{ErrorCode}{Optional integer value \describe{
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'             }
#' @description
#' This template can be used as a starting point for developing custom functionality when the patient response is binary.
#' The function signature must remain the same.
#' However, you may choose to ignore the parameters  PropResp if the patient simulator
#' you are creating only requires use of parameters the user will add to UserParam
######################################################################################################################## .

{{FUNCTION_NAME}}  <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL )
{

    # Step 1 - Initialize the return variables or other variables needed ####
    nError          <- 0
    vPatientOutcome <- rep( 0, NumSub )  # Note, as you simulate the patient data put in this vector so it can be returned

    # Step 2 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {

        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues. Also, if there is a default value for the parameters you may want to set them here. Default values usually
        # are applied to have the same functionality as East Horizon, see the first example

        # EXAMPLE - Set the default if needed
        #UserParam <- list( dProbOfZeroOutcomeCtrl = 0, dProbOfZeroOutcomeExp = 0 )
    }

    # Step 3 - Loop over the patients and simulate the outcome according to the treatment they received ####

    #Example 1 - Loop over the patient vector and sample patient outcome using rbinom
    for( nPatIndx in 1:NumSub )
    {
        # Add code here to modify how patient data is generated to fit your need

        # EXAMPLE
        # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index
        # nTreatmentID                <- TreatmentID[ nPatIndx ] + 1

        # Make any adjustments to the code as needed, for example simulating from a normal distribution
        # vPatientOutcome[ nPatIndx ] <- rbinom( 1, 1, PropResp[ nTreatmentID ])
    }

    # Write the actual code here.
    # Use appropriate error handling and modify the error code appropriately.

    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ) )
}
