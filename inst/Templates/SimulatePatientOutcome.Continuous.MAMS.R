######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Simulate Multi-Arm Continuous Patient Outcomes
#' @param NumSub Integer number of subjects to simulate.
#' @param NumArms Integer number of trial arms, including control.
#' @param ArrivalTime Numeric vector of subject arrival times with length `NumSub`.
#' @param TreatmentID Integer vector of arm assignments with length `NumSub`; 0 denotes control and 1 through `NumArms - 1` denote treatment arms.
#' @param Mean A vector of length = NumArms with the means of all arms
#' @param StdDev A vector of length = NumArms with the standard deviations of each arm
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' If UserParam values are supplied, they will be elements of the list.
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'             \item{Response}{Required numeric value. Contains a vector of generated response for all subjects.}
#'             \item{ErrorCode}{Optional integer value \describe{
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'             }
#' @description
#' This template can be used as a starting point for developing custom functionality.  The function signature must remain the same.
#' However, you may choose to ignore the parameters  Mean, StdDev if the patient simulator
#' you are creating only requires use of parameters the user will add to UserParam
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArms, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL )
{

    # Step 1 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {

        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues. Also, if there is a default value for the parameters you may want to set them here. Default values usually
        # are applied to have the same functionality as East Horizon, see the first example

        # EXAMPLE - Set the default if needed
        #UserParam <- list( dProbOfZeroOutcomeCtrl = 0, dProbOfZeroOutcomeExp = 0 )
    }

    # Step 2 - Initialize variable ####
    nError           <- 0 # No errors occurred
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    # Step 3 - Loop over the patients and simulate the outcome according to the treatment they received ####
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has treatments as 0, 1, 2,..., so add 1 to get a vector index

        # Make any adjustments to the code as needed, example simulating from for a normal distribution
        vPatientOutcome[ nPatIndx ] <- rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ] )
    }

    # Step 4 - Error Checking ####
    if( any( is.na( vPatientOutcome ) ) )
        nError <- -100

    # Step 5 - Build the return object, add other variables to the list as needed
    lReturn <- list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) )
    return( lReturn )
}
