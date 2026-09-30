######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#'
#' @title Template: Simulate continuous subject responses
#'
#' @description Simulate continuous subject responses. Use this template as a starting point for custom logic.
#'   Preserve the engine-supplied argument names and access named list elements by name. Supply additional
#'   user-defined inputs through UserParam where that argument is supported.
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArms Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param Mean Numeric vector of mean responses by arm, with the control arm first, followed by experimental arms
#'   in TreatmentID order.
#'
#' @param StdDev Numeric vector of response standard deviations by arm, with the control arm first, followed by
#'   experimental arms in TreatmentID order.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements: A list of user defined parameters in East Horizon. You
#'   must have a default of NULL, as in this example. If UserParam are supplied, they will be an element in the
#'   list, UserParam.
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject. Required.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArms, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL ) {
    # Step 1 - Validate custom variable input and set defaults ####
    if ( is.null( UserParam ) ) {
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues. Also, if there is a default value for the parameters you may want to set them here. Default values usually
        # are applied to have the same functionality as East Horizon, see the first example

        # EXAMPLE - Set the default if needed
        # UserParam <- list( dProbOfZeroOutcomeCtrl = 0, dProbOfZeroOutcomeExp = 0 )
    }

    # Step 2 - Initialize variable ####
    nErrorCode <- 0 # No errors occurred
    vPatientOutcome <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    # Step 3 - Loop over the patients and simulate the outcome according to the treatment they received ####
    for ( nPatIndx in 1:NumSub ) {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has treatments as 0, 1, 2,..., so add 1 to get a vector index

        # Make any adjustments to the code as needed, example simulating from for a normal distribution
        vPatientOutcome[ nPatIndx ] <- stats::rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ] )
    }

    # Step 4 - Error Checking ####
    if ( any( is.na( vPatientOutcome ) ) ) {
        nErrorCode <- -100
    }

    # Step 5 - Build the return object, add other variables to the list as needed
    lReturn <- list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nErrorCode ) )
    return( lReturn )
}
