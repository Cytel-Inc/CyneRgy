######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#'
#' @title Template: Simulate binary subject responses
#'
#' @description Simulate binary subject responses. Use this template as a starting point for custom logic. Preserve
#'   the engine-supplied argument names and access named list elements by name. Supply additional user-defined
#'   inputs through UserParam where that argument is supported.
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArm Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param PropResp Numeric vector of response probabilities by arm, with the control arm first, followed by
#'   experimental arms in TreatmentID order. Each probability is between 0 and 1.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' @details For vaccine-efficacy binary designs, the engine additionally supplies FollowUpDur (numeric follow-up
#'   duration, used as the assessment time for PropResp) and OneMinusROP (numeric value of 1 minus the
#'   treatment-to-control ratio of proportions). These standard binary examples do not use those inputs; extend the
#'   signature and generation logic for a vaccine-efficacy design. Standard binary responses are coded 0 =
#'   non-response and 1 = response.
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL ) {
    # Step 1 - Initialize the return variables or other variables needed ####
    nErrorCode <- 0
    vPatientOutcome <- rep( 0, NumSub ) # Note, as you simulate the patient data put in this vector so it can be returned

    # Step 2 - Validate custom variable input and set defaults ####
    if ( is.null( UserParam ) ) {
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues. Also, if there is a default value for the parameters you may want to set them here. Default values usually
        # are applied to have the same functionality as East Horizon, see the first example

        # EXAMPLE - Set the default if needed
        # UserParam <- list( dProbOfZeroOutcomeCtrl = 0, dProbOfZeroOutcomeExp = 0 )
    }

    # Step 3 - Loop over the patients and simulate the outcome according to the treatment they received ####

    # Example 1 - Loop over the patient vector and sample patient outcome using rbinom
    for ( nPatIndx in 1:NumSub ) {
        # Add code here to modify how patient data is generated to fit your need

        # EXAMPLE
        # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index
        # nTreatmentID                <- TreatmentID[ nPatIndx ] + 1

        # Make any adjustments to the code as needed, for example simulating from a normal distribution
        # vPatientOutcome[ nPatIndx ] <- rbinom( 1, 1, PropResp[ nTreatmentID ])
    }

    # Write the actual code here.
    # Use appropriate error handling and modify the error code appropriately.

    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nErrorCode ) ) )
}
