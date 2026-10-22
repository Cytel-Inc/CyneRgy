######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template: Generate dropout indicators for binary or continuous outcomes
#' @description Generate dropout indicators for binary or continuous outcomes. Use this template as a starting
#'   point for custom logic. Preserve the engine-supplied argument names and access named list elements by name.
#'   Supply additional user-defined inputs through UserParam where that argument is supported.
#' @param NumSub Integer number of subjects in the trial.
#' @param ProbDrop Numeric dropout probability for a two-arm design, shared by both arms; for a multi-arm design, a
#'   numeric vector of length NumArm with the control probability first and experimental-arm probabilities
#'   thereafter. Values must be between 0 and 1.
#' @param NumArm Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{CensorInd}{Integer vector of censor indicators, with one element per subject: 0 = dropout/non-completer;
#'     1 = completer.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details These binary/continuous dropout functions return CensorInd for standard binary and continuous outcomes.
#'   Vaccine-efficacy binary designs instead use DropOutTime and also supply NumArm = 2 and FollowUpDur (the
#'   follow-up time at which the dropout probability applies). Extend the function signature and generation logic
#'   before using it for that case.
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, ProbDrop, NumArm, TreatmentID, UserParam = NULL ) {
    nErrorCode <- 0

    vCensoringIndicator <- numeric( NumSub )
    for ( nSubjectIndex in seq_len( NumSub ) ) {
        # Get the arm index (adjusting for 0-based indexing in TreatmentID)
        nArmIndex <- TreatmentID[ nSubjectIndex ] + 1

        # Generate dropout indicator based on the arm-specific probability
        # 1 - ProbDrop[ nArmIndex ] gives the probability of completion (not dropping out)
        vCensoringIndicator[ nSubjectIndex ] <- stats::rbinom( n = 1, size = 1, prob = 1 - ProbDrop[ nArmIndex ] )
    }

    return( list( CensorInd = as.integer( vCensoringIndicator ), ErrorCode = as.integer( nErrorCode ) ) )
}
