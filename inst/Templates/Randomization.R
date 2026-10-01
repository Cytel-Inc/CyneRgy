######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#'
#' @title Template: Randomize subjects to treatment arms
#'
#' @description Randomize subjects to treatment arms. Use this template as a starting point for custom logic.
#'   Preserve the engine-supplied argument names and access named list elements by name. Supply additional
#'   user-defined inputs through UserParam where that argument is supported.
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArms Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#'
#' @param AllocRatio Numeric vector of experimental-to-control allocation ratios, one element per experimental arm.
#'   The control allocation is 1, so a ratio of 2 assigns twice as many subjects to that experimental arm as to
#'   control.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArms, AllocRatio, UserParam = NULL ) {
    nErrorCode <- 0

    # Allocation ratio on control and treatment arm
    vAllocRatio <- c( 1, AllocRatio )

    # Convert the Allocation Ratio to Allocation Fraction for control and treatment arms
    vAllocFraction <- vAllocRatio / sum( vAllocRatio )
    vTreatmentIDs <- sample( 0:( NumArms - 1 ), NumSub, prob = vAllocFraction, replace = TRUE )

    return( list( TreatmentID = as.integer( vTreatmentIDs ), ErrorCode = as.integer( nErrorCode ) ) )
}
