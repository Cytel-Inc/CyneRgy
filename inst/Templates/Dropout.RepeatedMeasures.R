######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#'
#' @title Template: Generate dropout for repeated-measures outcomes
#'
#' @description Generate dropout for repeated-measures outcomes. Use this template as a starting point for custom
#'   logic. Preserve the engine-supplied argument names and access named list elements by name. Supply additional
#'   user-defined inputs through UserParam where that argument is supported.
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArm Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#'
#' @param NumVisit Integer number of visits. The engine sets this to 1 when DropMethod = 2.
#'
#' @param VisitTime Numeric vector of visit times of length NumVisit.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param DropMethod Integer dropout input method for repeated measures: 1 = cumulative probability of dropout by
#'   visit; 2 = cumulative probability of dropout by time.
#'
#' @param ByTime Numeric dropout assessment times. For DropMethod = 1, a vector of length NumVisit equal to
#'   VisitTime; for DropMethod = 2, a single numeric time.
#'
#' @param DropParamControl Control-arm cumulative dropout probabilities. For DropMethod = 1, a numeric vector of
#'   length NumVisit ordered by visit; for DropMethod = 2, a single numeric probability by ByTime.
#'
#' @param DropParamTrt Experimental-arm cumulative dropout probabilities. For DropMethod = 1, a numeric vector of
#'   length NumVisit ordered by visit; for DropMethod = 2, a single numeric probability by ByTime.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{CensorInd1, ..., CensorIndNumVisit}{Integer censor-indicator vectors, one per visit: 0 =
#'     dropout/non-completer; 1 = completer.}
#'   \item{DropOutTime}{Numeric vector of generated dropout times measured from each subject's enrollment, with one
#'     element per subject. Inf indicates no dropout.}
#'   \item{DropoutVisitID}{Integer vector of 1-based visit IDs after which subjects drop out, with one element per
#'     subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' @details Return at least one of the three supported dropout representations. East Horizon uses CensorInd1, ...,
#'   CensorIndNumVisit first, then DropoutVisitID if censor indicators are absent, then DropOutTime if neither of
#'   the other representations is returned. Inf dropout times indicate no dropout.
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArm, NumVisit, VisitTime, TreatmentID,
                             DropMethod, ByTime, DropParamControl, DropParamTrt, UserParam = NULL ) {
    # Initialize the visit-specific censor indicators to no dropout.
    nErrorCode <- 0
    lReturn <- list( )
    for ( nVisitIndex in seq_len( NumVisit ) ) {
        strCensorIndName <- paste0( "CensorInd", nVisitIndex )
        lReturn[[ strCensorIndName ]] <- rep( 1L, NumSub )
    }

    # Replace the censor indicators using the chosen dropout model.
    # Alternatively, return DropoutVisitID or DropOutTime instead of CensorInd1, ... .
    # Do not return contradictory representations; censor indicators take precedence.
    lReturn$ErrorCode <- as.integer( nErrorCode )
    return( lReturn )
}
