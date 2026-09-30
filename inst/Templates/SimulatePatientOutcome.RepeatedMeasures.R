######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#'
#' @title Template: Simulate repeated-measures subject responses
#'
#' @description Simulate repeated-measures subject responses. Use this template as a starting point for custom
#'   logic. Preserve the engine-supplied argument names and access named list elements by name. Supply additional
#'   user-defined inputs through UserParam where that argument is supported.
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumVisit Integer number of visits.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param Inputmethod Integer input method: 0 = actual means and standard deviations at each visit; 1 = expected
#'   changes from baseline at each visit. Preserve this engine-supplied spelling.
#'
#' @param VisitTime Numeric vector of visit times of length NumVisit.
#'
#' @param MeanControl Numeric vector of control-arm mean responses of length NumVisit, ordered by visit.
#'
#' @param MeanTrt Numeric vector of experimental-arm mean responses of length NumVisit, ordered by visit.
#'
#' @param StdDevControl Numeric vector of control-arm response standard deviations of length NumVisit, ordered by
#'   visit.
#'
#' @param StdDevTrt Numeric vector of experimental-arm response standard deviations of length NumVisit, ordered by
#'   visit.
#'
#' @param CorrMat Numeric correlation matrix between visits, with NumVisit rows and NumVisit columns.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Response1, ..., ResponseNumVisit}{Numeric response vectors, one per visit, with one element per subject.
#'     Replace NumVisit by the actual number of visits.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' @details Return each visit response as a separate named list element: Response1, Response2, ...,
#'   ResponseNumVisit. Optional ArrivalTime may be included in the function signature when calendar arrival times
#'   are needed; it has the same definition as at the enrollment integration point.
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumVisit, ArrivalTime, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL ) {
    # TO DO : Modify this function appropriately
    nErrorCode <- 0
    vOutResponse <- c( )
    retval <- list( )

    # Add code to simulate the patient data as desired.
    # Example of how to create the return list with Response1, Response2, ..., ResponseNumVisit
    # Store the generated continuous response values in # an array called retval.
    # Initializing Response Array to 0
    for ( i in 1:NumVisit ) {
        strVisitName <- paste0( "Response", i )
        vOutResponse <- rep( 0, NumSub )
        retval[[ strVisitName ]] <- as.double( vOutResponse )
    }

    # Use appropriate error handling and modify the
    # error appropriately
    retval$ErrorCode <- as.integer( nErrorCode )
    return( retval )
}
