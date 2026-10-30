######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template: Generate subject arrival times
#' @description Generate subject arrival times. Use this template as a starting point for custom logic. Preserve
#'   the engine-supplied argument names and access named list elements by name. Supply additional user-defined
#'   inputs through UserParam.
#' @param NumPat Integer number of subjects in the trial.
#' @param Type Integer enrollment type. This function supports global enrollment (0), the default.
#' @param NumPrd Integer number of accrual periods.
#' @param PrdStart Numeric vector of accrual-period starting times of length NumPrd. The first period starts at 0.
#' @param AccrRate Numeric vector of accrual rates (subjects per unit time), with one element per accrual period.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details This function uses the global enrollment inputs NumPrd, PrdStart, and AccrRate.

######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumPat, NumPrd, PrdStart, AccrRate, UserParam = NULL, Type = 0 ) {
    # Step 1 - Initialize the return variables or other variables needed ####
    nErrorCode <- 0
    vPatientArrivalTime <- rep( 0, NumPat ) # Note, as you simulate the patient data put it in this vector so it can be returned

    # Step 2 - Validate custom variable input and set defaults ####
    if ( is.null( UserParam ) ) {
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the
        #   values are valid and
        # take care of any issues. Also, if there is a default value for the parameters you may want to set them here.
        #   Default values usually
        # are applied to have the same functionality as East Horizon, see the first example

        # EXAMPLE - Set the default if needed
        # UserParam <- list( dRate = 0.5 )
    }

    # Step 3 - Loop over the patients and simulate the patient arrival times in the trial ####

    # Example 1 - ####
    for ( nPatientIndex in seq_len( NumPat ) ) {
        # Add code here to simulate the patient arrival times.
        # The arrival times should be increasing
    }

    return( list( ArrivalTime = as.double( vPatientArrivalTime ), ErrorCode = as.integer( nErrorCode ) ) )
}
