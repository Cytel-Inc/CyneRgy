######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#'
#' @title Template: Simulate subject survival times
#'
#' @description Simulate subject survival times. Use this template as a starting point for custom logic. Preserve
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
#' @param SurvMethod Integer survival input method: 1 = hazard rates; 2 = cumulative survival percentages; 3 =
#'   median survival times.
#'
#' @param NumPrd Integer number of survival periods. Equals 1 for multi-arm confirmatory designs and stratified
#'   survival generation.
#'
#' @param PrdTime Times used to specify survival parameters: starting times of hazard pieces for SurvMethod = 1;
#'   times at which cumulative survival percentages are specified for SurvMethod = 2; 0 for SurvMethod = 3. Legacy
#'   East Horizon inputs may be vectors; East Horizon inputs may be period-by-arm arrays (stratum-by-arm arrays with
#'   stratification).
#'
#' @param SurvParam Array of survival parameters with NumPrd rows and NumArm columns, or one row per stratum when
#'   stratification is enabled. Column 1 is control; subsequent columns are experimental arms. Values are hazard
#'   rates for SurvMethod = 1, cumulative survival percentages for SurvMethod = 2, and median survival times for
#'   SurvMethod = 3. Without stratification, the median-survival method has one row.
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
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) {
    # Step 1 - Validate custom variable input and set defaults ####
    if ( is.null( UserParam ) ) {
        # If this function requires user defined parameters to be sent via the UserParam variable
        # check to make sure the values are valid and take care of any issues.
        # Also, if there is a default value for the parameters you may want to set them here
    }

    # Step 2 - Initialize variables ####
    # Initialize error codes and vectors used to store simulated survival times
    nErrorCode <- 0
    vResponse <- c( )

    # Step 3 - Determine which survival generation method will be used ####
    # The implementation supports:
    #   Method 1 - Piecewise exponential hazard model
    #   Method 2 - Survival probability based piecewise exponential model
    #   Method 3 - Median survival time based exponential model

    # Step 4 - Implement the data-generation logic ####

    # Step 5 - Error checking ####
    # Verify that all subjects received valid survival times
    # and that no missing values were generated
    if ( length( vResponse ) != NumSub || any( is.na( vResponse ) == TRUE ) ) {
        nErrorCode <- -100
    }

    # Step 6 - Build the return object ####
    lReturn <- list(
        SurvivalTime = as.double( vResponse ),
        ErrorCode    = as.integer( nErrorCode )
    )

    return( lReturn )
}
