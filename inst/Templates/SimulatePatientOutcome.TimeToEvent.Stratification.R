######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template: Simulate subject survival times
#' @description Simulate subject survival times. Use this template as a starting point for custom logic. Preserve
#'   the engine-supplied argument names and access named list elements by name. Supply additional user-defined
#'   inputs through UserParam where that argument is supported.
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#' @param StratumID Integer vector of stratum indices, with one element per subject. Stratum indices start at 1.
#'   Available when stratification is enabled.
#' @param SurvMethod Integer survival input method: 1 = hazard rates; 2 = cumulative survival percentages; 3 =
#'   median survival times.
#' @param NumPrd Integer number of survival periods. Equals 1 for multi-arm confirmatory designs and stratified
#'   survival generation.
#' @param PrdTime Times used to specify survival parameters: starting times of hazard pieces for SurvMethod = 1;
#'   times at which cumulative survival percentages are specified for SurvMethod = 2; 0 for SurvMethod = 3. Legacy
#'   East Horizon inputs may be vectors; East Horizon inputs may be period-by-arm arrays (stratum-by-arm arrays with
#'   stratification). The control-arm entries may be NA in engine-supplied arrays.
#' @param SurvParam Array of survival parameters with NumPrd rows and NumArm columns, or one row per stratum when
#'   stratification is enabled. Column 1 is control; subsequent columns are experimental arms. Values are hazard
#'   rates for SurvMethod = 1, cumulative survival percentages for SurvMethod = 2, and median survival times for
#'   SurvMethod = 3. Without stratification, the median-survival method has one row.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArm, ArrivalTime, TreatmentID, StratumID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) {
    # TO DO : Modify this function appropriately
    nErrorCode <- 0
    vSurvResponses <- c( )
    # Initialize the response vector to 0
    for ( nPatIndx in 1:NumSub ) {
        vSurvResponses[ nPatIndx ] <- 0
    }
    if ( SurvMethod == 1 ) { # Hazard Rates
        # Write the actual code for SurvMethod 1
        # here.
        # Store the generated survival times in an
        # array called vSurvResponses.
    }
    if ( SurvMethod == 2 ) { # Cumulative % Survivals
        # Write the actual code for SurvMethod 2
        # here.
        # Store the generated survival times in an
        # array called vSurvResponses.
    }
    if ( SurvMethod == 3 ) { # Median Survival Times
        # Write the actual code for SurvMethod 3
        # here.
        # Store the generated survival times in an
        # array called vSurvResponses.
    }
    # Use appropriate error handling and modify the
    # Error appropriately in each of the methods
    return( list( SurvivalTime = as.double( vSurvResponses ), ErrorCode = as.integer( nErrorCode ) ) )
}
