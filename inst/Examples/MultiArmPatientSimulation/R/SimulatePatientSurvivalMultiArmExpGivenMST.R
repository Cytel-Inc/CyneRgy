######################################################################################################################## .
#' @name SimulatePatientOutcomeMultiArmExpGivenMST
#'
#' @title Simulate survival outcomes for multi-arm clinical trial simulations given Median Survival Times (MST)
#'
#' @description Generate exponential survival times for multiple arms from the median survival times supplied
#'   in SurvParam. This example requires SurvMethod = 3 and supports a single survival period; other input
#'   methods return ErrorCode = -100.
#'
#' @author Gabriel Potvin and Anoop Singh Rawat
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
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' @details Example-specific error codes: Integer error code. 0 indicates success and -100 indicates invalid output
#'   generation.
######################################################################################################################## .

SimulatePatientOutcomeMultiArmExpGivenMST <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) {
    nErrorCode <- 0
    vResponse <- c( )

    # If inputs are Median Survival Times
    if ( SurvMethod == 3 ) {
        vMST <- as.numeric( SurvParam )
        vHRates <- log( 2 ) / vMST

        for ( nPatID in 1:NumSub ) {
            nArmIndex <- TreatmentID[ nPatID ] + 1
            vResponse[ nPatID ] <- stats::rexp( n = 1, rate = vHRates[ nArmIndex ] )
        }
    } else {
        nErrorCode <- -100
    }

    if ( length( vResponse ) != NumSub || any( is.na( vResponse ) == TRUE ) ) {
        nErrorCode <- -100
    }

    return( list( SurvivalTime = as.double( vResponse ), ErrorCode = as.integer( nErrorCode ) ) )
}
