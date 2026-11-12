######################################################################################################################## .
#' @name SimulatePatientSurvivalWeibull
#' @title Simulate patient outcomes from a Weibull distribution.
#' @description Generate Weibull survival times using the arm-specific shape and scale parameters
#'   supplied through UserParam.
#' @author Valeria A. G. Mazzanti, J. Kyle Wathen, and Gabriel Potvin
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
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
#'
#' Example-specific parameters and requirements:
#' If UserParam is supplied it must contain the following:
#'  \describe{
#'   \item{UserParam$dShapeCtrl}{Positive numeric Weibull shape parameter for the control arm, as used by
#'     `stats::rweibull()`.}
#'   \item{UserParam$dScaleCtrl}{Positive numeric Weibull scale parameter for the control arm, as used by
#'     `stats::rweibull()`.}
#'   \item{UserParam$dShapeExp}{Positive numeric Weibull shape parameter for the experimental arm, as used by
#'     `stats::rweibull()`.}
#'   \item{UserParam$dScaleExp}{Positive numeric Weibull scale parameter for the experimental arm, as used by
#'     `stats::rweibull()`.}
#'  }
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details Supply the required distribution parameters through UserParam as documented above. With UserParam = NULL, the control shape and scale are both 1 and the experimental
#'   shape and scale are both 12.
######################################################################################################################## .

SimulatePatientSurvivalWeibull <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) {
    # Step 1 - Initialize the return variables or other variables needed ####
    vSurvTime <- rep( -1, NumSub ) # The vector of patient survival times that will be returned.
    vTreatmentID <- TreatmentID + 1 # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1
    nErrorCode <- as.integer( 0 )

    # Step 2 - Validate custom variable input and set defaults ####
    if ( is.null( UserParam ) ) {
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues. Also, if there is a default value for the parameters you may want to set them here. Default values usually
        # are applied to have the same functionality as East Horizon, see the first example

        # EXAMPLE - Set the default if needed
        UserParam <- list( dShapeCtrl = 1, dShapeExp = 12, dScaleCtrl = 1, dScaleExp = 12 )
    }

    # Step 2 - Read the user parameters into a vector to make it easier to simulate outcomes ####
    vShapes <- c( UserParam$dShapeCtrl, UserParam$dShapeExp )
    vScales <- c( UserParam$dScaleCtrl, UserParam$dScaleExp )

    # Simulate the patient survival times based on the treatment
    # Use the custom Weibull shape and scale parameters for each arm.
    for ( nPatIndx in 1:NumSub ) {
        nPatientTreatment <- vTreatmentID[ nPatIndx ]
        vSurvTime[ nPatIndx ] <- stats::rweibull( 1, vShapes[ nPatientTreatment ], vScales[ nPatientTreatment ] )
    }

    return( list( SurvivalTime = as.double( vSurvTime ), ErrorCode = nErrorCode ) )
}
