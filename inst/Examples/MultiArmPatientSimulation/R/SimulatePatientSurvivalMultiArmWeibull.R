######################################################################################################################## .
#' @name SimulatePatientSurvivalMultiArmWeibull
#'
#' @title Simulate patient time-to-event outcomes from a Weibull distribution for multi-arm trials
#'
#' @description Generate Weibull survival times for multiple treatment arms, using arm-specific shapes and survival
#'   inputs.
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
#' Example-specific parameters and requirements:
#' If UserParam is supplied it must contain the following:
#'  \describe{
#'       \item{UserParam$dShapeCtrl}{The shape parameter in the Weibull distribution for the control treatment}
#'       \item{UserParam$dScaleCtrl}{The scale parameter in the Weibull distribution for the control treatment}
#'       \item{UserParam$dShapeExp1}{The shape parameter in the Weibull distribution for the experimental treatment
#'         1}
#'       \item{UserParam$dScaleExp1}{The scale parameter in the Weibull distribution for the experimental treatment
#'         1}
#'       \item{UserParam$dShapeExp2}{The shape parameter in the Weibull distribution for the experimental treatment
#'         2}
#'       \item{UserParam$dScaleExp2}{The scale parameter in the Weibull distribution for the experimental treatment
#'         2}
#'  }
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject. Required.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

SimulatePatientSurvivalMultiArmWeibull <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) {
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
        UserParam <- list(
            dShapeCtrl = 1, dShapeExp1 = 12, dShapeExp2 = 12,
            dScaleCtrl = 1, dScaleExp1 = 12, dScaleExp2 = 12
        )
    }

    # Step 2 - Read the user parameters into a vector to make it easier to simulate outcomes ####
    vShapes <- c( UserParam$dShapeCtrl, UserParam$dShapeExp1, UserParam$dShapeExp2 )
    vScales <- c( UserParam$dScaleCtrl, UserParam$dScaleExp1, UserParam$dScaleExp2 )

    # Simulate the patient survival times based on the treatment
    # For the Hazard Rate input with 1 piece, this is just simulating from an exponential distribution as an example and results will match
    # East Horizon if you used the build hazard option.
    for ( nPatIndx in 1:NumSub ) {
        nPatientTreatment <- vTreatmentID[ nPatIndx ]
        vSurvTime[ nPatIndx ] <- stats::rweibull( 1, vShapes[ nPatientTreatment ], vScales[ nPatientTreatment ] )
    }

    return( list( SurvivalTime = as.double( vSurvTime ), ErrorCode = nErrorCode ) )
}
