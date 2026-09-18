######################################################################################################################## .
#' @name SimulatePatientSurvivalMultiArmWeibull
#' @title Simulate Multi-Arm Time-to-Event Outcomes from Weibull Distributions
#' @description Simulates patient survival times from arm-specific Weibull distributions supplied through
#' `UserParam`. The integration-point arguments `SurvMethod`, `NumPrd`, `PrdTime`, and `SurvParam` are retained but
#' are not used by this example.
#' @author Anoop Singh Rawat
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param SurvMethod Integer survival-generation method: 1 for hazard rates, 2 for cumulative survival probabilities, or 3 for median survival times.
#' @param NumPrd Integer number of survival periods.
#' @param PrdTime Numeric matrix with `NumPrd` rows and `NumArm` columns, indicating the times used to specify survival parameters. For `SurvMethod = 1`, entries are hazard-piece start times; for `SurvMethod = 2`, entries are times at which cumulative survival is specified; for `SurvMethod = 3`, entries default to 0.
#' @param SurvParam Numeric matrix with `NumPrd` rows and `NumArm` columns containing arm-specific survival parameters.
#'   \describe{
#'     \item{SurvMethod = 1}{Hazard rates for each period and arm. Entry `[i, j]` is the hazard rate in period `i` for arm `j`.}
#'     \item{SurvMethod = 2}{Cumulative survival probabilities for each period and arm. Entry `[i, j]` is the cumulative survival probability in period `i` for arm `j`.}
#'     \item{SurvMethod = 3}{One row of median survival times, with one value per arm.}
#'   }
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#' In this example, UserParam must contain the following named elements:
#'  \describe{
#'       \item{UserParam$dShapeCtrl}{The shape parameter in the Weibull distribution for the control treatment}
#'       \item{UserParam$dScaleCtrl}{The scale parameter in the Weibull distribution for the control treatment}
#'       \item{UserParam$dShapeExp1}{The shape parameter in the Weibull distribution for the experimental treatment 1}
#'       \item{UserParam$dScaleExp1}{The scale parameter in the Weibull distribution for the experimental treatment 1}
#'       \item{UserParam$dShapeExp2}{The shape parameter in the Weibull distribution for the experimental treatment 2}
#'       \item{UserParam$dScaleExp2}{The scale parameter in the Weibull distribution for the experimental treatment 2}
#'  }
#' @return A list containing `SurvivalTime`, a numeric vector of length `NumSub`, and `ErrorCode`, an integer status
#' code where 0 indicates success.
######################################################################################################################## .

SimulatePatientSurvivalMultiArmWeibull <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{
    # Step 1 - Initialize the return variables or other variables needed ####
    vSurvTime    <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.
    vTreatmentID <- TreatmentID + 1    # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1
    nErrorCode   <- as.integer( 0 )

    # Step 2 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {

        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues. Also, if there is a default value for the parameters you may want to set them here. Default values usually
        # are applied to have the same functionality as East Horizon, see the first example

        # EXMAPLE - Set the default if needed
        UserParam <- list( dShapeCtrl = 1, dShapeExp1 = 12, dShapeExp2 = 12,
                           dScaleCtrl = 1, dScaleExp1 = 12, dScaleExp2 = 12 )
    }

    # Step 2 - Read the user parameters into a vector to make it easier to simulate outcomes ####
    vShapes <- c( UserParam$dShapeCtrl, UserParam$dShapeExp1, UserParam$dShapeExp2 )
    vScales <- c( UserParam$dScaleCtrl, UserParam$dScaleExp1, UserParam$dScaleExp2 )

    # Simulate the patient survival times based on the treatment
    # For the Hazard Rate input with 1 piece, this is just simulating from an exponential distribution as an example and results will match
    # East Horizon if you used the build hazard option.
    for( nPatIndx in 1:NumSub )
    {
        nPatientTreatment     <- vTreatmentID[ nPatIndx ]
        vSurvTime[ nPatIndx ] <- rweibull( 1, vShapes[ nPatientTreatment ], vScales[ nPatientTreatment ] )

    }

    return( list( SurvivalTime = as.double( vSurvTime ), ErrorCode = nErrorCode ) )
}
