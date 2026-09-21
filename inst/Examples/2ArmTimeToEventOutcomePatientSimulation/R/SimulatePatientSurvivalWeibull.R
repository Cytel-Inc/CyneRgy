######################################################################################################################## .
#' @name SimulatePatientSurvivalWeibull
#' @title Simulate patient outcomes from a Weibull distribution.
#' @author Valeria A. G. Mazzanti and J. Kyle Wathen
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
#'  \describe{
#'       \item{UserParam$dShapeCtrl}{The shape parameter in the Weibull distribution for the control treatment}
#'       \item{UserParam$dScaleCtrl}{The scale parameter in the Weibull distribution for the control treatment}
#'       \item{UserParam$dShapeExp}{The shape parameter in the Weibull distribution for the experimental treatment}
#'       \item{UserParam$dScaleExp}{The scale parameter in the Weibull distribution for the experimental treatment}
#'  }
#' @description
#'  This function simulates patient data from a Weibull( shape, scale ) distribution. The rweibull function in the stats package
#'  is used to simulate the survival time. See help on rweibull.
#'  The required function signature for integration with East Horizon includes the SurvMethod, NumPrd, PrdTime and SurvParam which are ignored in this function
#'  and only the parameters in UserParam are utilized.
#' @return A list that contains:
#' \describe{
#'     \item{SurvivalTime}{A numeric vector of length `NumSub` containing the simulated survival times.}
#'     \item{ErrorCode}{An integer value: ErrorCode = 0 indicates no error; ErrorCode > 0 indicates a nonfatal error and aborts the current simulation, but subsequent simulations continue; ErrorCode < 0 indicates a fatal error and stops further simulation.}
#' }
######################################################################################################################## .

SimulatePatientSurvivalWeibull <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{

    # Step 1 - Initialize the return variables or other variables needed ####
    vSurvTime    <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.
    vTreatmentID <- TreatmentID + 1   # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1
    ErrorCode    <- as.integer( 0 )

    # Step 2 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {

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
    # For the Hazard Rate input with 1 piece, this is just simulating from an exponential distribution as an example and results will match
    # East Horizon if you used the build hazard option.
    for( nPatIndx in 1:NumSub )
    {
        nPatientTreatment     <- vTreatmentID[ nPatIndx ]
        vSurvTime[ nPatIndx ] <- rweibull( 1, vShapes[ nPatientTreatment ], vScales[ nPatientTreatment ] )
    }

    return( list( SurvivalTime = as.double( vSurvTime ), ErrorCode = ErrorCode ) )
}
