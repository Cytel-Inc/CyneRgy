######################################################################################################################## .
#' @name SimulatePatientOutcomeMultiArmExpGivenMST
#' @title Simulate survival outcomes for multi-arm clinical trial simulations given Median Survival Times (MST)
#' @description
#' Generates patient-level survival times under several survival distribution
#' parameterizations for multi-arm clinical trial simulations.
#' @author Anoop Singh Rawat
#'
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#'        Control arm must be indexed as 0.
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
#'
#' @return List containing:
#'         \describe{
#'           \item{SurvivalTime}{Numeric vector of generated survival times.}
#'           \item{ErrorCode}{An integer value: ErrorCode = 0 indicates no error; ErrorCode > 0 indicates a nonfatal error and aborts the current simulation, but subsequent simulations continue; ErrorCode < 0 indicates a fatal error and stops further simulation. In this function, ErrorCode = -100 indicates invalid output generation.}
#'         }
######################################################################################################################## .

SimulatePatientOutcomeMultiArmExpGivenMST <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{
    nError    <- 0
    vResponse <- c()

    # If inputs are Median Survival Times
    if( SurvMethod == 3 )
    {
        vMST        <- as.numeric( SurvParam )
        vHRates     <- log( 2 ) / vMST

        for( nPatID in 1:NumSub )
        {
            nArmIndex           <- TreatmentID[ nPatID ] + 1
            vResponse[ nPatID ] <- rexp( n = 1, rate = vHRates[ nArmIndex ] )
        }
    }
    else
    {
        nError <- -100
    }

    if( length( vResponse ) != NumSub || any( is.na( vResponse ) == TRUE ) )
        nError <- -100

    return( list( SurvivalTime = as.double( vResponse ), ErrorCode = as.integer( nError ) ) )
}
