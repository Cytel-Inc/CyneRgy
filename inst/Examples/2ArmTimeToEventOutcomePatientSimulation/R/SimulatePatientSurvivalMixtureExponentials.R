######################################################################################################################## .
#' @name SimulatePatientSurvivalMixtureExponentials
#' @title Simulate patient outcomes from a mixture of Exponential distributions.
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
#'  In this example, UserParam must contain the following named elements:
#'  \describe{
#'       \item{UserParam$QtyOfSubgroups}{Number of patient subgroups.}
#'       \item{UserParam$ProbSubgroup1, ..., UserParam$ProbSubgroupN}{Probability that a patient belongs to subgroup `1` through `N`, where `N` is `UserParam$QtyOfSubgroups`.}
#'       \item{UserParam$MedianTTECtrlSubgroup1, ..., UserParam$MedianTTECtrlSubgroupN}{Median time to event on control for each subgroup.}
#'       \item{UserParam$MedianTTEExpSubgroup1, ..., UserParam$MedianTTEExpSubgroupN}{Median time to event on experimental treatment for each subgroup.}
#'  }
#' @description
#'  This function simulates patient data from a mixture of Exponential distributions. The mixture is based on patient subgroups.  For each,
#'  subgroup you specify the median time-to-event for the control and experimental treatments as well as the probability a patient belongs in a specific group.
#'  The required function signature for integration with East Horizon includes the SurvMethod, NumPrd, PrdTime and SurvParam which are ignored in this function
#'  and only the parameters in UserParam are utilized.
#' @return A list that contains:
#' \describe{
#'     \item{SurvivalTime}{A numeric vector of length `NumSub` containing the simulated survival times.}
#'     \item{Subgroup}{An integer vector of length `NumSub` containing the simulated subgroup assignments.}
#'     \item{ErrorCode}{An integer value: ErrorCode = 0 indicates no error; ErrorCode > 0 indicates a nonfatal error and aborts the current simulation, but subsequent simulations continue; ErrorCode < 0 indicates a fatal error and stops further simulation.}
#' }
######################################################################################################################## .

SimulatePatientSurvivalMixtureExponentials <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{

    # Step 1 - Setup variables that we need ####
    vSurvTime    <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.

    vTreatmentID <- TreatmentID +1   # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1
    ErrorCode    <- as.integer( 0 )

    ## Step 1.1 Read the UserParam and create required variables ####
    nQtyOfSubgroups <- UserParam$QtyOfSubgroups
    vProbOfSubgroup <- rep( NA, nQtyOfSubgroups )   # The probability a patient is in each group
    vMedianTTECtrl  <- rep( NA, nQtyOfSubgroups )   # The medians for the control treatment for each subgroup
    vMedianTTEExp   <- rep( NA, nQtyOfSubgroups )   # The medians for the experimental treatment for each subgroup
    for( nGroup in 1:nQtyOfSubgroups )
    {
        vProbOfSubgroup[ nGroup ] <- UserParam[[ paste0( "ProbSubgroup", nGroup ) ] ]
        vMedianTTECtrl[ nGroup ]  <- UserParam[[ paste0( "MedianTTECtrlSubgroup", nGroup ) ] ]
        vMedianTTEExp[ nGroup ]   <- UserParam[[ paste0( "MedianTTEExpSubgroup", nGroup ) ] ]
    }

    # To use the rexp function to generate the TTE we need the rate parameter.
    # In the case where data is simulated from an exponential distribution the following statement are helpful:
    #     rate   = 1/Mean
    #     Median = ln(2) * Mean
    #     Median = ln(2)/rate
    #     rate   = ln(2)/Median

    vRateCtrl <- log( 2 ) / vMedianTTECtrl
    vRateExp  <- log( 2 ) / vMedianTTEExp

    mRates    <- rbind( vRateCtrl, vRateExp )  # Now mRates has the rates for Ctrl in row 1, Exp in row 2 and the columns are the groups

    # Step 2 - Simulate the patient data using the variables above ####

    # Simulate the patient groups
    vPatientGroup <- sample( c( 1:nQtyOfSubgroups ), NumSub, replace = TRUE, prob = vProbOfSubgroup )

    # Simulate the patient survival times based on the patient group and treatment
    for( nPatIndx in 1:NumSub )
    {
        nPatientTreatment     <- vTreatmentID[ nPatIndx ]
        nPatientGroup         <- vPatientGroup[ nPatIndx ]
        dRate                 <- mRates[ nPatientTreatment, nPatientGroup ]
        vSurvTime[ nPatIndx ] <- rexp( 1, dRate )
    }

    return( list( SurvivalTime = as.double( vSurvTime ), Subgroup = as.double( vPatientGroup ), ErrorCode = ErrorCode ) )
}
