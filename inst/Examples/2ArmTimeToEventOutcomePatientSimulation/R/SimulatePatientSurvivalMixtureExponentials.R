######################################################################################################################## .
#' @name SimulatePatientSurvivalMixtureExponentials
#'
#' @title Simulate patient outcomes from a mixture of Exponential distributions.
#'
#' @description Generate survival times from a mixture of exponential distributions with user-defined mixture
#'   probabilities and median survival times.
#'
#' @author Valeria A. G. Mazzanti, J. Kyle Wathen, and Gabriel Potvin
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
#' If UserParam is supplied it must contain the following
#'  \describe{
#'       \item{UserParam$QtyOfSubgroups}{The quantity of patient subgroups. For each subgroup II =
#'         1,2..,QtyOfSubgroups,
#'       you must specify ProbSubgroupII, MedianTTECtrlSubgroupII, MedianTTEExpSubgroupII }
#'       \item{UserParam$ProbSubgroup1}{The probability a patient is in subgroup 1}
#'       \item{UserParam$MedianTTECtrlSubgroup1}{The median time-to-event for a patient in subgroup 1 that receives
#'         control treatment}
#'       \item{UserParam$MedianTTEExpSubgroup1}{The median time-to-event for a patient in subgroup 1 that receives
#'         experimental treatment}
#'       \item{UserParam$ProbSubgroup2}{The probability a patient is in subgroup 2}
#'       \item{UserParam$MedianTTECtrlSubgroup2}{The median time-to-event for a patient in subgroup 2 that receives
#'         control treatment}
#'       \item{UserParam$MedianTTEExpSubgroup2}{The median time-to-event for a patient in subgroup 2 that receives
#'         experimental treatment}
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
#'
#' @details SurvMethod, NumPrd, PrdTime, and SurvParam are accepted for compatibility with the response integration
#'   point but are ignored in this example. Supply the required distribution parameters through UserParam as
#'   documented above.
######################################################################################################################## .

SimulatePatientSurvivalMixtureExponentials <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) {
    # Step 1 - Setup variables that we need ####
    vSurvTime <- rep( -1, NumSub ) # The vector of patient survival times that will be returned.

    vTreatmentID <- TreatmentID + 1 # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1
    ErrorCode <- as.integer( 0 )

    ## Step 1.1 Read the UserParam and create required variables ####
    nQtyOfSubgroups <- UserParam$QtyOfSubgroups
    vProbOfSubgroup <- rep( NA, nQtyOfSubgroups ) # The probability a patient is in each group
    vMedianTTECtrl <- rep( NA, nQtyOfSubgroups ) # The medians for the control treatment for each subgroup
    vMedianTTEExp <- rep( NA, nQtyOfSubgroups ) # The medians for the experimental treatment for each subgroup
    for ( nGroup in 1:nQtyOfSubgroups ) {
        vProbOfSubgroup[ nGroup ] <- UserParam[[ paste0( "ProbSubgroup", nGroup ) ]]
        vMedianTTECtrl[ nGroup ] <- UserParam[[ paste0( "MedianTTECtrlSubgroup", nGroup ) ]]
        vMedianTTEExp[ nGroup ] <- UserParam[[ paste0( "MedianTTEExpSubgroup", nGroup ) ]]
    }

    # To use the rexp function to generate the TTE we need the rate parameter.
    # In the case where data is simulated from an exponential distribution the following statement are helpful:
    #     rate   = 1/Mean
    #     Median = ln(2) * Mean
    #     Median = ln(2)/rate
    #     rate   = ln(2)/Median

    vRateCtrl <- log( 2 ) / vMedianTTECtrl
    vRateExp <- log( 2 ) / vMedianTTEExp

    mRates <- rbind( vRateCtrl, vRateExp ) # Now mRates has the rates for Ctrl in row 1, Exp in row 2 and the columns are the groups

    # Step 2 - Simulate the patient data using the variables above ####

    # Simulate the patient groups
    vPatientGroup <- sample( c( 1:nQtyOfSubgroups ), NumSub, replace = TRUE, prob = vProbOfSubgroup )

    # Simulate the patient survival times based on the patient group and treatment
    for ( nPatIndx in 1:NumSub ) {
        nPatientTreatment <- vTreatmentID[ nPatIndx ]
        nPatientGroup <- vPatientGroup[ nPatIndx ]
        dRate <- mRates[ nPatientTreatment, nPatientGroup ]
        vSurvTime[ nPatIndx ] <- stats::rexp( 1, dRate )
    }

    return( list( SurvivalTime = as.double( vSurvTime ), Subgroup = as.double( vPatientGroup ), ErrorCode = ErrorCode ) )
}
