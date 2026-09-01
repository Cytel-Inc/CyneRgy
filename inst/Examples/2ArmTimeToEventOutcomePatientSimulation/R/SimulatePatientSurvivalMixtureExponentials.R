######################################################################################################################## .
#' @name SimulatePatientSurvivalMixtureExponentials
#' @title Simulate patient outcomes from a mixture of Exponential distributions.
#' @author Valeria A. G. Mazzanti, J. Kyle Wathen, and Gabriel Potvin
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param SurvMethod - This values is pulled from the Input Method drop-down list. This will be 1 (Hazard Rate), 2 (Cumulative \% survival), 3 (Medians)
#' @param NumPrd Number of time periods that are provided.
#' @param PrdTime Numeric matrix with `NumPrd` rows and `NumArm` columns, indicating the times used to specify survival parameters. For `SurvMethod = 1`, entries are hazard-piece start times; for `SurvMethod = 2`, entries are times at which cumulative survival is specified; for `SurvMethod = 3`, entries default to 0.
#' @param SurvParam \describe{Depends on the table in the Response Generation tab. 2‐D array of parameters to generate the survival times
#'    \item{If SurvMethod is 1}{SurvParam is an array (NumPrd rows, NumArm columns) that specifies arm by arm hazard rates (one rate per arm per piece).
#'    Thus SurvParam [i, j] specifies hazard rate in ith period for jth arm.
#'    Arms are in columns with column 1 is control, column 2 is experimental
#'    Time periods are in rows, row 1 is time period 1, row 2 is time period 2...}
#'    \item{If SurvMethod is 2}{SurvParam is an array (NumPrd rows,NumArm columns) specifies arm by arm the Cum \% Survivals (one value per arm per piece). Thus, SurvParam [i, j] specifies Cum \% Survivals in ith period for jth arm.}
#'    \item{If SurvMethod is 3}{SurvParam will be a 1 x 2 array with median survival times on each arms. Column 1 is control, column 2 is experimental }
#'  }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'  If UserParam is supplied it must contain the following
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
#' @return A named list containing `SurvivalTime`, simulated `Subgroup` assignments, and integer `ErrorCode`.
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
