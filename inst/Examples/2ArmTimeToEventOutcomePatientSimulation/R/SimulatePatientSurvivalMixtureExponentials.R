######################################################################################################################## .
#' @title Simulate TTE Patient Outcomes from a Mixture Exponential Distribution
#'
#' @description
#' This function simulates patient data from a mixture of Exponential distributions. The mixture is based on patient subgroups.  
#' For each subgroup, you specify the median time-to-event for the control and experimental treatments as well as the probability 
#' a patient belongs in a specific group. The required function signature for integration with East or East Horizon includes the SurvMethod, 
#' NumPrd, PrdTime, and SurvParam, which are ignored in this function, and only the parameters in UserParam are utilized.
#'
#' @param NumSub The number of patient times to generate for the trial. This is a single numeric value, e.g., 250.
#' @param NumArm The number of arms in the trial, a single numeric value. For a two-arm trial, this will be 2.
#' @param TreatmentID A vector of treatment IDs, 0 = treatment 1, 1 = treatment 2. The length of TreatmentID must equal NumSub.
#' @param SurvMethod This value is pulled from the Input Method drop-down list. It will be 1 (Hazard Rate), 2 (Cumulative percentage survival), or 3 (Medians).
#' @param NumPrd The number of time periods that are provided.
#' @param PrdTime \describe{
#'      \item{If SurvMethod = 1}{PrdTime is a vector of starting times of hazard pieces.}
#'      \item{If SurvMethod = 2}{Times at which the cumulative percentage survivals are specified.}
#'      \item{If SurvMethod = 3}{Period time is 0 by default.}
#'      }
#' @param SurvParam \describe{
#'      Depends on the table in the Response Generation tab. A 2-D array of parameters to generate the survival times:
#'      \item{If SurvMethod = 1}{SurvParam is an array (NumPrd rows, NumArm columns) that specifies arm-by-arm hazard rates 
#'      (one rate per arm per piece). Thus, SurvParam[i, j] specifies the hazard rate in the ith period for the jth arm. 
#'      Arms are in columns, with column 1 being control and column 2 being experimental. Time periods are in rows, with row 1 
#'      being time period 1, row 2 being time period 2, etc.}
#'      \item{If SurvMethod = 2}{SurvParam is an array (NumPrd rows, NumArm columns) that specifies arm-by-arm cumulative percentage survivals 
#'      (one value per arm per piece). Thus, SurvParam[i, j] specifies the cumulative percentage survival in the ith period for the jth arm.}
#'      \item{If SurvMethod = 3}{SurvParam will be a 1 x 2 array with median survival times for each arm. Column 1 is control, column 2 is experimental.}
#'      }
#' @param UserParam A list of user-defined parameters in East or East Horizon. The default is NULL. If UserParam is supplied, 
#' it must contain the following:
#' \describe{
#'      \item{UserParam$QtyOfSubgroups}{The quantity of patient subgroups. For each subgroup II = 1,2,...,QtyOfSubgroups, 
#'      you must specify ProbSubgroupII, MedianTTECtrlSubgroupII, and MedianTTEExpSubgroupII.}
#'      \item{UserParam$ProbSubgroup1}{The probability a patient is in subgroup 1.}
#'      \item{UserParam$MedianTTECtrlSubgroup1}{The median time-to-event for a patient in subgroup 1 that receives control treatment.}
#'      \item{UserParam$MedianTTEExpSubgroup1}{The median time-to-event for a patient in subgroup 1 that receives experimental treatment.}
#'      \item{UserParam$ProbSubgroup2}{The probability a patient is in subgroup 2.}
#'      \item{UserParam$MedianTTECtrlSubgroup2}{The median time-to-event for a patient in subgroup 2 that receives control treatment.}
#'      \item{UserParam$MedianTTEExpSubgroup2}{The median time-to-event for a patient in subgroup 2 that receives experimental treatment.}
#' }
#' 
#' @return A list with the following components:
#' \item{`SurvivalTime`}{A vector of simulated survival times for patients.}
#' \item{`Subgroup`}{A vector of the patient subgroups.}
#' \item{`ErrorCode`}{Optional integer value:
#'                      \describe{
#'                        \item{0}{No error.}
#'                        \item{> 0}{Non-fatal error; current simulation is aborted but subsequent simulations continue.}
#'                        \item{< 0}{Fatal error; no further simulations are attempted.}
#'                      }}
#' @export
######################################################################################################################## .

SimulatePatientSurvivalMixtureExponentials <- function(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
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
        vProbOfSubgroup[ nGroup ] <- UserParam[[ paste0( "ProbSubgroup", nGroup ) ]]
        vMedianTTECtrl[ nGroup ]  <- UserParam[[ paste0( "MedianTTECtrlSubgroup", nGroup ) ]]
        vMedianTTEExp[ nGroup ]   <- UserParam[[ paste0( "MedianTTEExpSubgroup", nGroup ) ]]
    }
    
    # To use the rexp function to generate the TTE we need the rate parameter. 
    # In the case where data is simulated from an exponential distribution the following statement are helpful:
    #     rate   = 1/Mean
    #     Median = ln(2) * Mean 
    #     Median = ln(2)/rate 
    #     rate   = ln(2)/Median
    
    vRateCtrl <- log(2)/vMedianTTECtrl
    vRateExp  <- log(2)/vMedianTTEExp 
    
    mRates    <- rbind( vRateCtrl, vRateExp)  # Now mRates has the rates for Ctrl in row 1, Exp in row 2 and the columns are the groups
    
    # Step 2 - Simulate the patient data using the variables above ####
    
    # Simulate the patient groups
    vPatientGroup <- sample( c(1:nQtyOfSubgroups), NumSub, replace = TRUE, prob = vProbOfSubgroup )
    
    
    # Simulate the patient survival times based on the patient group and treatment
    for( nPatIndx in 1:NumSub)  
    {
        nPatientTreatment     <- vTreatmentID[ nPatIndx ]
        nPatientGroup         <- vPatientGroup[ nPatIndx ]
        dRate                 <- mRates[ nPatientTreatment, nPatientGroup ]
        vSurvTime[ nPatIndx ] <- rexp( 1, dRate )
    }
    
    return(list(SurvivalTime = as.double(vSurvTime), Subgroup = as.double( vPatientGroup ), ErrorCode = ErrorCode) )
}