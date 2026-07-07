########################################################################################################################
#' @name SimulatePatientOutcomeMultiArmExpGivenMST
#' @title Simulate survival outcomes for multi-arm clinical trial simulations given Median Survival Times (MST)
#' @description
#' Generates patient-level survival times under several survival distribution
#' parameterizations for multi-arm clinical trial simulations.
#'
#' @param NumSub Integer. Total number of subjects.
#' @param NumArm Integer. Number of treatment arms including control.
#' @param ArrivalTime Numeric vector containing patient arrival times.
#' @param TreatmentID Integer vector indicating treatment assignment for each patient.
#'        Control arm must be indexed as 0.
#' @param SurvMethod This example supports SurvMethod = 3, i.e. Median Survival Times.
#' @param NumPrd Integer specifying the number of survival periods.
#' @param PrdTime Numeric vector containing period boundary times.
#' @param SurvParam For SurvMethod = 3, this will be an array of arm-wise Median Survival Times. 
#' @param UserParam Optional user-defined list of custom parameters.
#'
#' @return List containing:
#'         \describe{
#'           \item{SurvivalTime}{Numeric vector of generated survival times.}
#'           \item{ErrorCode}{Integer error code. 0 indicates success and -100 indicates invalid output generation.}
#'         }
########################################################################################################################

SimulatePatientOutcomeMultiArmExpGivenMST <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{
    nError        <- 0
    vResponse     <- c()

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
    } else {
        nError      <- -100  
    }

    if(length( vResponse ) != NumSub || any( is.na( vResponse ) == TRUE ) )
        nError      <- -100
  
    return( list( SurvivalTime = as.double( vResponse ), ErrorCode = as.integer( nError ) ))
}
