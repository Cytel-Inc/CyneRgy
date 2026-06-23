
########################################################################################################################
#' @name SimulatePatientOutcomeTTEGivenMST
#' @title Simulate survival outcomes for multi-arm clinical trial simulations given Median Survial Times (MST)
#' @description
#' Generates patient-level survival times under several survival distribution
#' parameterizations for multi-arm clinical trial simulations. The function supports:
#' \describe{
#'   \item{SurvMethod = 1}{Piecewise exponential survival model using hazard rates.}
#'   \item{SurvMethod = 2}{Survival probability driven piecewise exponential model.}
#'   \item{SurvMethod = 3}{Median survival time based exponential model.}
#' }
#'
#' @param NumSub Integer. Total number of subjects.
#' @param NumArm Integer. Number of treatment arms including control.
#' @param ArrivalTime Numeric vector containing patient arrival times.
#' @param TreatmentID Integer vector indicating treatment assignment for each patient.
#'        Control arm must be indexed as 0.
#' @param SurvMethod Integer specifying the survival generation method.
#' @param NumPrd Integer specifying the number of survival periods.
#' @param PrdTime Numeric vector containing period boundary times.
#' @param SurvParam Matrix of survival parameters. Interpretation depends on SurvMethod:
#'        \describe{
#'          \item{Method 1}{Piecewise hazard rates by period and arm.}
#'          \item{Method 2}{Survival probabilities by period and arm.}
#'          \item{Method 3}{Median survival times by arm.}
#'        }
#' @param UserParam Optional user-defined list of custom parameters.
#'
#' @return List containing:
#'         \describe{
#'           \item{SurvivalTime}{Numeric vector of generated survival times.}
#'           \item{ErrorCode}{Integer error code. 0 indicates success and -100 indicates invalid output generation.}
#'         }
########################################################################################################################

SimulatePatientOutcomeTTEGivenHRates <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{
    nError        <- 0
    vResponse     <- c()

    # If inputs are Median Survival Times
    if( SurvMethod == 3 )      
    {
        vMST        <- as.numeric(SurvParam)
        vHRates     <- log(2) / vMST

        for( nPatID in 1:NumSub )
        {
            nTrmt               <- TreatmentID[ nPatID ]
            vResponse[ nPatID ] <- rexp( n=1, rate=vHRates[ nTrmt ] )
        }
    } 

    if(length( vResponse ) != NumSub || any( is.na( vResponse ) == TRUE ) )
        nError      <- -100
  
    return( list( SurvivalTime = as.double( vResponse ), ErrorCode = as.integer( nError ) ))
}





