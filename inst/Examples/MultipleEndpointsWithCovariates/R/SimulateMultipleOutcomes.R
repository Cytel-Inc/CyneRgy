######################################################################################################################## .
#' @name SimulateMultipleOutcomes
#' @title Simulate Multiple Independent Outcomes
#'
#' @description This function simulates three independent normally distributed outcomes for a given number of subjects,
#' based on their treatment assignment. Each outcome has a treatment-specific mean and a fixed standard deviation.
#' Covariates are not used in this version.
#' Note: this code can be extended to any number of endpoints.
#'
#' @param NumSub Integer. Number of subjects to simulate.
#' @param ArrivalTime Arrival times of the subjects, numeric vector, length( ArrivalTime ) = NumSub.
#' @param TreatmentID Integer vector of length `NumSub`. Treatment assignment for each subject (for two arm confirmatory: 0 = control, 1 = treatment).
#' @param Mean Numeric. Not used directly in this function.
#' @param StdDev Numeric. Not used directly in this function.
#' @param UserParam List. Contains treatment-specific means for each outcome:
#'        \describe
#'        {
#'          \item{MeanOutcome1Ctrl}{Mean of outcome 1 for control group}
#'          \item{MeanOutcome1Trt} {Mean of outcome 1 for treatment group}
#'          \item{MeanOutcome2Ctrl}{Mean of outcome 2 for control group}
#'          \item{MeanOutcome2Trt} {Mean of outcome 2 for treatment group}
#'          \item{MeanOutcome3Ctrl}{Mean of outcome 3 for control group}
#'          \item{MeanOutcome3Trt} {Mean of outcome 3 for treatment group}
#'        }
#'
#' @return A list containing:
#'        \describe
#'        {
#'          \item{PatientOutcome1}{Numeric vector of simulated values for continuous outcome 1}
#'          \item{PatientOutcome2}{Numeric vector of simulated values for continuous outcome 2}
#'          \item{PatientOutcome3}{Numeric vector of simulated values for continuous outcome 3}
#'          \item{Response}{Placeholder, always a numeric vector of zeros (reserved for compatibility with other functions)}
#'          \item{ErrorCode}{Integer. 0 if successful, 1 if `UserParam` is NULL}
#'        }
#' 
#' @examples 
#' UserParam <- list( MeanOutcome1Ctrl = 10, MeanOutcome1Trt = 12,
#'                    MeanOutcome2Ctrl = 20, MeanOutcome2Trt = 22,
#'                    MeanOutcome3Ctrl = 30, MeanOutcome3Trt = 32 )
#' 
#' result <- SimulateMultipleOutcomes( NumSub = 100,
#'                                     ArrivalTime = NULL,
#'                                     TreatmentID = sample( 0:1, 100, replace = TRUE ),
#'                                     Mean = NULL, 
#'                                     StdDev = NULL, 
#'                                     UserParam = UserParam )
#' @export

######################################################################################################################## .

SimulateMultipleOutcomes <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL ) 
{
    
    # Initialize the return variables that will contain results for 3 normal endpoints 
    vPatientOutcome1 <- rep( 0, NumSub ) 
    vPatientOutcome2 <- rep( 0, NumSub )
    vPatientOutcome3 <- rep( 0, NumSub )
    
    # Validate custom variable input and set defaults 
    nError <- 0
    
    if ( is.null( UserParam ) ) 
    {
        nError <- 1
    }
    
    # Extract means for each outcome and arm from UserParam 
    vMeansOutcome1 <- c( UserParam$MeanOutcome1Ctrl, UserParam$MeanOutcome1Trt )
    vMeansOutcome2 <- c( UserParam$MeanOutcome2Ctrl, UserParam$MeanOutcome2Trt )
    vMeansOutcome3 <- c( UserParam$MeanOutcome3Ctrl, UserParam$MeanOutcome3Trt )
    
    # Simulate the patient independent outcome data 
    for ( nPatientIndex in 1:NumSub ) 
    {
        # Convert 0(Ctrl) -> 1 to 1 (Trt) -> 2 for indexing
        nTreatmentID <- TreatmentID[ nPatientIndex ] + 1  
        
        vPatientOutcome1[ nPatientIndex ] <- rnorm( 1, mean = vMeansOutcome1[ nTreatmentID ], sd = 1 )
        vPatientOutcome2[ nPatientIndex ] <- rnorm( 1, mean = vMeansOutcome2[ nTreatmentID ], sd = 1 )
        vPatientOutcome3[ nPatientIndex ] <- rnorm( 1, mean = vMeansOutcome3[ nTreatmentID ], sd = 1 )
    }
    
    # Return the simulated outcomes and error code 
    lReturn <- list( PatientOutcome1 = as.double( vPatientOutcome1 ),
                     PatientOutcome2 = as.double( vPatientOutcome2 ),
                     PatientOutcome3 = as.double( vPatientOutcome3 ),
                     Response        = as.double( rep( 0, NumSub ) ),
                     ErrorCode       = as.integer( nError ) )
    
    return( lReturn )
}

