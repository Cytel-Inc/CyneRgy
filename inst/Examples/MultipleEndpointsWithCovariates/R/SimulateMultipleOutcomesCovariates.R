######################################################################################################################## .
#' @name SimulateMultipleOutcomesCovariates
#' @title Simulate Multiple Independent Outcomes Using Covariates
#'
#' @description This function simulates three independent normally distributed outcomes for a given number of subjects,
#' based on their treatment assignment. Each outcome has a treatment-specific mean and a fixed standard deviation.
#' Two covariates are used in this version:
#'        \itemize{
#'          \item Covariate 1: binary (e.g., diabetic)
#'          \item Covariate 2: binary (e.g., smoker)
#'        }
#' Covariate effects are incorporated linearly into the outcome generation.
#' Note: this function can be extended to simulate any number of endpoints and covariates.
#' @author Julija Saltane
#'
#' @param NumSub Integer. Number of subjects to simulate.
#' @param ArrivalTime Arrival times of the subjects, numeric vector, length( ArrivalTime ) = NumSub.
#' @param TreatmentID Integer vector of length `NumSub`. Treatment assignment for each subject (for two arm confirmatory: 0 = control, 1 = treatment).
#' @param Mean Numeric. Not used directly in this function.
#' @param StdDev Numeric. Not used directly in this function.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'        \describe{
#'          \item{UserParam$MeanOutcome1Ctrl}{Mean of outcome 1 for the control group.}
#'          \item{UserParam$MeanOutcome1Trt}{Mean of outcome 1 for the treatment group.}
#'          \item{UserParam$MeanOutcome2Ctrl}{Mean of outcome 2 for the control group.}
#'          \item{UserParam$MeanOutcome2Trt}{Mean of outcome 2 for the treatment group.}
#'          \item{UserParam$MeanOutcome3Ctrl}{Mean of outcome 3 for the control group.}
#'          \item{UserParam$MeanOutcome3Trt}{Mean of outcome 3 for the treatment group.}
#'          \item{UserParam$Beta1}{Additive effect of covariate 1 on each outcome.}
#'          \item{UserParam$Beta2}{Additive effect of covariate 2 on each outcome.}
#'          \item{UserParam$Cov1Prob}{Probability that binary covariate 1 equals 1.}
#'          \item{UserParam$Cov2Prob}{Probability that binary covariate 2 equals 1.}
#'        }
#'
#' @return A list containing:
#'        \describe{
#'          \item{PatientOutcome1}{Numeric vector of simulated values for continuous outcome 1}
#'          \item{PatientOutcome2}{Numeric vector of simulated values for continuous outcome 2}
#'          \item{PatientOutcome3}{Numeric vector of simulated values for continuous outcome 3}
#'          \item{Covariate1}{Binary vector of simulated values for covariate 1}
#'          \item{Covariate2}{Binary vector of simulated values for covariate 2}
#'          \item{Response}{Placeholder, always a numeric vector of zeros (reserved for compatibility with other functions)}
#'          \item{ErrorCode}{Integer. 0 if successful, 1 if `UserParam` is NULL}
#'        }
#'
#' @examples
#' UserParam <- list(MeanOutcome1Ctrl = 10, MeanOutcome1Trt = 12,
#'                   MeanOutcome2Ctrl = 20, MeanOutcome2Trt = 22,
#'                   MeanOutcome3Ctrl = 30, MeanOutcome3Trt = 32,
#'                   Beta1 = 0.1, Beta2 = 2,
#'                   Cov1Prob = 0.2, Cov2Prob = 0.5)
#'
#' NumSub      <- 100
#' TreatmentID <- rep(c(0,1), NumSub / 2, replace = TRUE)
#'
#' result <- SimulateMultipleOutcomesCovariates(NumSub = NumSub,
#'                                              ArrivalTime = NULL,
#'                                              TreatmentID = TreatmentID,
#'                                              Mean = NULL,
#'                                              StdDev = NULL,
#'                                              UserParam = UserParam)
#'
######################################################################################################################## .

SimulateMultipleOutcomesCovariates <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL )
{

    # Initialize the return variables that will contain results for 3 normal endpoints
    vPatientOutcome1 <- rep( 0, NumSub )
    vPatientOutcome2 <- rep( 0, NumSub )
    vPatientOutcome3 <- rep( 0, NumSub )

    # Validate custom variable input and set defaults
    nError <- 0

    if( is.null( UserParam ) )
    {
        nError <- 1
    }

    # Extract means for each outcome and group
    vMeansOutcome1 <- c( UserParam$MeanOutcome1Ctrl, UserParam$MeanOutcome1Trt )
    vMeansOutcome2 <- c( UserParam$MeanOutcome2Ctrl, UserParam$MeanOutcome2Trt )
    vMeansOutcome3 <- c( UserParam$MeanOutcome3Ctrl, UserParam$MeanOutcome3Trt )

    # Extract covariate effects
    dBeta1 <- UserParam$Beta1
    dBeta2 <- UserParam$Beta2

    # Simulate the effect of covariates
    vCovariate1 <- rbinom( NumSub, size = 1, prob = UserParam$Cov1Prob )
    vCovariate2 <- rbinom( NumSub, size = 1, prob = UserParam$Cov2Prob )

    vCovariateEffect <- dBeta1 * vCovariate1 + dBeta2 * vCovariate2

    # Simulate the patient independent outcome data
    for( nPatientIndex in 1:NumSub )
    {
        # Convert 0(Ctrl) -> 1 to 1 (Trt) -> 2 for indexing
        nTreatmentID <- TreatmentID[ nPatientIndex ] + 1

        vPatientOutcome1[ nPatientIndex ] <- rnorm( 1, mean = vMeansOutcome1[ nTreatmentID ] + vCovariateEffect[ nPatientIndex ], sd = 1 )
        vPatientOutcome2[ nPatientIndex ] <- rnorm( 1, mean = vMeansOutcome2[ nTreatmentID ] + vCovariateEffect[ nPatientIndex ], sd = 1 )
        vPatientOutcome3[ nPatientIndex ] <- rnorm( 1, mean = vMeansOutcome3[ nTreatmentID ] + vCovariateEffect[ nPatientIndex ], sd = 1 )
    }

    # Return the simulated outcomes and error code
    lReturn <- list( PatientOutcome1 = as.double( vPatientOutcome1 ),
                     PatientOutcome2 = as.double( vPatientOutcome2 ),
                     PatientOutcome3 = as.double( vPatientOutcome3 ),
                     Covariate1 = as.double( vCovariate1 ),
                     Covariate2 = as.double( vCovariate2 ),
                     Response   = as.double( rep( 0, NumSub ) ),
                     ErrorCode  = as.integer( nError ) )

    return( lReturn )
}
