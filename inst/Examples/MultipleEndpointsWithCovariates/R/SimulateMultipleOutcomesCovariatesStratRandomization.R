######################################################################################################################## .
#' @name SimulateMultipleOutcomesCovariatesStratRandomization
#' @title Simulate Multiple Independent Outcomes with Covariate Effects and Stratified Randomization
#'
#' @description This function simulates three independent normally distributed outcomes for a given number of subjects,
#' incorporating covariate effects. This function performs stratified randomization based on the two covariates:
#'        \itemize{
#'          \item Covariate 1: binary (e.g., diabetic)
#'          \item Covariate 2: binary (e.g., smoker)
#'        }
#' Covariate effects are incorporated linearly into the outcome generation.
#' Note: this function can be extended to simulate any number of endpoints and covariates.
#' @author Julija Saltane
#'
#' @param NumSub Integer number of subjects in the trial.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' generating new treatment IDs (0 = control, 1 = treatment).
#' @param Mean Numeric vector of arm-specific outcome means. Not used by this example.
#' @param StdDev Numeric vector of arm-specific outcome standard deviations. Not used by this example.
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
#'          \item{UserParam$AllocRatio}{Treatment-to-control allocation ratio; 1 gives equal allocation and 2 assigns twice as many patients to treatment.}
#'        }
#'
#' @return A list containing:
#'        \describe{
#'          \item{PatientOutcome1}{Numeric vector of simulated values for continuous outcome 1}
#'          \item{PatientOutcome2}{Numeric vector of simulated values for continuous outcome 2}
#'          \item{PatientOutcome3}{Numeric vector of simulated values for continuous outcome 3}
#'          \item{Covariate1}{Binary vector of simulated values for covariate 1}
#'          \item{Covariate2}{Binary vector of simulated values for covariate 2}
#'          \item{PatientTreatmentID} {Vector of integer values where 0 indicates assignment to control group and 1 - to treatment group }
#'          \item{Response}{Placeholder, always a numeric vector of zeros (reserved for compatibility with other functions)}
#'          \item{ErrorCode}{Integer. 0 if successful, 1 if `UserParam` is NULL}
#'        }
#'
#' @examples
#' UserParam <- list( MeanOutcome1Ctrl = 10, MeanOutcome1Trt = 12,
#'                    MeanOutcome2Ctrl = 20, MeanOutcome2Trt = 22,
#'                    MeanOutcome3Ctrl = 30, MeanOutcome3Trt = 32,
#'                    Beta1 = 0.1, Beta2 = 2,
#'                    Cov1Prob = 0.2, Cov2Prob = 0.5,
#'                    AllocRatio = 1 )
#'
#' NumSub    <- 100
#'
#' result <- SimulateMultipleOutcomesCovariatesStratRandomization( NumSub = NumSub,
#'                                                                 ArrivalTime = NULL,
#'                                                                 TreatmentID = NULL,
#'                                                                 Mean = NULL,
#'                                                                 StdDev = NULL,
#'                                                                 UserParam = UserParam )
#'
######################################################################################################################## .

SimulateMultipleOutcomesCovariatesStratRandomization <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL )
{

    # Initialize the return variables that will contain results for 3 normal endpoints
    vPatientOutcome1 <- rep( 0, NumSub )  # First outcome
    vPatientOutcome2 <- rep( 0, NumSub )  # Second outcome
    vPatientOutcome3 <- rep( 0, NumSub )  # Third outcome

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
    vCovariate1      <- rbinom( NumSub, size = 1, prob = UserParam$Cov1Prob )
    vCovariate2      <- rbinom( NumSub, size = 1, prob = UserParam$Cov2Prob )
    vCovariateEffect <- dBeta1 * vCovariate1 + dBeta2 * vCovariate2

    # Create strata
    vCovariate1Groups <- factor( ifelse( vCovariate1 == 0, "Non-Diabetic", "Diabetic" ), levels = c( "Non-Diabetic", "Diabetic" ) )
    vCovariate2Groups <- factor( ifelse( vCovariate2 == 0, "Non-Smoker", "Smoker" ), levels = c( "Non-Smoker", "Smoker" ) )
    vStrata           <- interaction( vCovariate1Groups, vCovariate2Groups, sep = "_" )

    # Create allocation fraction
    vAllocRatio    <- c( 1, UserParam$AllocRatio )
    vAllocFraction <- c( vAllocRatio[ 1 ] / sum( vAllocRatio ), 1 - vAllocRatio[ 1 ] / sum( vAllocRatio ) )

    # Randomize treatment within each stratum
    vTreatmentID <- rep( 0, NumSub )

    for( strStrata in unique( vStrata ) )
    {
        # Determine indeces of patients in that strata
        vIndex      <- which( vStrata == strStrata )
        nSampleSize <- length( vIndex )

        # Calculate allocation fraction for control and treatment group
        nSampleSizeTrt <- nSampleSize - round( nSampleSize * vAllocFraction[ 1 ] )

        # Find the indices for treatment group
        vTreatmentArmIndex                 <- vIndex[ sample( 1:nSampleSize, size = nSampleSizeTrt, replace = FALSE ) ]
        vTreatmentID[ vTreatmentArmIndex ] <- 1
    }

    # Simulate the patient independent outcome data
    for( nPatientIndex in 1:NumSub )
    {
        # Convert 0(Ctrl) -> 1 to 1 (Trt) -> 2 for indexing
        nTreatmentID <- vTreatmentID[ nPatientIndex ] + 1

        vPatientOutcome1[ nPatientIndex ] <- rnorm( 1, mean = vMeansOutcome1[ nTreatmentID ] + vCovariateEffect[ nPatientIndex ], sd = 1 )
        vPatientOutcome2[ nPatientIndex ] <- rnorm( 1, mean = vMeansOutcome2[ nTreatmentID ] + vCovariateEffect[ nPatientIndex ], sd = 1 )
        vPatientOutcome3[ nPatientIndex ] <- rnorm( 1, mean = vMeansOutcome3[ nTreatmentID ] + vCovariateEffect[ nPatientIndex ], sd = 1 )
    }

    # Return the simulated outcomes and error code
    lReturn <- list( PatientOutcome1    = as.double( vPatientOutcome1 ),
                     PatientOutcome2    = as.double( vPatientOutcome2 ),
                     PatientOutcome3    = as.double( vPatientOutcome3 ),
                     PatientTreatmentID = as.integer( vTreatmentID ),
                     Covariate1 = as.double( vCovariate1 ),
                     Covariate2 = as.double( vCovariate2 ),
                     Response   = as.double( rep( 0, NumSub ) ),
                     ErrorCode  = as.integer( nError ) )

    return( lReturn )
}
