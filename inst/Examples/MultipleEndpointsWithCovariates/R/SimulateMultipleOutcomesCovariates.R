######################################################################################################################## .
#' @name SimulateMultipleOutcomesCovariates
#'
#' @title Simulate Multiple Independent Outcomes Using Covariates
#'
#' @description This function simulates three independent normally distributed outcomes for a given number of
#'   subjects,
#' based on their treatment assignment. Each outcome has a treatment-specific mean and a fixed standard deviation.
#' Two covariates are used in this version:
#'        \itemize{
#'          \item Covariate 1: binary (e.g., diabetic)
#'          \item Covariate 2: binary (e.g., smoker)
#'        }
#' Covariate effects are incorporated linearly into the outcome generation.
#' Note: this function can be extended to simulate any number of endpoints and covariates.
#'
#' @author Julija Saltane
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param Mean Numeric vector of mean responses by arm, with the control arm first, followed by experimental arms
#'   in TreatmentID order.
#'
#' @param StdDev Numeric vector of response standard deviations by arm, with the control arm first, followed by
#'   experimental arms in TreatmentID order.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' Contains treatment-specific means and covariate parameters:
#'        \describe{
#'          \item{MeanOutcome1Ctrl}{Mean of outcome 1 for control group}
#'          \item{MeanOutcome1Trt}{Mean of outcome 1 for treatment group}
#'          \item{MeanOutcome2Ctrl}{Mean of outcome 2 for control group}
#'          \item{MeanOutcome2Trt}{Mean of outcome 2 for treatment group}
#'          \item{MeanOutcome3Ctrl}{Mean of outcome 3 for control group}
#'          \item{MeanOutcome3Trt}{Mean of outcome 3 for treatment group}
#'          \item{Beta1}{Effect size of covariate 1}
#'          \item{Beta2}{Effect size of covariate 2}
#'          \item{Cov1Prob}{Probability of covariate 1 being 1}
#'          \item{Cov2Prob}{Probability of covariate 2 being 1}
#'        }
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' Example-specific additional output elements:
#' \describe{
#'   \item{PatientOutcome1}{Numeric vector of simulated values for continuous outcome 1}
#'   \item{PatientOutcome2}{Numeric vector of simulated values for continuous outcome 2}
#'   \item{PatientOutcome3}{Numeric vector of simulated values for continuous outcome 3}
#'   \item{Covariate1}{Binary vector of simulated values for covariate 1}
#'   \item{Covariate2}{Binary vector of simulated values for covariate 2}
#' }
#'
#' @details Usage of Mean in this example: Numeric. Not used directly in this function.
#'
#' Usage of StdDev in this example: Numeric. Not used directly in this function.
#'
#' Example-specific error codes: Integer. 0 if successful, 1 if `UserParam` is NULL
######################################################################################################################## .

SimulateMultipleOutcomesCovariates <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL ) {
    # Initialize the return variables that will contain results for 3 normal endpoints
    vPatientOutcome1 <- rep( 0, NumSub )
    vPatientOutcome2 <- rep( 0, NumSub )
    vPatientOutcome3 <- rep( 0, NumSub )

    # Validate custom variable input and set defaults
    nErrorCode <- 0

    if ( is.null( UserParam ) ) {
        nErrorCode <- 1
    }

    # Extract means for each outcome and group
    vMeansOutcome1 <- c( UserParam$MeanOutcome1Ctrl, UserParam$MeanOutcome1Trt )
    vMeansOutcome2 <- c( UserParam$MeanOutcome2Ctrl, UserParam$MeanOutcome2Trt )
    vMeansOutcome3 <- c( UserParam$MeanOutcome3Ctrl, UserParam$MeanOutcome3Trt )

    # Extract covariate effects
    dBeta1 <- UserParam$Beta1
    dBeta2 <- UserParam$Beta2

    # Simulate the effect of covariates
    vCovariate1 <- stats::rbinom( NumSub, size = 1, prob = UserParam$Cov1Prob )
    vCovariate2 <- stats::rbinom( NumSub, size = 1, prob = UserParam$Cov2Prob )

    vCovariateEffect <- dBeta1 * vCovariate1 + dBeta2 * vCovariate2

    # Simulate the patient independent outcome data
    for ( nPatientIndex in 1:NumSub ) {
        # Convert 0(Ctrl) -> 1 to 1 (Trt) -> 2 for indexing
        nTreatmentID <- TreatmentID[ nPatientIndex ] + 1

        vPatientOutcome1[ nPatientIndex ] <- stats::rnorm( 1, mean = vMeansOutcome1[ nTreatmentID ] + vCovariateEffect[ nPatientIndex ], sd = 1 )
        vPatientOutcome2[ nPatientIndex ] <- stats::rnorm( 1, mean = vMeansOutcome2[ nTreatmentID ] + vCovariateEffect[ nPatientIndex ], sd = 1 )
        vPatientOutcome3[ nPatientIndex ] <- stats::rnorm( 1, mean = vMeansOutcome3[ nTreatmentID ] + vCovariateEffect[ nPatientIndex ], sd = 1 )
    }

    # Return the simulated outcomes and error code
    lReturn <- list(
        PatientOutcome1 = as.double( vPatientOutcome1 ),
        PatientOutcome2 = as.double( vPatientOutcome2 ),
        PatientOutcome3 = as.double( vPatientOutcome3 ),
        Covariate1 = as.double( vCovariate1 ),
        Covariate2 = as.double( vCovariate2 ),
        Response = as.double( rep( 0, NumSub ) ),
        ErrorCode = as.integer( nErrorCode )
    )

    return( lReturn )
}
