######################################################################################################################## .
#' @name SimulateMultipleOutcomes
#'
#' @title Simulate Multiple Independent Outcomes
#'
#' @description This function simulates three independent normally distributed outcomes for a given number of
#'   subjects, based on their treatment assignment. Each outcome has a treatment-specific mean and a fixed standard
#'   deviation. Covariates are not used in this version. Note: this code can be extended to any number of
#'   endpoints.
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
#' Contains treatment-specific means for each outcome:
#'        \describe{
#'          \item{MeanOutcome1Ctrl}{Mean of outcome 1 for control group}
#'          \item{MeanOutcome1Trt}{Mean of outcome 1 for treatment group}
#'          \item{MeanOutcome2Ctrl}{Mean of outcome 2 for control group}
#'          \item{MeanOutcome2Trt}{Mean of outcome 2 for treatment group}
#'          \item{MeanOutcome3Ctrl}{Mean of outcome 3 for control group}
#'          \item{MeanOutcome3Trt}{Mean of outcome 3 for treatment group}
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
#' }
#'
#' @details Usage of Mean in this example: Numeric. Not used directly in this function.
#'
#' Usage of StdDev in this example: Numeric. Not used directly in this function.
#'
#' Example-specific error codes: Integer. 0 if successful, 1 if `UserParam` is NULL
######################################################################################################################## .

SimulateMultipleOutcomes <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL ) {
    # Initialize the return variables that will contain results for 3 normal endpoints
    vPatientOutcome1 <- rep( 0, NumSub )
    vPatientOutcome2 <- rep( 0, NumSub )
    vPatientOutcome3 <- rep( 0, NumSub )

    # Validate custom variable input and set defaults
    nErrorCode <- 0

    if ( is.null( UserParam ) ) {
        nErrorCode <- 1
    }

    # Extract means for each outcome and arm from UserParam
    vMeansOutcome1 <- c( UserParam$MeanOutcome1Ctrl, UserParam$MeanOutcome1Trt )
    vMeansOutcome2 <- c( UserParam$MeanOutcome2Ctrl, UserParam$MeanOutcome2Trt )
    vMeansOutcome3 <- c( UserParam$MeanOutcome3Ctrl, UserParam$MeanOutcome3Trt )

    # Simulate the patient independent outcome data
    for ( nPatientIndex in 1:NumSub ) {
        # Convert 0(Ctrl) -> 1 to 1 (Trt) -> 2 for indexing
        nTreatmentID <- TreatmentID[ nPatientIndex ] + 1

        vPatientOutcome1[ nPatientIndex ] <- stats::rnorm( 1, mean = vMeansOutcome1[ nTreatmentID ], sd = 1 )
        vPatientOutcome2[ nPatientIndex ] <- stats::rnorm( 1, mean = vMeansOutcome2[ nTreatmentID ], sd = 1 )
        vPatientOutcome3[ nPatientIndex ] <- stats::rnorm( 1, mean = vMeansOutcome3[ nTreatmentID ], sd = 1 )
    }

    # Return the simulated outcomes and error code
    lReturn <- list(
        PatientOutcome1 = as.double( vPatientOutcome1 ),
        PatientOutcome2 = as.double( vPatientOutcome2 ),
        PatientOutcome3 = as.double( vPatientOutcome3 ),
        Response = as.double( rep( 0, NumSub ) ),
        ErrorCode = as.integer( nErrorCode )
    )

    return( lReturn )
}
