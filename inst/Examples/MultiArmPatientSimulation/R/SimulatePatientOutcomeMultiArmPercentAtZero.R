######################################################################################################################## .
#' @name SimulatePatientOutcomeMultiArmPercentAtZero
#'
#' @title Simulate patient continuous outcomes from a normal distribution with a percent of patients having an
#'   outcome of 0 for multi-arm trials
#'
#' @description Generate normal subject responses with an arm-specific probability of a zero outcome.
#'   Subjects selected to have a zero outcome are assigned 0; otherwise their responses are drawn from the
#'   normal distribution specified by Mean and StdDev. With UserParam = NULL, no outcomes are forced to zero.
#'
#' @author Gabriel Potvin and Anoop Singh Rawat
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArms Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
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
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'   \item{UserParam$dProbOfZeroOutcomeCtrl}{Numeric probability in [0, 1] that a subject has a zero outcome
#'     on the control arm.}
#'   \item{UserParam$dProbOfZeroOutcomeExp1}{Numeric probability in [0, 1] that a subject has a zero outcome
#'     on experimental arm 1.}
#'   \item{UserParam$dProbOfZeroOutcomeExp2}{Numeric probability in [0, 1] that a subject has a zero outcome
#'     on experimental arm 2.}
#' }
#'
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' @details This example supports control and two experimental arms. With UserParam = NULL, the probability of
#'   a forced zero outcome is 0 on all arms. Extend the named user parameters and probability vector before
#'   using additional experimental arms.

######################################################################################################################## .

SimulatePatientOutcomeMultiArmPercentAtZero <- function( NumSub, NumArms, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL ) {
    # If the user did not specify the user parameters, but still called this function then the probability
    # of a 0 outcome is 0 for both treatments
    if ( is.null( UserParam ) ) {
        UserParam <- list(
            dProbOfZeroOutcomeCtrl = 0,
            dProbOfZeroOutcomeExp1 = 0,
            dProbOfZeroOutcomeExp2 = 0
        )
    }

    # Create the vector of probabilities of a 0 outcome for each treatment to be used in the for loop below
    vProbabilityOfZeroOutcome <- c(
        UserParam$dProbOfZeroOutcomeCtrl,
        UserParam$dProbOfZeroOutcomeExp1,
        UserParam$dProbOfZeroOutcomeExp2
    )

    nErrorCode <- 0 # No errors occurred
    vPatientOutcome <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    # Loop over the patients and simulate the outcome according to the treatment they
    for ( nPatIndx in 1:NumSub ) {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # Convert to 1-based index
        dProbZeroOutcome <- vProbabilityOfZeroOutcome[ nTreatmentID ]

        # Need to check the probability of a 0 outcome to make sure it is in the range (0, 1) and if not simulate the outcome accordingly
        if ( dProbZeroOutcome > 0 & dProbZeroOutcome < 1 ) { # Probability is valid, so need to simulate if the patient is a 0 response
            nResponseIsZero <- stats::rbinom( 1, 1, dProbZeroOutcome )
        } else if ( dProbZeroOutcome <= 0 ) { # If Probability of a 0  <= 0
            nResponseIsZero <- 0
        } else { # if the probability of a 0 >= 1 --> Don't need to simulate from the normal distribution as all patients in the treatment are a 0
            nResponseIsZero <- 1
        }

        if ( nResponseIsZero == 0 ) { # The patient responded, so we need to simulate their outcome from a normal distribution with the specified mean and standard deviation
            vPatientOutcome[ nPatIndx ] <- stats::rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ] )
        }
    }

    if ( any( is.na( vPatientOutcome ) == TRUE ) ) {
        nErrorCode <- -100
    }

    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nErrorCode ) ) )
}
