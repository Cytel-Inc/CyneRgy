######################################################################################################################## .
#' @name SimulatePatientOutcomePercentAtZeroBetaDist
#' @title Simulate patient outcomes from a normal distribution with a percent of patients having an outcome of 0
#'   where the probability of a 0 is drawn from a Beta distribution.
#' @description The function assumes that the probability a patient has a zero response is random and follows a
#'   Beta( a, b ) distribution. Each distribution must provide 2 parameters for the Beta distribution and the
#'   probability of 0 outcome is selected from the corresponding Beta distribution. The probability of 0 outcome on
#'   the control treatment is sampled from a Beta( UserParam$dCtrlBetaParam1, UserParam$dCtrlBetaParam2 )
#'   distribution. The probability of 0 outcome on the experimental treatment is sampled from a Beta(
#'   UserParam$dExpBetaParam1, UserParam$dExpBetaParam2 ) distribution. The intent of this option is to incorporate
#'   the variability in the unknown, probability of no response, quantity.
#' @author J. Kyle Wathen
#' @param NumSub Integer number of subjects in the trial.
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#' @param Mean Numeric vector of mean responses by arm, with the control arm first, followed by experimental arms
#'   in TreatmentID order.
#' @param StdDev Numeric vector of response standard deviations by arm, with the control arm first, followed by
#'   experimental arms in TreatmentID order.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' When UserParam is NULL, no subjects are forced to have zero outcomes. If supplied, the list must
#'   contain the following named elements:
#' \describe{
#'   \item{UserParam$dCtrlBetaParam1}{Positive numeric shape1 (alpha) parameter for the Beta prior on the
#'     probability of a zero outcome on the control arm.}
#'   \item{UserParam$dCtrlBetaParam2}{Positive numeric shape2 (beta) parameter for the Beta prior on the
#'     probability of a zero outcome on the control arm.}
#'   \item{UserParam$dExpBetaParam1}{Positive numeric shape1 (alpha) parameter for the Beta prior on the
#'     probability of a zero outcome on the experimental arm.}
#'   \item{UserParam$dExpBetaParam2}{Positive numeric shape2 (beta) parameter for the Beta prior on the
#'     probability of a zero outcome on the experimental arm.}
#' }
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details This is a fill-in-the-blank exercise. Replace the underscore placeholders before sourcing or running
#'   the function.
######################################################################################################################## .

SimulatePatientOutcomePercentAtZeroBetaDist <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, ________________________ ) {
    # If the user did not specify the user parameters, but still called this function then the probability
    # of a 0 outcome is 0 for both treatments
    if ( is.null( ___________ ) ) {
        vProbabilityOfZeroOutcome <- c( 0, 0 )
    } else {
        # Simulate the probability of a 0 response from the respective Beta distributions
        dProbabilityOfZeroOutcomeCtrl <- _______( 1, UserParam$dCtrlBetaParam1, UserParam$dCtrlBetaParam2 )
        dProbabilityOfZeroOutcomeExp <- _______( 1, UserParam$dExpBetaParam1, UserParam$dExpBetaParam2 )

        # Create the vProbabilityOfZeroOutcome that is needed below when the patient outcome is simulated
        vProbabilityOfZeroOutcome <- c( dProbabilityOfZeroOutcomeCtrl, dProbabilityOfZeroOutcomeExp )
    }

    nErrorCode <- 0 # No errors occurred
    vPatientOutcome <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    for ( nPatIndx in 1:_________ ) {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index

        # Need to check the probability of a 0 outcome to make sure it is in the range (0, 1) and if not simulate the outcome accordingly
        if ( vProbabilityOfZeroOutcome[ nTreatmentID ] > 0 & vProbabilityOfZeroOutcome[ nTreatmentID ] < 1 ) { # Probability is valid, so need to simulate if the patient is a 0 response
            nResponseIsZero <- stats::rbinom( 1, 1, vProbabilityOfZeroOutcome[ nTreatmentID ] )
        } else if ( vProbabilityOfZeroOutcome[ nTreatmentID ] <= 0 ) { # If Probability of a 0  <= 0
            nResponseIsZero <- 0
        } else { # if the probability of a 0 >= 1 --> Don't need to simulate from the normal distribution as all patients in the treatment are a 0
            nResponseIsZero <- 1
        }

        if ( nResponseIsZero == 0 ) { # The patient responded, so we need to simulate their outcome from a normal distribution with the specified mean and standard deviation
            vPatientOutcome[ nPatIndx ] <- stats::rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ] )
        }
    }

    if ( any( is.na( vPatientOutcome ) ) ) {
        nErrorCode <- -100
    }

    return( list( Response = as.double( ____________ ), ErrorCode = as.integer( _________________ ) ) )
}
