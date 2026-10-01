######################################################################################################################## .
#' @name SimulatePatientOutcomeMultiArmPercentAtZero.Binary
#'
#' @title Simulate patient binary outcomes from a binary distribution with a specified percent of
#'   treatment-resistant patients for multi-arm trials
#'
#' @description In this example, the binary outcome is a patient's response to treatments (0 non-response or 1
#'   response). For this function, a percent of patients are believed to be treatment resistant, meaning the
#'   patient will not respond to any treatment and their outcome is always a 0.
#'
#' The steps to simulating patient data in this example follows a two-step procedure. Step 1: Determine if the
#'   patient is treatment resistant by simulating a binary variable with the probability of success defined by
#'   UserParam$dProbOfTreatmentResistantCtrl, UserParam$dProbOfTreatmentResistantExp1 or
#'   UserParam$dProbOfTreatmentResistantExp2 Step 2: If the patient is determined to be treatment resistant in Step
#'   1, their outcome is set to 0. Otherwise, their outcome is simulated from a binomial distribution using the
#'   response probabilities provided in PropResp.
#'
#' @author Gabriel Potvin and Anoop Singh Rawat
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArm Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param PropResp Numeric vector of response probabilities by arm, with the control arm first, followed by
#'   experimental arms in TreatmentID order. Each probability is between 0 and 1.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'   \item{UserParam$dProbOfTreatmentResistantCtrl}{Numeric probability in [0, 1] that a subject is treatment
#'     resistant on the control arm.}
#'   \item{UserParam$dProbOfTreatmentResistantExp1}{Numeric probability in [0, 1] that a subject is treatment
#'     resistant on experimental arm 1 and therefore has a response of 0.}
#'   \item{UserParam$dProbOfTreatmentResistantExp2}{Numeric probability in [0, 1] that a subject is treatment
#'     resistant on experimental arm 2 and therefore has a response of 0.}
#' }
#'
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated binary subject responses, coded 0 = non-response and 1 = response,
#'     with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' @details For vaccine-efficacy binary designs, the engine additionally supplies FollowUpDur (numeric follow-up
#'   duration, used as the assessment time for PropResp) and OneMinusROP (numeric value of 1 minus the
#'   treatment-to-control ratio of proportions). These standard binary examples do not use those inputs; extend the
#'   signature and generation logic for a vaccine-efficacy design. Standard binary responses are coded 0 =
#'   non-response and 1 = response.
#'
#' This example supports control and two experimental arms. With UserParam = NULL, the probability of
#'   a forced zero outcome is 0 on all arms. Extend the named user parameters and probability vector before
#'   using additional experimental arms.

######################################################################################################################## .

SimulatePatientOutcomeMultiArmPercentAtZero.Binary <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL ) {
    # If the user did not specify the user parameters, but still called this function then the probability
    # of treatment resistant is 0 for both treatments
    if ( is.null( UserParam ) ) {
        UserParam <- list(
            dProbOfTreatmentResistantCtrl = 0,
            dProbOfTreatmentResistantExp1 = 0,
            dProbOfTreatmentResistantExp2 = 0
        )
    }

    # Create the vector of probabilities of a 0 outcome for each treatment to be used in the for loop below
    vProbabilityOfTreatmentResistant <- c(
        UserParam$dProbOfTreatmentResistantCtrl,
        UserParam$dProbOfTreatmentResistantExp1,
        UserParam$dProbOfTreatmentResistantExp2
    ) # By default, 0% of patients are treatment resistant

    nErrorCode <- 0 # No errors occurred
    vPatientOutcome <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    # Loop over the patients and simulate the outcome according to the treatment they
    for ( nPatIndx in 1:NumSub ) {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # Convert to 1-based index
        dProbResist <- vProbabilityOfTreatmentResistant[ nTreatmentID ]

        # Determine if patient is treatment resistant
        if ( dProbResist > 0 & dProbResist < 1 ) {
            nTreatmentResistant <- stats::rbinom( 1, 1, dProbResist )
        } else if ( dProbResist <= 0 ) {
            nTreatmentResistant <- 0
        } else {
            nTreatmentResistant <- 1
        }

        # If nTreatmentResistant == 1, the patient outcome is a 0 and we don't need to simulate it.

        if ( nTreatmentResistant == 0 ) { # The patient is not resistant, so we need to simulate their outcome from a binary distribution
            vPatientOutcome[ nPatIndx ] <- stats::rbinom( 1, 1, PropResp[ nTreatmentID ] )
        }
    }

    if ( any( is.na( vPatientOutcome ) == TRUE ) ) {
        nErrorCode <- -100
    }

    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nErrorCode ) ) )
}
