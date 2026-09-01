######################################################################################################################## .
#' @name SimulatePatientOutcomePercentAtZeroBetaDist.Binary
#' @title Simulate patient outcomes from a binary distribution with a percent of patients having an outcome of 0 where the probability of a 0 is drawn from a Beta distribution.
#' @author J. Kyle Wathen
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param PropResp Numeric vector of length `NumArm`, containing response probabilities for control followed by each experimental arm.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'   \item{UserParam$dCtrlBetaParam1}{First parameter in the Beta distribution for the control treatment.}
#'   \item{UserParam$dCtrlBetaParam2}{Second parameter in the Beta distribution for the control treatment.}
#'   \item{UserParam$dExpBetaParam1}{First parameter in the Beta distribution for the experimental treatment.}
#'   \item{UserParam$dExpBetaParam2}{Second parameter in the Beta distribution for the experimental treatment.}
#' }
#' @description
#' The function assumes that the probability a patient has a zero response is random and follows a Beta( a, b ) distribution.
#' Each distribution must provide 2 parameters for the beta distribution and the probability of 0 outcome is selected from the corresponding Beta distribution.
#' The probability of 0 outcome on the control treatment is sampled from a Beta( UserParam$dCtrlBetaParam1, UserParam$dCtrlBetaParam2 ) distribution.
#' The probability of 0 outcome on the experimental treatment is sampled from a Beta( UserParam$dExpBetaParam1, UserParam$dExpBetaParam2 ) distribution.
#' The intent of this option is to incorporate the variability in the unknown, probability of no response, quantity.
#' @return A named list containing `Response`, a binary vector of length `NumSub`, and integer `ErrorCode`.
######################################################################################################################## .
SimulatePatientOutcomePercentAtZeroBetaDist.Binary <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL )
{
    # If the user did not specify the user parameters, but still called this function then the probability
    # of a 0 outcome is 0 for both treatments
    if( is.null( UserParam ) )
    {
        vProbabilityOfZeroOutcome <- c( 0, 0 )
    }
    else
    {
        # Simulate the probability of a 0 response from the respective Beta distributions
        dProbabilityofZeroOutcomeCtrl <- rbeta( 1, UserParam$dCtrlBetaParam1, UserParam$dCtrlBetaParam2 )
        dProbabilityofZeroOutcomeExp  <- rbeta( 1, UserParam$dExpBetaParam1, UserParam$dExpBetaParam2 )

        #Create the vProbabilityOfZeroOutcome that is needed below when the patient outcome is simulated
        vProbabilityOfZeroOutcome     <- c( dProbabilityofZeroOutcomeCtrl, dProbabilityofZeroOutcomeExp )
    }

    nError           <- 0 # No errors occurred
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index

        # Need to check the probability of a 0 outcome to make sure it is in the range (0, 1) and if not simulate the outcome accordingly
        if( vProbabilityOfZeroOutcome[ nTreatmentID ] > 0 & vProbabilityOfZeroOutcome[ nTreatmentID ] < 1 ) # Probability is valid, so need to simulate if the patient is a 0 response
            nResponseIsZero <- rbinom( 1, 1, vProbabilityOfZeroOutcome[ nTreatmentID ] )
        else if( vProbabilityOfZeroOutcome[ nTreatmentID ] <= 0 )   # If Probability of a 0  <= 0
            nResponseIsZero <- 0
        else                        # if the probability of a 0 >= 1 --> Don't need to simulate from the binary distribution as all patients in the treatment are a 0
            nResponseIsZero <- 1

        # If nResponseIsZero == 1 then the patient outcome is a a 0 and we don't need to simulate it

        if( nResponseIsZero == 0 )  # The patient responded, so we need to simulate their outcome from a binary distribution with the specified response proportion
            vPatientOutcome[ nPatIndx ] <- rbinom( 1, 1, PropResp[ nTreatmentID ] )
    }

    if( any( is.na( vPatientOutcome ) ) )
        nError <- -100

    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ) )

}
