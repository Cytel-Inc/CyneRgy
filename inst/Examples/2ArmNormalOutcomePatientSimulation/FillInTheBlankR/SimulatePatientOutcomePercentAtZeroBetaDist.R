######################################################################################################################## .
#' @name SimulatePatientOutcomePercentAtZeroBetaDist
#' @title Simulate patient outcomes from a normal distribution with a percent of patients having an outcome of 0 where the probability of a 0 is drawn from a Beta distribution.
#' @author J. Kyle Wathen
#' @param NumSub Integer number of subjects in the trial.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param Mean Numeric vector of arm-specific outcome means.
#' @param StdDev Numeric vector of arm-specific outcome standard deviations.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'   \item{UserParam$dCtrlBetaParam1}{First Beta-distribution parameter for the control probability of an outcome of 0.}
#'   \item{UserParam$dCtrlBetaParam2}{Second Beta-distribution parameter for the control probability of an outcome of 0.}
#'   \item{UserParam$dExpBetaParam1}{First Beta-distribution parameter for the experimental probability of an outcome of 0.}
#'   \item{UserParam$dExpBetaParam2}{Second Beta-distribution parameter for the experimental probability of an outcome of 0.}
#' }
#' @description
#' The function assumes that the probability a patient has a zero response is random and follows a Beta( a, b ) distribution.
#' Each distribution must provide 2 parameters for the beta distribution and the probability of 0 outcome is selected from the corresponding Beta distribution.
#' The probability of 0 outcome on the control treatment is sampled from a Beta( UserParam$dCtrlBetaParam1, UserParam$dCtrlBetaParam2 ) distribution.
#' The probability of 0 outcome on the experimental treatment is sampled from a Beta( UserParam$dExpBetaParam1, UserParam$dExpBetaParam2 ) distribution.
#' The intent of this option is to incorporate the variability in the unknown, probability of no response, quantity.
#' @return After the blanks are completed, a named list containing numeric vector `Response` and integer `ErrorCode`.
######################################################################################################################## .
SimulatePatientOutcomePercentAtZeroBetaDist <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, ________________________ )
{
    # If the user did not specify the user parameters, but still called this function then the probability
    # of a 0 outcome is 0 for both treatments
    if( is.null( ___________ ) )
    {
        vProbabilityOfZeroOutcome <- c( 0, 0 )
    }
    else
    {
        # Simulate the probability of a 0 response from the respective Beta distributions
        dProbabilityofZeroOutcomeCtrl <- _______( 1, UserParam$dCtrlBetaParam1, UserParam$dCtrlBetaParam2 )
        dProbabilityofZeroOutcomeExp  <- _______( 1, UserParam$dExpBetaParam1, UserParam$dExpBetaParam2 )

        # Create the vProbabilityOfZeroOutcome that is needed below when the patient outcome is simulated
        vProbabilityOfZeroOutcome     <- c( dProbabilityofZeroOutcomeCtrl, dProbabilityofZeroOutcomeExp )
    }

    nError           <- 0 # No errors occurred
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    for( nPatIndx in 1:_________ )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index

        # Need to check the probability of a 0 outcome to make sure it is in the range (0, 1) and if not simulate the outcome accordingly
        if( vProbabilityOfZeroOutcome[ nTreatmentID ] > 0 & vProbabilityOfZeroOutcome[ nTreatmentID ] < 1 ) # Probability is valid, so need to simulate if the patient is a 0 response
            nResponseIsZero <- rbinom( 1, 1, vProbabilityOfZeroOutcome[ nTreatmentID ] )
        else if( vProbabilityOfZeroOutcome[ nTreatmentID ] <= 0 )   # If Probability of a 0  <= 0
            nResponseIsZero <- 0
        else                        # if the probability of a 0 >= 1 --> Don't need to simulate from the normal distribution as all patients in the treatment are a 0
            nResponseIsZero <- 1

        if( nResponseIsZero == 0 ) # The patient responded, so we need to simulate their outcome from a normal distribution with the specified mean and standard deviation
            vPatientOutcome[ nPatIndx ] <- rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ] )
    }

    if( any( is.na( vPatientOutcome ) == TRUE ) )
        nError <- -100

    return( list( Response = as.double( ____________ ), ErrorCode = as.integer( _________________ ) ) )
}
