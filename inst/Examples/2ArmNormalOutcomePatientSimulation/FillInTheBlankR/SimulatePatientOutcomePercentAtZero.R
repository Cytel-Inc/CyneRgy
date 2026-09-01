######################################################################################################################## .
#' @name SimulatePatientOutcomePercentAtZero
#' @title Simulate patient outcomes from a normal distribution with a percent of patients having an outcome of 0.
#' @author J. Kyle Wathen
#' @param NumSub Integer number of subjects in the trial.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param Mean Numeric vector of arm-specific outcome means.
#' @param StdDev Numeric vector of arm-specific outcome standard deviations.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'   \item{UserParam$dProbOfZeroOutcomeCtrl}{Probability that a control patient has an outcome of 0.}
#'   \item{UserParam$dProbOfZeroOutcomeExp}{Probability that an experimental-treatment patient has an outcome of 0.}
#' }
#' @description
#' In this example, the continuous outcome is a patient's change from baseline.   For this function, 20\% of patients are believed to have no change due to treatment.
#' As such, this function simulations patient outcome where, on average, 20\% will have a value of 0 for the outcome and 80\%, on average, will have their value
#' simulated from a normal distribution with the mean and standard deviation as sent from East Horizon.
#' @return After the blanks are completed, a named list containing numeric vector `Response` and integer `ErrorCode`.
######################################################################################################################## .
SimulatePatientOutcomePercentAtZero <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL )
{
    # If the user did not specify the user parameters, but still called this function then the probability
    # of a 0 outcome is 0 for both treatments
    if( is.null( UserParam ) )
    {
        UserParam <- list( _____________________ = 0, ___________________ = 0 )
    }

    # Create the vector of probabilities of a 0 outcome for each treatment to be used in the for loop below
    vProbabilityOfZeroOutcome <- c( UserParam$dProbOfZeroOutcomeCtrl, UserParam$dProbOfZeroOutcomeExp )    # For this example, 20% of patients do not respond to treatment and thus have no change from baseline.

    nError           <- 0 # No errors occurred
    vPatientOutcome  <- rep( 0, _________ ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    # Loop over the patients and simulate the outcome according to the treatment they
    for( nPatIndx in 1:________ )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index

        # Need to check the probability of a 0 outcome to make sure it is in the range (0, 1) and if not simulate the outcome accordingly
        if( vProbabilityOfZeroOutcome[ nTreatmentID ] > 0 & vProbabilityOfZeroOutcome[ nTreatmentID ] < 1 ) # Probability is valid, so need to simulate if the patient is a 0 response
            nResponseIsZero <- ________( 1, 1, vProbabilityOfZeroOutcome[ nTreatmentID ] )
        else if( vProbabilityOfZeroOutcome[ nTreatmentID ] <= 0 )   # If Probability of a 0  <= 0
            nResponseIsZero <- 0
        else                        # if the probability of a 0 >= 1 --> Don't need to simulate from the normal distribution as all patients in the treatment are a 0
            nResponseIsZero <- 1

        if( ___________________ == 0 ) # The patient responded, so we need to simulate their outcome from a normal distribution with the specified mean and standard deviation
            vPatientOutcome[ nPatIndx ] <- rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ] )
    }

    if( any( is.na( vPatientOutcome ) == TRUE ) )
        nError <- -100

    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ) )
}
