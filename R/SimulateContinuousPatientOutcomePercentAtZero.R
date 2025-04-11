######################################################################################################################## .
#' @name SimulateContinuousPatientOutcomePercentAtZero
#' @title Simulate Continuous Patient Outcomes with Proportion at Zero
#' 
#' @description Simulates patient outcomes from a normal distribution, with a specified percentage of patients having an outcome of 0. In this example, the continuous outcome represents a patient's change from baseline. This function generates patient outcomes such that, on average:
#' - A specified proportion of patients will have a value of 0 for the outcome, as defined by `UserParam`.
#' - The remaining patients will have their values simulated from a normal distribution using the provided mean and standard deviation parameters.
#'
#' @param NumSub The number of subjects to simulate. Must be an integer value.
#' @param TreatmentID A vector of treatment IDs, where:
#' - `0` represents Treatment 1.
#' - `1` represents Treatment 2.
#' The length of `TreatmentID` must equal `NumSub`.
#' @param Mean A numeric vector of length 2 specifying the mean values for the two treatments.
#' @param StdDev A numeric vector of length 2 specifying the standard deviations for each treatment.
#' @param UserParam A list of user-defined parameters. Must contain the following named elements:
#' \describe{
#'   \item{UserParam$dProbOfZeroOutcomeCtrl}{Numeric (0, 1); defines the probability that a patient has an outcome of 0 for the control (Treatment 1).}
#'   \item{UserParam$dProbOfZeroOutcomeExp}{Numeric (0, 1); defines the probability that a patient has an outcome of 0 for the experimental (Treatment 2).}
#' }
#' @return A list containing the following elements:
#' \describe{
#'   \item{Response}{A numeric vector representing the simulated outcomes for each patient.}
#'   \item{ErrorCode}{Optional integer value:
#'                      \describe{
#'                        \item{0}{No error.}
#'                        \item{> 0}{Non-fatal error; current simulation is aborted but subsequent simulations continue.}
#'                        \item{< 0}{Fatal error; no further simulations are attempted.}
#'                      }}
#' }
#' @export
######################################################################################################################## .

SimulateContinuousPatientOutcomePercentAtZero <- function( NumSub, TreatmentID, Mean, StdDev, UserParam = NULL )
{
    # Create a fatal error when user parameters are missing to avoid misleading results
    vRequiredParams <- c( "dProbOfZeroOutcomeCtrl", "dProbOfZeroOutcomeExp" )
    vMissingParams <- vRequiredParams[ !vRequiredParams %in% names( UserParam ) ]
    
    if( is.null( UserParam ) || length( vMissingParams ) > 0 )
    {
        return( list( Response  = as.double( 0 ), 
                      ErrorCode = as.integer( -1 ) ) )
    }
    
    # Create the vector of probabilities of a 0 outcome for each treatment to be used in the for loop below
    vProbabilityOfZeroOutcome <- c( UserParam$dProbOfZeroOutcomeCtrl, UserParam$dProbOfZeroOutcomeExp )    # For this example, 20% of patients do not respond to treatment and thus have no change from baseline.  
    
    nError           <- 0 # No error occurred 
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    
    # Loop over the patients and simulate the outcome according to the treatment they 
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East has the treatments as 0, 1 so need to add 1 to get a vector index
        
        # Need to check the probability of a 0 outcome to make sure it is in the range (0, 1) and if not simulate the outcome accordingly 
        if( vProbabilityOfZeroOutcome[ nTreatmentID ] > 0 & vProbabilityOfZeroOutcome[ nTreatmentID ] < 1 ) # Probability is valid, so need to simulate if the patient is a 0 response
            nResponseIsZero <- rbinom( 1, 1, vProbabilityOfZeroOutcome[ nTreatmentID ] )
        else if( vProbabilityOfZeroOutcome[ nTreatmentID ] <= 0 )   # If Probability of a 0  <= 0
            nResponseIsZero <- 0
        else                        # if the probability of a 0 >= 1 --> Don't need to simulate from the normal distribution as all patients in the treatment are a 0
            nResponseIsZero <- 1
        
        
        if( nResponseIsZero == 0  )  # The patient responded, so we need to simulate their outcome from a normal distribution with the specified mean and standard deviation 
            vPatientOutcome[ nPatIndx ] <- rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ] )
    }
    
    if(  any( is.na( vPatientOutcome ) == TRUE ) )
        nError <- -100
    
    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ) )
}