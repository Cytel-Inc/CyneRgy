######################################################################################################################## .
#' @title Simulate Continuous Patient Outcomes with Probability of Zero from a Beta Distribution
#' 
#' @description Simulates patient outcomes from a normal distribution, with the probability of a zero outcome being random and sampled from a Beta distribution. 
#' The probability of a zero outcome is determined as follows:
#' - For the control treatment, it is sampled from a \eqn{Beta(UserParam$dCtrlBetaParam1, UserParam$dCtrlBetaParam2)} distribution.
#' - For the experimental treatment, it is sampled from a \eqn{Beta(UserParam$dExpBetaParam1, UserParam$dExpBetaParam2)} distribution.
#' This approach incorporates variability in the unknown probability of no response.
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
#'   \item{UserParam$dCtrlBetaParam1}{Numeric; first parameter in the Beta distribution for the control (Treatment 1).}
#'   \item{UserParam$dCtrlBetaParam2}{Numeric; second parameter in the Beta distribution for the control (Treatment 1).}
#'   \item{UserParam$dExpBetaParam1}{Numeric; first parameter in the Beta distribution for the experimental (Treatment 2).}
#'   \item{UserParam$dExpBetaParam2}{Numeric; second parameter in the Beta distribution for the experimental (Treatment 2).}
#' }
#'
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
######################################################################################################################## .

SimulatePatientOutcomePercentAtZeroBetaDist <- function(NumSub, TreatmentID, Mean, StdDev, UserParam = NULL)
{
    # Note: It can be helpful to save to the parameters that East sent.
    # The next two lines show how you could save the UserParam variable to an Rds file
    # setwd( "[ENTERED THE DESIRED LOCATION TO SAVE THE FILE]" )
    # saveRDS(UserParam, "UserParam.Rds")
    
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
        dProbabilityofZeroOutcomeExp  <- rbeta( 1, UserParam$dExpBetaParam1,  UserParam$dExpBetaParam2 )
        
        #Create the vProbabilityOfZeroOutcome that is needed below when the patient outcome is simulated 
        vProbabilityOfZeroOutcome     <- c( dProbabilityofZeroOutcomeCtrl, dProbabilityofZeroOutcomeExp )    
    }
    
    nError           <- 0 # East code for no errors occurred 
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    
    
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
            vPatientOutcome[ nPatIndx ] <- rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ])
    }
    
    if(  any( is.na( vPatientOutcome ) == TRUE) )
        nError <- -100
    
    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ))
}