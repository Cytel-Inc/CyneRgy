######################################################################################################################## .
#' @param SimulatePatientOutcomePercentAtZeroBetaDist
#' @title Simulate patient outcomes from a normal distribution with a percent of patients having an outcome of 0 where the probability of a 0 is drawn from a Beta distribution.
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2, length( TreatmentID ) = NumSub
#' @param Mean A vector of length = 2 with the means of the two treatments.
#' @param StdDev A vector of length = 2 with the standard deviations of each treatment
#' @param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL resulting in ignoring the percent of patients at 0.
#' If UserParam is supplied, the list must contain the following named elements:
#'  UserParam$dCtrlBetaParam1 -  First parameter in the Beta distribution for the control (ctrl) treatment.
#'  UserParam$dCtrlBetaParam2 - Second parameter in the Beta distribution for the control (ctrl) treatment.
#'  UserParam$dExpBetaParam1 - First parameter in the Beta distribution for the experimental (exp) treatment.
#'  UserParam$dExpBetaParam2 - Second parameter in the Beta distribution for the experimental (exp) treatment.
#' @description
#' The function assumes that the probability a patient has a zero response is random and follows a Beta( a, b ) distribution.
#' Each distribution must provide 2 parameters for the Beta distribution and the probability of 0 outcome is selected from the corresponding Beta distribution.
#' The probability of 0 outcome on the control treatment is sampled from a Beta( UserParam$dCtrlBetaParam1, UserParam$dCtrlBetaParam2 ) distribution.
#' The probability of 0 outcome on the experimental treatment is sampled from a Beta( UserParam$dExpBetaParam1, UserParam$dExpBetaParam2 ) distribution.
#' The intent of this option is to incorporate the variability in the unknown, probability of no response, quantity.  
#' ######################################################################################################################## .
#' 
SimulatePatientOutcomePercentAtZeroBetaDist <- function( NumSub, TreatmentID, Mean, StdDev, UserParam = NULL )
{
    # Create a fatal error when user parameters are missing to avoid misleading results
    vRequiredParams <- c("dCtrlBetaParam1", "dCtrlBetaParam2", "dExpBetaParam1", "dExpBetaParam2")
    vMissingParams <- vRequiredParams[!vRequiredParams %in% names(UserParam)]
    
    if( is.null( UserParam ) || length( vMissingParams ) > 0 )
    {
        return(list(Response  = as.double(0), 
                    ErrorCode = as.integer(-1)))
    }
    
    nError           <- 0 # No error occurred 
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East or East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index
        
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
    
    if(  any( is.na( vPatientOutcome ) == TRUE ) )
        nError <- -100
    
    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ))
}