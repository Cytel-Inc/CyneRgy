#' @param SimulatePatientOutcomePercentAtZero
#' @title Simulate patient outcomes from a normal distribution with a percent of patients having an outcome of 0. 
#' @param NumSub Integer. The number of subjects that need to be simulated.
#' @param NumArm Integer. The number of arms in the trial, including the placebo/control.
#' @param TreatmentID A vector specifying the arm index for each subject. The index for the placebo/control arm is 0.
#' @param Mean A vector with means for all the arms.
#' @param StdDev A vector with the standard deviations of each arm.
#' @param UserParam A list of user defined parameters in East Horizon. The default must be NULL resulting in ignoring the percent of patients at 0.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'    \item{UserParam$dProbOfZeroOutcomeCtrl}{A value in (0, 1) that defines the probability a patient will have an outcome of 0 on the control arm.}
#'    \item{UserParam$dProbOfZeroOutcomeExp1}{A value in (0, 1) that defines the probability a patient will have an outcome of 0 on the experimental arm 1.}
#'    \item{UserParam$dProbOfZeroOutcomeExp2}{A value in (0, 1) that defines the probability a patient will have an outcome of 0 on the experimental arm 2.}
#' }
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed. 
#'             \describe{
#'             \item{Response}{Required numeric value. Contains a vector of generated response for all subjects.}
#'             \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'             }
#' @description
#' In this example, the continuous outcome is a patient's change from baseline. For this function, 20% of patients are believed to have no change due to treatment.  
#' As such, this function simulates patient outcome where, on average, 20% will have a value of 0 for the outcome and 80%, on average, will have their value
#' simulated from a normal distribution with the mean and standard deviation as sent from East Horizon. 
SimulatePatientOutcomePercentAtZero <- function(NumSub, NumArms, TreatmentID, Mean, StdDev, UserParam = NULL)
{
    # If the user did not specify the user parameters, but still called this function then the probability
    # of a 0 outcome is 0 for both treatments
    if( is.null( UserParam ) )
    {
        UserParam <- list( dProbOfZeroOutcomeCtrl = 0,
                           dProbOfZeroOutcomeExp1 = 0,
                           dProbOfZeroOutcomeExp2 = 0 )
    }
    
    # Create the vector of probabilities of a 0 outcome for each treatment to be used in the for loop below
    vProbabilityOfZeroOutcome <- c( UserParam$dProbOfZeroOutcomeCtrl,
                                    UserParam$dProbOfZeroOutcomeExp1,
                                    UserParam$dProbOfZeroOutcomeExp2 )    # For this example, 20% of patients do not respond to treatments and thus have no change from baseline.  
    
    nError           <- 0 # East code for no errors occurred 
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    
    
    # Loop over the patients and simulate the outcome according to the treatment they 
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # Convert to 1-based index
        dProbZeroOutcome            <- vProbabilityOfZeroOutcome[ nTreatmentID ]
        
        # Need to check the probability of a 0 outcome to make sure it is in the range (0, 1) and if not simulate the outcome accordingly 
        if( dProbZeroOutcome > 0 & dProbZeroOutcome < 1 ) # Probability is valid, so need to simulate if the patient is a 0 response
            nResponseIsZero <- rbinom( 1, 1, dProbZeroOutcome )
        else if( dProbZeroOutcome <= 0 )   # If Probability of a 0  <= 0
            nResponseIsZero <- 0
        else                        # if the probability of a 0 >= 1 --> Don't need to simulate from the normal distribution as all patients in the treatment are a 0
            nResponseIsZero <- 1
        
        
        if( nResponseIsZero == 0  )  # The patient responded, so we need to simulate their outcome from a normal distribution with the specified mean and standard deviation 
            vPatientOutcome[ nPatIndx ] <- rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ])
    }
    
    if(  any( is.na( vPatientOutcome )==TRUE) )
        nError <- -100
    
    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ))
}