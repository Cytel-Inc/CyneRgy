#' @param SimulatePatientOutcomeMultiArmPercentAtZero.Binary
#' @title Simulate patient binary outcomes from a binary distribution with a specified percent of treatment-resistant patients for multi-arm trials
#' @param NumSub Integer. The number of subjects to be simulated.
#' @param NumArm Integer. The number of arms in the trial, including the placebo/control.
#' @param ArrivalTime Arrival times of the subjects, numeric vector, length( ArrivalTime ) = NumSub.
#' @param TreatmentID A vector specifying the arm index for each subject. The index for the placebo/control arm is 0.
#' @param PropResp A vector of expected proportions of response for each arm
#' @param UserParam A list of user defined parameters in East Horizon. The default must be NULL resulting in ignoring the percent of patients at 0.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'    \item{UserParam$dProbOfTreatmentResistantCtrl}{A value in (0, 1) that defines the probability a patient is treatment resistant on the control arm.}
#'    \item{UserParam$dProbOfTreatmentResistantExp1}{A value in (0, 1) that defines the probability a patient is treatment resistant on the experimental arm 1.}
#'    \item{UserParam$dProbOfTreatmentResistantExp2}{A value in (0, 1) that defines the probability a patient is treatment resistant on the experimental arm 2.}
#' }
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed. 
#'             \describe{
#'             \item{Response}{Required numeric value. Contains a vector of generated binary response for all subjects.}
#'             \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'             }
#' @description
#' In this example, the binary outcome is a patient's response to treatments (0 non-response  or 1 response).   
#' For this function, a percent of patients are believed to be treatment resistant,
#' meaning the patient will not respond to any treatment and their outcome is always a 0.  
#' 
#' The steps to simulating patient data in this example follows a two-step procedure.  
#'  Step 1: Determine if the patient is treatment resistant by simulating a binary variable with the probability of success defined by UserParam$dProbOfTreatmentResistantCtrl, 
#'  UserParam$dProbOfTreatmentResistantExp1 or UserParam$dProbOfTreatmentResistantExp2
#' Step 2: If the patient is determined to be treatment resistant in Step 1, their outcome is set to 0. Otherwise, their outcome is simulated from
#' a binomial distribution using the response probabilities provided in PropResp. 
SimulatePatientOutcomeMultiArmPercentAtZero.Binary <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL )
{

    # If the user did not specify the user parameters, but still called this function then the probability
    # of treatment resistant is 0 for both treatments
    if( is.null( UserParam ))
    {
        UserParam <- list( dProbOfTreatmentResistantCtrl = 0,
                           dProbOfTreatmentResistantExp1 = 0,
                           dProbOfTreatmentResistantExp2 = 0 )
    }
    
    #Create the vector of probabilities of a 0 outcome for each treatment to be used in the for loop below
    vProbabilityOfTreatmentResistant <- c( UserParam$dProbOfTreatmentResistantCtrl,
                                           UserParam$dProbOfTreatmentResistantExp1,
                                           UserParam$dProbOfTreatmentResistantExp2 )    # By default, 0% of patients are treatment resistant
    
    nError           <- 0 # East code for no errors occurred 
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    
    
    # Loop over the patients and simulate the outcome according to the treatment they 
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # Convert to 1-based index
        probResist                  <- vProbabilityOfTreatmentResistant[ nTreatmentID ]
        
        # Determine if patient is treatment resistant
        if( probResist > 0 & probResist < 1 )
        {
            nTreatmentResistant <- rbinom( 1, 1, probResist )
        }
        else if( probResist <= 0 )
        {
            nTreatmentResistant <- 0
        }
        else
        {
            nTreatmentResistant <- 1
        }
        
        # If nTreatmentResistant == 1, the patient outcome is a 0 and we don't need to simulate it. 
        
        if( nTreatmentResistant == 0 )  # The patient is not resistant, so we need to simulate their outcome from a binary distribution 
        {
            vPatientOutcome[ nPatIndx ] <- rbinom( 1, 1, PropResp[ nTreatmentID ])
        }
    }
    
    if( any( is.na( vPatientOutcome ) == TRUE ))
    {
        nError <- -100
    }
   
    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError )))
}