######################################################################################################################## .
#' @name SimulatePatientOutcomeMultiArmPercentAtZero.Binary
#' @title Simulate Multi-Arm Binary Outcomes with Treatment Resistance
#' @description Simulates binary responses while allowing arm-specific probabilities that a patient is treatment
#' resistant and therefore cannot respond.
#' @author Gabriel Potvin and Anoop Singh Rawat
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param PropResp Numeric vector of length `NumArm`, containing response probabilities for control followed by each experimental arm.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
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
#' Each nonresistant patient's outcome is sampled using the response probability in `PropResp`.
######################################################################################################################## .

SimulatePatientOutcomeMultiArmPercentAtZero.Binary <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL )
{

    # If the user did not specify the user parameters, but still called this function then the probability
    # of treatment resistant is 0 for both treatments
    if( is.null( UserParam ) )
    {
        UserParam <- list( dProbOfTreatmentResistantCtrl = 0,
                           dProbOfTreatmentResistantExp1 = 0,
                           dProbOfTreatmentResistantExp2 = 0 )
    }

    #Create the vector of probabilities of a 0 outcome for each treatment to be used in the for loop below
    vProbabilityOfTreatmentResistant <- c( UserParam$dProbOfTreatmentResistantCtrl,
                                           UserParam$dProbOfTreatmentResistantExp1,
                                           UserParam$dProbOfTreatmentResistantExp2 )    # By default, 0% of patients are treatment resistant

    nError           <- 0 # No errors occurred
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
            vPatientOutcome[ nPatIndx ] <- rbinom( 1, 1, PropResp[ nTreatmentID ] )
        }
    }

    if( any( is.na( vPatientOutcome ) == TRUE ) )
    {
        nError <- -100
    }

    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ) )
}
