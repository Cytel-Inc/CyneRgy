######################################################################################################################## .
#' @name SimulatePatientOutcomePercentAtZero.Binary
#' @title Simulate patient outcomes from a binary distribution with a percent of patients are treatment resistant.
#' @author J. Kyle Wathen
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param PropResp Numeric vector of length `NumArm`, containing response probabilities for control followed by each experimental arm.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'    \item{UserParam$dProbOfTreatmentResistantCtrl}{A value in (0, 1) that defines the probability a patient is treatment resistant the control (ctrl) treatment.}
#'    \item{UserParam$dProbOfTreatmentResistantExp}{A value in (0, 1) that defines the probability a patient is treatment resistant experimental (exp) treatment.}
#' }
#' @description
#' In this example, the binary outcome is a patient's response to treatment (0 non-response  or 1 response).
#' For this function, a percent of patients are believed to be treatment resistant,
#' meaning the patient will not respond to any treatment and their outcome is always a 0.
#'
#' The steps to simulating patient data in this example follows a two-step procedure.
#'  Step 1: Determine if the patient is treatment resistant by simulating a binary variable with the probability of success defined by UserParam$dProbOfTreatmentResistantCtrl or
#'  UserParam$dProbOfTreatmentResistantExp
#'  Step 2: If the value in Step 1, indicating the patient is treatment resistant then their outcome is set to 0, otherwise the simulate their
#'  outcome from a binomial distribution using the response probabilities provided in PropRest.
#' @return A named list containing `Response`, a binary vector of length `NumSub`, and integer `ErrorCode`.
######################################################################################################################## .
SimulatePatientOutcomePercentAtZero.Binary <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL )
{
    if( is.null( UserParam ) )
    {
        UserParam <- list( dProbOfTreatmentResistantCtrl = 0, dProbOfTreatmentResistantExp = 0 )
    }

    #Create the vector of probabilities of a 0 outcome for each treatment to be used in the for loop below
    vProbabilityOfTreatmentResistant <- c( UserParam$dProbOfTreatmentResistantCtrl, UserParam$dProbOfTreatmentResistantExp )    # By default, 0% of patients are treatment resistant

    nError           <- 0 # No errors occurred
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    # Loop over the patients and simulate the outcome according to the treatment they
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index

        # Need to check the probability of a 0 outcome to make sure it is in the range (0, 1) and if not simulate the outcome accordingly
        if( vProbabilityOfTreatmentResistant[ nTreatmentID ] > 0 & vProbabilityOfTreatmentResistant[ nTreatmentID ] < 1 ) # Probability is valid, so need to simulate if the patient is a 0 response
            nTreatmentResistant <- rbinom( 1, 1, vProbabilityOfTreatmentResistant[ nTreatmentID ] )
        else if( vProbabilityOfTreatmentResistant[ nTreatmentID ] <= 0 )   # If Probability of a 0  <= 0
            nTreatmentResistant <- 0
        else                        # if the probability of a 0 >= 1 --> Don't need to simulate from the binary distribution as all patients in the treatment are a 0
            nTreatmentResistant <- 1

        # If nTreatmentResistant == 1 then the patient outcome is a a 0 and we don't need to simulate it.

        if( nTreatmentResistant == 0 )  # The patient responded, so we need to simulate their outcome from a binary distribution
            vPatientOutcome[ nPatIndx ] <- rbinom( 1, 1, PropResp[ nTreatmentID ] )
    }

    if( any( is.na( vPatientOutcome ) ) )
        nError <- -100

    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ) )
}
