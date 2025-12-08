#' @param SimulatePatientOutcomePercentAtZero.Binary
#' @title Simulate patient outcomes from a binary distribution with a percent of patients are treatment resistant. 
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param NumArm The number of arms in the trial including experimental and control, integer value
#' @param ArrivalTime Arrival times of the subjects, numeric vector, length( ArrivalTime ) = NumSub
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param PropResp A vector of expected proportions of response for each arm
#' @param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL resulting in simulating from a non-mixture distribution.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'    \item{UserParam$dProbOfTreatmentReistantCtrl}{A value in (0, 1) that defines the probability a patient is treatment resistant the control (ctrl) treatment.}
#'    \item{UserParam$dProbOfTreatmentReistantExp}{A value in (0, 1) that defines the probability a patient is treatment resistant experimental (exp) treatment.}
#' }
#' @description
#' In this example, the binary outcome is a patient's response to treatment (0 non-response  or 1 response).   
#' For this function, a percent of patients are believed to be treatment resistant,
#' meaning the patient will not respond to any treatment and their outcome is always a 0.  
#' 
#' The steps to simulating patient data in this example follows a two-step procedure.  
#'  Step 1: Determine if the patient is treatment resistant by simulating a binary variable with the probability of success defined by UserParam$dProbOfTreatmentReistantCtrl or 
#'  UserParam$dProbOfTreatmentReistantExp
#'  Step 2: If the value in Step 1, indicating the patient is treatment resistant then their outcome is set to 0, otherwise the simulate their
#'  outcome from a binomial distribution using the response probabilities provided in PropRest.  
SimulatePatientOutcomePercentAtZero.Binary <- function(NumSub, NumArm, ArrivalTime, TreatmentID, PropResp,UserParam = NULL)
{
    # Note: It can be helpful to save to the parameters that East sent.
    # The next two lines show how you could save the UserParam variable to an Rds file
    # setwd(["ENTER THE DESIRED LOCATION TO SAVE THE FILE"])
    # saveRDS( UserParam, "UserParam.Rds")
    # saveRDS( NumSub, "NumSub.Rds" )
    # saveRDS( TreatmentID, "TreatmentID.Rds" )
    # saveRDS( PropResp, "PropResp.Rds" )
    # saveRDS( NumArm, "NumArm.Rds" )
 
    
    # If the user did not specify the user parameters, but still called this function then the probability
    # of treatment resistant is 0 for both treatments
    if( is.null( UserParam ) )
    {
        UserParam <- list( dProbOfTreatmentReistantCtrl = 0, dProbOfTreatmentReistantExp = 0 )
    }
    
    #Create the vector of probabilities of a 0 outcome for each treatment to be used in the for loop below
    vProbabilityOfTreatmentResistant <- c( UserParam$dProbOfTreatmentReistantCtrl, UserParam$dProbOfTreatmentReistantExp )    # By default, 0% of patients are treatment resistant
    
    nError           <- 0 # East code for no errors occurred 
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    
    
    # Loop over the patients and simulate the outcome according to the treatment they 
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East has the treatments as 0, 1 so need to add 1 to get a vector index
        
        # Need to check the probability of a 0 outcome to make sure it is in the range (0, 1) and if not simulate the outcome accordingly 
        if( vProbabilityOfTreatmentResistant[ nTreatmentID ] > 0 & vProbabilityOfTreatmentResistant[ nTreatmentID ] < 1 ) # Probability is valid, so need to simulate if the patient is a 0 response
            nTreatmentResistant <- rbinom( 1, 1, vProbabilityOfTreatmentResistant[ nTreatmentID ] )
        else if( vProbabilityOfTreatmentResistant[ nTreatmentID ] <= 0 )   # If Probability of a 0  <= 0
            nTreatmentResistant <- 0
        else                        # if the probability of a 0 >= 1 --> Don't need to simulate from the binary distribution as all patients in the treatment are a 0
            nTreatmentResistant <- 1
        
        # If nTreatmentResistant == 1 then the patient outcome is a a 0 and we don't need to simulate it. 
        
        if( nTreatmentResistant == 0  )  # The patient responded, so we need to simulate their outcome from a binary distribution 
            vPatientOutcome[ nPatIndx ] <- rbinom( 1, 1, PropResp[ nTreatmentID ])
    }
    
    if(  any( is.na( vPatientOutcome )==TRUE) )
        nError <- -100
   
    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ))
}