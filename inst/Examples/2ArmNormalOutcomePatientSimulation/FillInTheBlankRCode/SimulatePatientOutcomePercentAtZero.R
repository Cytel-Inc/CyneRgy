#' @param SimulatePatientOutcomePercentAtZero
#' @title Simulate patient outcomes from a normal distribution with a percent of patients having an outcome of 0. 
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param Mean A vector of length = 2 with the means of the two treatments.
#' @param StdDev A vector of length = 2 with the standard deviations of each treatment
#' @param  UserParam A list of user defined parameters in East.   The default must be NULL resulting in ignoring the percent of patients at 0.
#' If UseParam is supplied, the list must contain the following named elements:
#'  UserParam$dProbOfZeroOutcomeCtrl - A value in (0, 1) that defines the probability a patient will have an outcome of 0 on the control (ctrl) treatment.
#'  UserParam$dProbOfZeroOutcomeExp - A value in (0, 1) that defines the probability a patient will have an outcome of 0 on the control (ctrl) treatment.
#' @description
#' In this example, the continuous outcome is a patient's change from baseline.   For this function, 20% of patients are believed to have no change due to treatment.  
#' As such, this function simulations patient outcome where, on average, 20% will have a value of 0 for the outcome and 80%, on average, will have their value
#' simulated from a normal distribution with the mean and standard deviation as sent from East. 
SimulatePatientOutcomePercentAtZero <- function(NumSub, TreatmentID, Mean, StdDev, UserParam = NULL)
{
    # Note: It can be helpful to save to the parameters that East sent.
    # The next two lines show how you could save the UserParam variable to an Rds file
    # setwd( "[ENTERED THE DESIRED LOCATION TO SAVE THE FILE]" )
    # saveRDS(UserParam, "UserParam.Rds")
    
    # If the user did not specify the user parameters, but still called this function then the probability
    # of a 0 outcome is 0 for both treatments
    if( is.null( UserParam ) )
    {
        UserParam <- list( _____________________ = 0, ___________________ = 0 )
    }
    
    #Create the vector of probabilities of a 0 outcome for each treatment to be used in the for loop below
    vProbabilityOfZeroOutcome <- c( UserParam$dProbOfZeroOutcomeCtrl, UserParam$dProbOfZeroOutcomeExp )    # For this example, 20% of patients do not respond to treatment and thus have no change from baseline.  
    
    nError           <- 0 # East code for no errors occurred 
    vPatientOutcome  <- rep( 0, _________ ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    
    
    # Loop over the patients and simulate the outcome according to the treatment they 
    for( nPatIndx in 1:________ )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East has the treatments as 0, 1 so need to add 1 to get a vector index
        
        # Need to check the probability of a 0 outcome to make sure it is in the range (0, 1) and if not simulate the outcome accordingly 
        if( vProbabilityOfZeroOutcome[ nTreatmentID ] > 0 & vProbabilityOfZeroOutcome[ nTreatmentID ] < 1 ) # Probability is valid, so need to simulate if the patient is a 0 response
            nResponseIsZero <- ________( 1, 1, vProbabilityOfZeroOutcome[ nTreatmentID ] )
        else if( vProbabilityOfZeroOutcome[ nTreatmentID ] <= 0 )   # If Probability of a 0  <= 0
            nResponseIsZero <- 0
        else                        # if the probability of a 0 >= 1 --> Don't need to simulate from the normal distribution as all patients in the treatment are a 0
            nResponseIsZero <- 1
            
        
        if( ___________________ == 0  )  # The patient responded, so we need to simulate their outcome from a normal distribution with the specified mean and standard deviation 
            vPatientOutcome[ nPatIndx ] <- rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ])
    }
    
    if(  any( is.na( vPatientOutcome )==TRUE) )
        nError <- -100
    
    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ))
}