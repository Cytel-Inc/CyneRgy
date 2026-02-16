#  Last Modified Date: 02/10/2026
#' @name SimulatePatientOutcome
#' @title Function to simulate patient data with specified mean and standard deviation for each arm
#' @param NumSub The number of subjects that need to be simulated
#' @param ArrivalTime Arrival times of the subjects, numeric vector, length( ArrivalTime ) = NumSub
#' @param TreatmentID A vector of treatment ids, 0 = Treatment 1, 1 = Treatment 2, length( TreatmentID ) = NumSub
#' @param Mean A vector of length = 2 with the means of the two treatments.
#' @param StdDev A vector of length = 2 with the standard deviations of each treatment
#' @param UserParam A list of user defined parameters in East or East Horizon. The list must contain:
#'   \itemize{
#'     \item \code{dMeanFollowUpCtrl} – Mean at follow-up for the control group.
#'     \item \code{dMeanFollowUpExp} – Mean at follow-up for the experimental group.
#'     \item \code{dStdDevFollowUpCtrl} – Standard deviation at follow-up for the control group.
#'     \item \code{dStdDevFollowUpExp} – Standard deviation at follow-up for the experimental group.
#'   }    

SimulatePatientOutcome <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL )
{
    # Initialize variable
    nError <- 0 # East code for no errors occurred 
    vPatientOutcome <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    vMeanFollowUp   <- c( UserParam$dMeanFollowUpCtrl,  UserParam$dMeanFollowUpExp )
    vStdDevFollowUp <- c( UserParam$dStdDevFollowUpCtrl,  UserParam$dStdDevFollowUpExp )
    # Create vector with the standard deviation
    
    # Loop over the patients and simulate the outcome according to the treatment they received
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East has the treatments as 0, 1 so need to add 1 to get a vector index
        
        # Simulate from a normal distribution and round to nearest integer
        outcome1 <- round( rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ] ) )
        
        # Fix the next line to use the vector you create above
        outcome2 <- round( rnorm( 1, vMeanFollowUp[ nTreatmentID ], vStdDevFollowUp[ nTreatmentID ] ) )
        
        # Ensure outcome is within specified range
        outcome1 <- max( min( outcome1, 45 ), 9 )
        outcome2 <- max( min( outcome2, 45 ), 9 )
        
        # Note: Response = Baseline - Followup so a value above 0 means the patient improved.
        vPatientOutcome[ nPatIndx ] <- outcome1 - outcome2
    }
    
    # Error Checking
    if( any( is.na( vPatientOutcome ) ) )
        nError <- -100
    
    # Build the return object, add other variables to the list as needed
    lReturn <- list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) )
    return( lReturn )
}

