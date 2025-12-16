#  Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template for simulating patient arrival time.
#' @param NumPat The number of participants that need to be simulated, integer value
#' @param NumPrd Number of time periods that are provided.
#' @param PrdStart Vector with start of a time interval, PrdStarr[ 1 ] = 0
#' @param AccrRate the accrual rate in each period.
#' @param UserParam A list of user defined parameters that may be provided in East or East Horizon. You must have a default of NULL, as in this example.
#' The user may supplies rates names Rate1, Rate2, ...., RateX to represent the per unit time accrual rate where the maximum RateX is used after the ramp-up.
#'    \describe{
#'      \item{Rate1}{The rate in the first unit of time}
#'      \item{Rate2}{The rate in the first second of time}
#'    }
#' @description
#' This template can be used as a starting point for developing custom functionality when the patient arrives in the trial.  
#' The function signature must remain the same.  
{{FUNCTION_NAME}}  <- function(NumPat, NumPrd, PrdStart, AccrRate, UserParam = NULL )
{
    # Step 1 - Initialize the return variables or other variables needed ####
    Error 	            <- 0
    vPatientArrivalTime <- rep( 0, NumPat )  # Note, as you simulate the patient data put in in this vector so it can be returned
    
    # Step 2 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {
        
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues. Also, if there is a default value for the parameters you may want to set them here. Default values usually
        # are applied to have the same functionality as East, see the first example
        
        # EXMAPLE - Set the default if needed
        # UserParam <- list( dRate = 0.5 )
    }
    
    # Step 3 - Loop over the patients and simulate the patient arrival times in the trial ####
    
    #Example 1 - #### 
    for( nPatIndx in 1:NumPat )
    {
        # Add code here to simulate the patient arrival times. 
        # The arrival times should be increasing 
        
    }
    
    return(list(ArrivalTime = as.double(vPatientArrivalTime), ErrorCode =as.integer(Error)))
}
