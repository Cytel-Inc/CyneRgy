#  Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template for simulating patient data in R when the outcome time is time-to-event. 
#' @param NumSub The number of patient times to generate for the trial.  This is a single numeric value, eg 250.
#' @param NumArm  The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param SurvMethod - This values is pulled from the Input Method drop-down list. This will be 1 (Hazard Rate), 2 (Cumulative % survival), 3 (Medians)
#' @param NumPrd Number of time periods that are provided. 
#' @param PrdTime \describe{ 
#'      \item{If SurvMethod = 1}{PrdTime is a vector of starting times of hazard pieces.}
#'      \item{If SurvMethod = 2}{Times at which the cumulative % survivals are specified.}
#'      \item{If SurvMethod = 3}{Period time is 0 by default}
#'      }
#' @param SurvParam \describe{Depends on the table in the Response Generation tab. 2‐D array of parameters to generate the survival times
#'    \item{If SurvMethod is 1}{SurvParam is an array (NumPrd rows, NumArm columns) that specifies arm by arm hazard rates (one rate per arm per piece). 
#'    Thus SurvParam [i, j] specifies hazard rate in ith period for jth arm.
#'    Arms are in columns with column 1 is control, column 2 is experimental
#'    Time periods are in rows, row 1 is time period 1, row 2 is time period 2...}
#'    \item{If SurvMethod is 2}{SurvParam is an array (NumPrd rows,NumArm columns) specifies arm by arm the Cum % Survivals (one value per arm per piece). Thus, SurvParam [i, j] specifies Cum % Survivals in ith period for jth arm.}
#'    \item{If SurvMethod is 3}{SurvParam will be a 1 x 2 array with median survival times on each arms. Column 1 is control, column 2 is experimental }
#'  }
#' @param  UserParam A list of user defined parameters in East.   You must have a default = NULL, as in this example.
#' If UseParam are supplied in East or Solara, they will be an element in the list, eg UserParam$ParameterName.  
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed. 
#'             \describe{
#'             \item{SurvivalTime}{Required numeric value. A vector of generated time to response values for each subject.}
#'             \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'             }  
#' @description
#' This template can be used as a starting point for developing custom functionality.  The function signature must remain the same.  
#' However, you may choose to ignore the parameters SurvMethod, NumPrd, PrdTime, and SurvParam if the patient simulator
#' you are creating only requires use of parameters the user will add to UserParam
{{FUNCTION_NAME}} <- function( NumSub, NumArm, TreatmentID,  SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{
    # Step 1 - Initialize the return variables or other variables needed ####
	Error 	        <- 0
	vPatientOutcome <- rep( 0, NumSub )  # Note, as you simulate the patient data put in in this vector so it can be returned

	# Step 2 - Validate custom variable input and set defaults ####
	if( is.null( UserParam ) )
	{
	    
	    # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
	    # take care of any issues.   Also, if there is a default value for the parameters you may want to set them here.  Default values usually
	    # are applied to have the same functionality as East, see the first example
	    
	    # EXMAPLE - Set the default if needed
	    #UserParam <- list( dProbOfZeroOutcomeCtrl = 0, dProbOfZeroOutcomeExp = 0 )
	}
	
	# Step 3 - Simulate the patient data and store in vPatientOutcome ####
	
	# Example 1 of using the parameters East/East Horizon Explore sent - If you don't need this block of code you may delete it.
	if(SurvMethod == 1)   # Hazard Rates
	{
		# Simulate patient data using the hazard rates
	}

	if(SurvMethod == 2)   # Cumulative % Survivals
	{
	    # Simulate patient data using Cumulative % Survivals
	}
	
	if(SurvMethod == 3)   # Median Survival Times
	{
	    # Simulate patient data using median survival times
	}
    # End of example block
	
	# Example 2 - Loop over the patient vector and simulate from an Exponential distribution, as an example
	# vTrueRates <- c( 1/UserParam$dMeanCtrl, 1/UserParam$dMeanExp )
	# for( nPatIndx in 1:NumSub )
	# {
	#     nTreatmentID                 <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East/Solara has the treatments as 0, 1 so need to add 1 to get a vector index
	#     vPatientOutcome[ nPatIndx ]  <- rexp( 1, vTrueRates[ nTreatmentID ] )       
	# }
	# End of example block
	
	
	# Use appropriate error handling and modify the
	# Error appropriately in each of the methods

	return( list( SurvivalTime = as.double( vPatientOutcome ), ErrorCode = as.integer( Error )))
}
