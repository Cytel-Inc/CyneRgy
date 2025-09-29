#  Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template for simulating patient data in R when the outcome is Dual Endpoints. 
#' @param NumSub The number of patient times to generate for the trial.  This is a single numeric value, eg 250.
#' @param NumArm The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
#' @param ArrivalTime The Array specifying Arrival Times for all patients.  length( ArrivalTime ) = NumSub 
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param EndpointType A vector of endpoint type for each endpoint, 0 (Continuous), 1 (Binary), 2 (TTE). length( EndpointType ) = number of endpoints.
#' @param EndpointName A vector of endpoint names for each endpoint, length( EndpointType ) = number of endpoints.
#' The parameters SurvMethod, NumPrd, PrdTime, SurvParam, and PropResp are lists containing two elements, each corresponding to the input for a specific endpoint.
#' Below is the detail of elements within the respective parameters.
#' @param SurvMethod Contains elements for input methods for TTE endpoints. An element has value 1 (Hazard Rates), 2 (Cumulative % Survivals), 3 (Median Survival Times), NA (non-TTE Endpoint).
#' @param NumPrd Contains elements representing the number of time periods specified for TTE endpoints, and are set to NA for non-TTE endpoints.
#' @param PrdTime \describe{ 
#'      Contains elements where each is a vector of period times for TTE endpoints, depend on corresponding SurvMethod as mentioned below, and are set to NA for non-TTE endpoints.
#'      \item{If input method is Hazard Rates}{element is a vector of starting times of hazard pieces.}
#'      \item{If input method is Cumulative % Survivals}{element is a vector of times at which the cumulative % survivals are specified.}
#'      \item{If input method is Median Survival Times}{element is 0 by default.}
#'      }
#' @param SurvParam \describe{Contains elements where each is a 2D array of parameters used to generate survival times. Each array has the following structure:
#'    \item{If input method is Hazard Rates}{The element is an array (NumPrd rows, NumArm columns) that specifies arm by arm hazard rates (one rate per arm per piece). 
#'    Thus [i, j] the element of the array specifies hazard rate in ith period for jth arm.
#'    Arms are in columns with column 1 is control, column 2 is experimental
#'    Time periods are in rows, row 1 is time period 1, row 2 is time period 2...}
#'    \item{If input method is Cumulative % Survivals}{The element is an array (NumPrd rows,NumArm columns) specifies arm by arm the Cum % Survivals (one value per arm per piece). Thus, SurvParam [i, j] specifies Cum % Survivals in ith period for jth arm.}
#'    \item{If input method is Median Survival Times}{The element is a 1 x 2 array with median survival times on each arms. Column 1 is control, column 2 is experimental }
#'  }
#' @param PropResp \describe{Contains elements where each is a vector of expected proportions of responders in each arm for binary endpoints, and are set to NA for non-binary endpoints.
#' @param Correlation \describe{Correlation between two endpoints as mentioned below,}
#'    \item{0} {Uncorrelated}  
#'    \item{1} {Very Weak Positive}  
#'    \item{2} {Weak Positive}  
#'    \item{3} {Moderate Positive} 
#'    \item{4} {Strong Positive}  
#'    \item{5} {Very Strong Positive}
#'    \item{-1} {Very Weak Negative} 
#'    \item{-2} {Weak Negative} 
#'    \item{-3} {Moderate Negative}  
#'    \item{-4} {Strong Negative} 
#'    \item{-5} {Very Strong Negative}  
#' @param  UserParam A list of user defined parameters in East.   You must have a default = NULL, as in this example.
#' If UseParam are supplied in East or East Horizon, they will be an element in the list, eg UserParam$ParameterName.  
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed. 
#'             \describe{
#'             \item{Response}{Required. A list which contains arrays of generated response values for each endpoint. It will contain survival times for TTE endpoints and appropriate response values for Binary and Continous endpoints.}
#'             \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'             }  
#' @description
#' This template can be used as a starting point for developing custom functionality.  The function signature must remain the same.  
#' However, you may choose to ignore the parameters ArrivalTime, SurvMethod, NumPrd, PrdTime, SurvParam, PropResp, and Correlation if the patient simulator.
#' If you are creating task that requires use of additional parameters that listed above, add that as element to to UserParam.
{{FUNCTION_NAME}} <- function( NumSub, NumArm, ArrivalTime, TreatmentID, EndpointType, EndpointName, Correlation, SurvMethod=NULL, NumPrd=NULL, PrdTime=NULL, SurvParam=NULL, PropResp=NULL, UserParam = NULL)
{
  # Step 1 - Initialize the return variables or other variables needed ####
	Error               <- 0
  vPatientOutcomeEP1  <- rep( 0, NumSub )  
  vPatientOutcomeEP2  <- rep( 0, NumSub )  
  Response            <- list()
  
	# Step 2 - Validate custom variable input and set defaults ####
	if( is.null( UserParam ) )
	{
	    
	    # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
	    # take care of any issues.   Also, if there is a default value for the parameters you may want to set them here.  Default values usually
	    # are applied to have the same functionality as East, see the first example
	    
	    # EXMAPLE - Set the default if needed
	    #UserParam <- list( dProbOfZeroOutcomeCtrl = 0, dProbOfZeroOutcomeExp = 0 )
	}
	
	# Step 3 - Simulate the patient data and store in Response ####
	for(nSubjID in 1:NumSub) 
  {
	  # Write code to simulate patient data with a specified correlation.
	}

	# Use appropriate error handling and modify the
	# Error appropriately in each of the methods

  Response[[EndpointName[[1]]]] <- vPatientOutcomeEP1
  Response[[EndpointName[[2]]]] <- vPatientOutcomeEP2
  
	return( list( Response = as.list( Response ), ErrorCode = as.integer( Error )))
}
