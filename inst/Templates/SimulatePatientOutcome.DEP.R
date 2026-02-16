#  Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template for simulating patient data in R when the outcome is Dual Endpoints. 
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param NumArm The number of arms in the trial including experimental and control, integer value
#' @param ArrivalTime Arrival times of the subjects, numeric vector, length( ArrivalTime ) = NumSub
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param EndpointType A vector of endpoint type for each endpoint, 0 (Continuous), 1 (Binary), 2 (TTE). length( EndpointType ) = number of endpoints.
#' @param EndpointName A vector of endpoint names for each endpoint, length( EndpointType ) = number of endpoints.
#' The parameters SurvMethod, NumPrd, PrdTime, SurvParam, and PropResp are lists containing two elements -- first corresponds to the first endpoint and second to second.
#' Below are the details of elements within the respective parameters.
#' @param SurvMethod A list containing the input methods for each endpoint. TTE endpoint has values 1 (Hazard Rates), 2 (Cumulative % Survivals), 3 (Median Survival Times). Non-TTE endpoints have value NA.
#' @param NumPrd A list containing the number of time periods specified for each endpoint. For TTE endpoints, this represents the number of time intervals for which hazard rates or survival percentages are defined. For non-TTE endpoints, this value is set to NA.
#' @param PrdTime \describe{ 
#'      A list where each element is a vector of period times for TTE endpoints, dependent on the corresponding SurvMethod value. For non-TTE endpoints, this value is set to NA.
#'      \item{If SurvMethod is 1 (Hazard Rates)}{Element is a vector specifying the starting times of each hazard piece. The number of elements equals NumPrd.}
#'      \item{If SurvMethod is 2 (Cumulative % Survivals)}{Element is a vector specifying the time points at which the cumulative survival percentages are defined. The number of elements equals NumPrd.}
#'      \item{If SurvMethod is 3 (Median Survival Times)}{Element is 0 by default as no time periods need to be defined.}
#'      }
#' @param SurvParam \describe{
#'    A list where each element is a 2D array of parameters used to generate survival times based on the corresponding SurvMethod value:
#'    \item{If SurvMethod is 1 (Hazard Rates)}{The element is an array (NumPrd rows, NumArm columns) that specifies arm-specific hazard rates (one rate per arm per time period). 
#'    Element [i, j] specifies the hazard rate in the i-th time period for the j-th arm.
#'    Arms are arranged in columns: column 1 is control arm, column 2 is experimental arm.
#'    Time periods are arranged in rows: row 1 is time period 1, row 2 is time period 2, etc.}
#'    \item{If SurvMethod is 2 (Cumulative % Survivals)}{The element is an array (NumPrd rows, NumArm columns) that specifies arm-specific cumulative survival percentages. 
#'    Element [i, j] specifies the cumulative survival percentage at the i-th time point for the j-th arm.}
#'    \item{If SurvMethod is 3 (Median Survival Times)}{The element is a 1 x NumArm array specifying the median survival time for each arm. 
#'    Column 1 is control arm, column 2 is experimental arm.}
#'  }
#' @param PropResp \describe{
#'    A list where each element is a vector of expected proportions of responders in each arm for binary endpoints.
#'    \item{For binary endpoints}{Element is a vector of length NumArm, where each value represents the expected response proportion for the corresponding arm.}
#'    \item{For non-binary endpoints}{Element is set to NA as response proportions are not applicable.}
#'    }
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
#' @param  UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example.
#' If UserParam are supplied in East Horizon, they will be an element in the list, eg UserParam$ParameterName.  
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed. 
#'             \describe{
#'             \item{Response}{Required. A list which contains vectors of generated response values for each endpoint. It will contain survival times for TTE endpoints and appropriate response values for Binary and Continous endpoints.}
#'             \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     } 
#' @description
#' This template can be used as a starting point for developing custom functionality. The function signature must remain the same.
#' If your custom logic requires use of additional parameters that are not listed above, add them to UserParam.
{{FUNCTION_NAME}} <- function( NumSub, NumArm, ArrivalTime, TreatmentID, EndpointType, EndpointName, Correlation, SurvMethod = NULL, NumPrd = NULL, PrdTime = NULL, SurvParam = NULL, PropResp = NULL, UserParam = NULL)
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
	    # take care of any issues.   Also, if there is a default value for the parameters, you may want to set them here.
	    
	    # EXAMPLE - Set the default if needed
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
