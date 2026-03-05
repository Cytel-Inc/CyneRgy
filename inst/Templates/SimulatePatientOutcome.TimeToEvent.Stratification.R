#  Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template for simulating patient data in R when the outcome time is time-to-event. 
#' @param NumSub The number of patient times to generate for the trial.  This is a single numeric value, eg 250.
#' @param NumArm  The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
#' @param ArrivalTime Arrival times of the subjects, numeric vector, length( ArrivalTime ) = NumSub
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param StratumID A vector of stratum ids, 1 = stratum 1, 2 = strata 2..... length( StratumID ) = NumSub
#' @param SurvMethod - This values is pulled from the Input Method drop-down list. This will be 1 (Hazard Rate), 2 (Cumulative % survival), 3 (Medians)
#' @param NumPrd Number of time periods that are provided. 
#' @param PrdTime \describe{ 
#'      \item{If SurvMethod = 1}{PrdTime is a vector of starting times of hazard pieces.}
#'      \item{If SurvMethod = 2}{Times at which the cumulative % survivals are specified.}
#'      \item{If SurvMethod = 3}{Period time is 0 by default}
#'      }
#' @param SurvParam \describe{Depends on the table in the Response Generation tab.
#'    A 2-D array of parameters to generate the survival times, defined by stratum and arm.
#'
#'    \item{If SurvMethod = 1}{SurvParam is an array (NumStratum rows, NumArm columns)
#'    that specifies stratum-by-arm hazard rates (one rate per arm per stratum).
#'    Thus, SurvParam[i, j] specifies the hazard rate for the i-th stratum and j-th arm.
#'    Arms are in columns, with column 1 as control and column 2 as experimental.}
#'
#'    \item{If SurvMethod = 2}{SurvParam is an array (NumStratum rows, NumArm columns)
#'    that specifies stratum-by-arm cumulative % survival values
#'    (one value per arm per stratum).
#'    Thus, SurvParam[i, j] specifies the cumulative % survival
#'    for the i-th stratum and j-th arm.}
#'
#'    \item{If SurvMethod = 3}{SurvParam is an array (NumStratum rows, NumArm columns)
#'    that specifies stratum-by-arm median survival times.
#'    Thus, SurvParam[i, j] specifies the median survival time
#'    for the i-th stratum and j-th arm.
#'    Column 1 is control and column 2 is experimental.}
#' }
#' @param  UserParam A list of user defined parameters in East.   You must have a default = NULL, as in this example.
#' If UseParam are supplied in East Horizon, they will be an element in the list, eg UserParam$ParameterName.  
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
# Function Template for Generating Survival Times (Time to Response)
GenSurvTime <- function(NumSub, NumArm, ArrivalTime, TreatmentID, StratumID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL)
{
  # TO DO : Modify this function apprpriately  
  Error 	= 0
  retval 	= c()
  # Initialising Response Array to 0	
  for(i in 1:NumSub)
  {
    retval[i] = 0;	
  }
  if(SurvMethod == 1)   # Hazard Rates
  {
    # Write the actual code for SurvMethod 1 
    # here.
    # Store the generated survival times in an 
    # array called retval.
  }
  if(SurvMethod == 2)   # Cumulative % Survivals
  {
    # Write the actual code for SurvMethod 2 
    # here.
    # Store the generated survival times in an 
    # array called retval.
  }
  if(SurvMethod == 3)   # Median Survival Times
  {
    # Write the actual code for SurvMethod 3 
    # here.
    # Store the generated survival times in an 
    # array called retval.
  }
  # Use appropriate error handling and modify the
  # Error appropriately in each of the methods  
  return(list(SurvivalTime = as.double(retval), ErrorCode = as.integer(Error)))
}
