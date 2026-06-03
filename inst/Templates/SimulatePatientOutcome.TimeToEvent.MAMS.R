# Last Modified Date: {{CREATION_DATE}}

#' @name {{FUNCTION_NAME}}
#' @title Template for simulating survival outcomes in R.
#'
#' @param NumSub The number of subjects that need to be simulated, integer value
#'
#' @param NumArm The number of arms in the trial including placebo/control, integer value
#'
#' @param ArrivalTime Arrival times of the subjects, numeric vector,
#'        length(ArrivalTime) = NumSub
#'
#' @param TreatmentID A vector of treatment ids,
#'        0 = control, 1,2,...,NumArm-1 for treatment arms.
#'        length(TreatmentID) = NumSub
#'
#' @param SurvMethod Integer value specifying the survival generation method.
#'        \describe{
#'        \item{SurvMethod = 1}{Piecewise exponential model using hazard rates}
#'        \item{SurvMethod = 2}{Piecewise exponential model using survival probabilities}
#'        \item{SurvMethod = 3}{Exponential survival model using median survival times}
#'        }
#'
#' @param NumPrd Number of survival periods, integer value
#'
#' @param PrdTime Numeric vector containing survival period boundary times
#'
#' @param SurvParam Matrix containing survival parameters for each period and treatment arm.
#'        Interpretation depends on SurvMethod:
#'        \describe{
#'        \item{Method 1}{Hazard rates by period and treatment arm}
#'        \item{Method 2}{Survival probabilities by period and treatment arm}
#'        \item{Method 3}{Median survival times by treatment arm}
#'        }
#'
#' @param UserParam A list of user defined parameters in East or East Horizon. You must have a default of NULL, as in this example.
#'        If UserParam are supplied, they will be available as elements in the list UserParam.
#'
#' @return The function must return a list in the return statement of the function. The information below lists
#'         elements of the list, if the element is required or optional and a description of the return values if needed.
#'         \describe{
#'         \item{SurvivalTime}{Required numeric vector. Contains generated survival times for all subjects}
#'         \item{ErrorCode}{Optional integer value
#'                         \describe{
#'                         \item{ErrorCode = 0}{No Error}
#'                         \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                         \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                         }
#'                         }
#'         }
#'
#' @description
#' This template simulates time-to-event outcomes for multi-arm clinical trial simulations.
#' The function supports several survival generation approaches including piecewise
#' exponential models and median survival based exponential models.
#'
#' The function signature must remain unchanged. However, additional user-defined logic
#' and parameters may be incorporated through the UserParam list if needed.
{{FUNCTION_NAME}} <- function(NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL)
{

```
# Step 1 - Validate custom variable input and set defaults ####
if( is.null( UserParam ) )
{
    
    # If this function requires user defined parameters to be sent via the UserParam variable
    # check to make sure the values are valid and take care of any issues.
    # Also, if there is a default value for the parameters you may want to set them here
    
}


# Step 2 - Initialize variables ####
# Initialize error codes and vectors used to store simulated survival times
Error  <- 0
retval <- c()


# Step 3 - Determine which survival generation method will be used ####
# The implementation supports:
#   Method 1 - Piecewise exponential hazard model
#   Method 2 - Survival probability based piecewise exponential model
#   Method 3 - Median survival time based exponential model


# Step 4 - Compute survival distributions and hazard functions ####
# Depending on the selected method:
#   1. Compute survival probabilities
#   2. Compute cumulative distribution functions
#   3. Compute hazard rates if needed


# Step 5 - Loop over patients and simulate survival times ####
# For each patient:
#   1. Determine treatment assignment
#   2. Generate random uniform variables
#   3. Determine the survival interval
#   4. Simulate a survival time from the appropriate distribution


# Step 6 - Error checking ####
# Verify that all subjects received valid survival times
# and that no missing values were generated
if( length( retval ) != NumSub || any( is.na( retval ) == TRUE ) )
    Error <- -100


# Step 7 - Build the return object ####
lReturn <- list(
    SurvivalTime = as.double( retval ),
    ErrorCode    = as.integer( Error )
)

return( lReturn )
```

}
