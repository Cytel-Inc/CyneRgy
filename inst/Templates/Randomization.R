#  Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @param NumSub: Mandatory. The number of subjects that need to be simulated, integer value. The argument value is passed from Engine.
#' @param NumArms: Mandatory. The number of arms in the trial including experimental and control, integer value. The argument value is passed from Engine.
#' @param AllocRatio: Mandatory. Vector containing the expected allocation ratios - relative to the control arm - for the treatment arms. Length of vector = (Number of arms - 1) 
#' @param UserParam : Optional. User can pass custom scalar variables defined by users as a member of this list. 
#'                    User should access the variables using names, for example UserParam$Var1 and not order. 
#'                    These variables can be of the following types: Integer, Numeric, or Character

#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{TreatmentID}{Required value. This is a vector of treatment ID allocation per subject where:
#'                                  \describe{
#'                                    \item{TreatmentID = 0}{ Subject allotted to Control arm }
#'                                    \item{TreatmentID = n}{ Subject allotted to Experimental arm n where n >=1 }
#'                                    } 
#'                                    }
#'                  \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'                      }
#'                      
{{FUNCTION_NAME}} <- function(NumSub, NumArms, AllocRatio, UserParam = NULL)
{
  
  Error 	                      <- 0
  
  # Allocation ratio on control and treatment arm
  vAllocRatio                   <- c( 1, AllocRatio )
  
  # Convert the Allocation Ratio to Allocation Fraction for control and treatment arms
  dAllocFraction                <- c( vAllocRatio[ 1 ]/sum( vAllocRatio ), 1 - vAllocRatio[ 1 ]/sum( vAllocRatio ) )
  vTreatmentIDs                 <- sample(0:( NumArms - 1 ), NumSub, prob = dAllocFraction, replace = TRUE)
  
  return(list(TreatmentID = as.integer(vTreatmentIDs), ErrorCode = as.integer(Error)))
}