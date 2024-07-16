#  Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @param NumSub: Mandatory. The number of subjects that need to be simulated, integer value. The argument value is passed from Engine.
#' @param NumArms: Mandatory. The number of arms in the trial including experimental and control, integer value. The argument value is passed from Engine.
#' @param AllocRatio: Mandatory. The ratio of the experimental group sample size (nt) to control group sample size (nc) i.e. (nt/nc). The argument value is passed from Engine.
#' @param UserParam : Optional. User can pass custom scalar variables defined by users as a member of this list. 
#'                   User should access the variables using names, for example UserParam$Var1 and not order. 
#'                   These variables can be of the following types: Integer, Numeric, or Character

#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{TreatmentID}{Required value. This is a binary vector defining the treatment ID where:
#'                                  \describe{
#'                                    \item{TreatmentID = 0}{ Subject allotted to Control arm }
#'                                    \item{TreatmentID = 1}{ Subject allotted to Experimental arm}
#'                                    } 
#'                                    }
#'                  \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Non fatal error, current simulation is aborted but the next simulations will run}
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
  
  # Convert the Allocation Ratio to Allocation Fraction for control and treatment arm
  dAllocFraction                <- c( vAllocRatio[ 1 ]/sum( vAllocRatio ), 1 - vAllocRatio[ 1 ]/sum( vAllocRatio ) )
  vSampleSizeArmWise            <- c( round( NumSub * dAllocFraction[ 1 ]), NumSub - round( NumSub * dAllocFraction[ 1 ] ) )
  
  # Find the indices for Control and treatment arms
  vControlArmIndex              <- sample( 1:NumSub, size = vSampleSizeArmWise[ 1 ], replace = FALSE )
  vTreatmentArmIndex            <- c( 1:NumSub )[ -vControlArmIndex ]
  
  # Generate a vector of zeroes of size NumSub and then replace the Treatment Indices with 1.
  
  retval                        <- rep( 0, NumSub )
  retval[ vTreatmentArmIndex ]  <-  1
  
  return(list(TreatmentID = as.integer(retval), ErrorCode = as.integer(Error)))
}