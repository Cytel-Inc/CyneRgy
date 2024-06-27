#  Last Modified Date: {{27th June 2024}}
#' @name {{FUNCTION_NAME}}
#' @param NumSub: Mandatory. The number of subjects that need to be simulated, integer value. The argument value is passed from Engine.
#' @param NumArm: Mandatory. The number of arms in the trial including experimental and control, integer value. The argument value is passed from Engine.
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


{{FUNCTION_NAME}} <- function( NumSub, NumArms, AllocRatio, UserParam = NULL  )
{

  nError 	        <- 0
  vTreatmentID	  <- rep( 0, NumSub )   #A Binary vector of length NumSub

  # Write a code to generate Treatment ID for the subjects
  
  return( list( TreatmentID = as.double( vTreatmentID ), ErrorCode = as.integer( nError ) ) )
}