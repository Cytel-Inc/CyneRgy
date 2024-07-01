#  Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @param NumSub Mandatory. The integer value specifying the number of patients or subjects in the trial. The numeric value of the argument value is sent in when called.
#' @param ProbDrop The numeric value specifying probability of dropout. Mandatory for Continuous and Binary Endpoints.
#' @param UserParam : User can pass custom scalar variables defined by users as a member of this list. 
#'                    User should access the variables using names, for example UserParam$Var1 and not order. 
#'                    These variables can be of the following types: Integer, Numeric, or Character

#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{CensorInd}{Mandatory for Continuous and Binary Endpoints. A Binary vector of length NumSub such that
#'                                  \describe{
#'                                    \item{CensorInd = 0}{ Non Completer / Dropout }
#'                                    \item{CensorInd = 1}{ Completer }
#'                                    } 
#'                                    }
#'                  \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Non fatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'                              }
#'                      
{{FUNCTION_NAME}} <- function( NumSub, ProbDrop,  UserParam = NULL ) 
{   
  
    Error 	            <- 0
  
    vCensoringIndicator <- rbinom( n = NumSub, size = 1, prob = 1 - ProbDrop )
  
    return( list( CensorInd = as.integer( vCensoringIndicator ), ErrorCode = as.integer( Error ) ) );
}
