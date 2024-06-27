#  Last Modified Date: {{27th June 2024}}
#' @name {{FUNCTION_NAME}}
#' @param NumSub Mandatory. The integer value specifying the number of patients or subjects in the trial. The numeric value of the argument value is sent in when called.
#' @param NumArm Mandatory. The number of arms in the trial including experimental and control, integer value. The argument value is passed from Engine.
#' @param ProbDrop The numeric value specifying probability of dropout. Mandatory for Continuous and Binary Endpoints.
#' @param TreatmentID Vector specifying indexes of arms to which subjects are allocated (one arm index per subject). Index for placebo / control is 0. Mandatory for Time To Event and Repeated measures designs.
#' @param DropMethod Input method for specifying dropout parameters. Mandatory for Time to Event and Repeated measures designs.
#'           \describe{
#'           \item{Time to Event}{2 - Probability of dropouts}
#'           \item{Repeated Measures}{1 – Cumulative Probability of Dropout by Visit. 2 – Cumulative Probability of Dropout by Time}
#'           }
#' @param NumPrd The integer value specifying number of dropout periods. Mandatory for Time to Event endpoint
#' @param PrdTime Vector of numeric time values used to specify dropout parameters. Mandatory for Time to Event endpoint
#' @param DropParam A 2D array of parameters used to generate dropout times. Mandatory for Time to Event endpoint
#'           \describe{
#'           \item{Number of rows = Number of Dropout periods.}
#'           \item{Number of columns = Number of arms including control/placebo.}
#'           }
#' @param NumVisit Mandatory for Repeated Measures. Integer indicating number of visits.
#' @param VisitTime Mandatory for Repeated Measures. Vector containing numeric visit times for each visit.
#' @param ByTime Mandatory for Repeated Measures. Vector containing numeric by time for dropouts.
#' @param DropParamControl Mandatory for Repeated Measures. Vector containing numeric parameters used to generate dropout times for Control arm.
#' @param DropParamTrt Mandatory for Repeated Measures. Vector containing numeric parameters used to generate dropout times for Treatment arm.
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
#'                  \item{DropOutTime}{Mandatory for Time to Event endpoint and applicable for Repeated measures. A numeric array of generated dropout times.}
#'                  \item{CensorInd<NumVisit>}{Applicable for Repeated Measures design. A set of arrays of censor indicator values for all subjects. Each array corresponds to each visit user has specified.}
#'                  \item{DropoutVisitID}{Applicable for Repeated Measures design. An array of 1-based Visit ID after which the patient dropped out}
#'                      }
#'                      
#'   @return 
  #' \describe{
  #'     \item{ErrorCode (Optional)}{An integer value:  ErrorCode = 0 --> No Error
  #'                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
  #'                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted.}
  #'     \item{CensorInd (Mandatory)}{A vector of length NumSub of censor indicator values with 0 for patients that dropout eg non-completer, 1 for no dropout, eg compelter. }

{{FUNCTION_NAME}} <- function(NumSub, NumArms, AllocRatio, UserParam = NULL  )
{
  
  nError 	              <- 0
  vCensorInd	          <- rep(0, NumSub)     # Binary Vector of length NumSub
  
  # Write a code to randomly allot the subjects either on control or treatment arm.
  
  # Repeated Measures Dropout Output Heirarchy
  # Step 1: If user has returned Censor Indicator arrays CensorInd1, CensorInd … CensorInd<NumVisit> from their R code, 
  # then no other outputs are required. In that case, all other outputs become optional and the workflow ends here. 
  # If user has not returned Censor Indicator arrays from their R code, please go to Step 2.
  # 
  # Step 2: If user has returned DropoutVisitID from their R code, then no other outputs are required. 
  # In that case, all other outputs become optional and the workflow ends here.  
  # If user has not returned DropoutVisitID from their R code, please go to Step 3.
  # 
  # Step 3: If user has returned DropOutTime from their R code, then simulations run successfully and 
  # all other outputs becomes optional. no other outputs are required. If user has not returned DropOutTime from their R code, 
  # then the application will return an error code. The workflow ends here. 
  
  
  return( list( CensorInd = as.double( vCensorInd ), ErrorCode = as.integer( nError ) ) )
}