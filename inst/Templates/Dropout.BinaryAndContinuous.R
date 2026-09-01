######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Generate Binary or Continuous Dropout Indicators
#' @description Generate subject-level completion indicators from a common dropout probability.
#' @param NumSub Integer number of subjects in the trial.
#' @param ProbDrop The numeric value specifying probability of dropout. Mandatory for Continuous and Binary Endpoints.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'                    User should access the variables using names, for example UserParam$Var1 and not order.
#'                    These variables can be of the following types: Integer, Numeric, or Character
#'
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{CensorInd}{Mandatory for Continuous and Binary Endpoints. A Binary vector of length NumSub such that
#'                                  \describe{
#'                                    \item{CensorInd = 0}{ Non Completer / Dropout}
#'                                    \item{CensorInd = 1}{ Completer}
#'                                    }
#'                                    }
#'                  \item{ErrorCode}{Optional integer value \describe{
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'                              }
#'
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, ProbDrop, UserParam = NULL )
{

    nError                <- 0

    vCensoringIndicator <- rbinom( n = NumSub, size = 1, prob = 1 - ProbDrop )

    return( list( CensorInd = as.integer( vCensoringIndicator ), ErrorCode = as.integer( nError ) ) )
}
