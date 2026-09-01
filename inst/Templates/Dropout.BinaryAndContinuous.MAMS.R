######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Generate Multi-Arm Binary or Continuous Dropout Indicators
#' @description Generate subject-level completion indicators using arm-specific dropout probabilities.
#' @param NumSub Integer number of subjects in the trial.
#' @param ProbDrop Mandatory. A vector of numeric values specifying probability of dropout for each arm
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'                    User should access the variables using names, for example UserParam$Var1 and not order.
#'                    These variables can be of the following types: Integer, Numeric, or Character
#'
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{CensorInd}{Mandatory. A Binary vector of length NumSub such that
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

{{FUNCTION_NAME}} <- function( NumSub, ProbDrop, NumArm, TreatmentID, UserParam = NULL )
{

    nError                <- 0

    vCensoringIndicator <- numeric( NumSub )
    for( i in 1:NumSub )
    {
        # Get the arm index (adjusting for 0-based indexing in TreatmentID)
        nArmIndex <- TreatmentID[ i ] + 1

        # Generate dropout indicator based on the arm-specific probability
        # 1 - ProbDrop[armIndex] gives the probability of completion (not dropping out)
        vCensoringIndicator[ i ] <- rbinom( n = 1, size = 1, prob = 1 - ProbDrop[ nArmIndex ] )
    }

    return( list( CensorInd = as.integer( vCensoringIndicator ), ErrorCode = as.integer( nError ) ) )
}
