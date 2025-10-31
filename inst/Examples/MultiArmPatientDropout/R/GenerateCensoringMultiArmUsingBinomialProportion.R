#' @name GenerateCensoringMultiArmUsingBinomialProportion
#' @author Anoop Singh Rawat
#' @description Generate censoring indicator ( CensorInd ) for multi arm designs with Normal and Binomial Endpoint armwise dropout probability.  
#'               
#' @param NumSub Mandatory. The integer value specifying the number of patients or subjects in the trial. The numeric value of the argument value is sent in when called.
#' @param ProbDrop Mandatory. A vector of numeric values specifying probability of dropout for each arm. Length of vector = Number of arms
#' @param NumArm Mandatory. The integer value specifying the number of arms (including Control) in the trial.
#' @param TreatmentID Mandatory. A vector of length NumSub specifying indexes of arms to which subjects are allocated (one arm index per subject). Index for control is 0.
#' @param UserParam : User can pass custom scalar variables defined by users as a member of this list. 
#'                    User should access the variables using names, for example UserParam$Var1 and not order. 
#'                    These variables can be of the following types: Integer, Numeric, or Character

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
GenerateCensoringMultiArmUsingBinomialProportion <- function( NumSub, ProbDrop, NumArm, TreatmentID, UserParam = NULL ) 
{   

    nError 	            <- 0
    
    vCensoringIndicator <- numeric( NumSub )
    
    for (i in 1:NumSub)
    {
        # Get the arm index since TreatmentID uses 0 for control, 1, 2, ... for other arms
        nArmIndex <- TreatmentID[ i ] + 1
        
        # Generate dropout indicator based on the arm-specific probability
        # 1 - ProbDrop[armIndex] gives the probability of completion (not dropping out)
        vCensoringIndicator[ i ] <- rbinom( n = 1, size = 1, prob = 1 - ProbDrop[ nArmIndex ])
    }
    
    return( list( CensorInd = as.integer( vCensoringIndicator ), ErrorCode = as.integer( nError )))
}
