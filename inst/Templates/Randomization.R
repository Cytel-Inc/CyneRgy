######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Randomize Subjects to Treatment Arms
#' @description Generate treatment assignments using allocation ratios relative to the control arm.
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArms Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param AllocRatio Positive numeric scalar giving the experimental-to-control allocation ratio for a two-arm design, or a numeric vector of experimental-to-control allocation ratios with length `NumArms - 1` for a multiple-arm design.
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#'
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
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArms, AllocRatio, UserParam = NULL )
{

    nError                        <- 0

    # Allocation ratio on control and treatment arm
    vAllocRatio                   <- c( 1, AllocRatio )

    # Convert the Allocation Ratio to Allocation Fraction for control and treatment arms
    dAllocFraction                <- c( vAllocRatio[ 1 ] / sum( vAllocRatio ), 1 - vAllocRatio[ 1 ] / sum( vAllocRatio ) )
    vTreatmentIDs                 <- sample( 0:( NumArms - 1 ), NumSub, prob = dAllocFraction, replace = TRUE )

    return( list( TreatmentID = as.integer( vTreatmentIDs ), ErrorCode = as.integer( nError ) ) )
}
