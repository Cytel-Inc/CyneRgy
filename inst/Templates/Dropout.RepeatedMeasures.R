######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Generate Repeated-Measures Dropout Data
#' @description Generate dropout times, dropout visits, or visit-specific censoring indicators for repeated measures.
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param DropMethod Input method for specifying dropout parameters.
#'           \describe{
#'           \item{Repeated Measures}{1 - Cumulative Probability of Dropout by Visit. 2 - Cumulative Probability of Dropout by Time}
#'           }
#' @param NumVisit Integer number of visits.
#' @param VisitTime Numeric vector of length `NumVisit`, indicating the visit times.
#' @param ByTime Numeric vector of length `NumVisit` when `DropMethod = 1`, or a numeric scalar when `DropMethod = 2`. For method 1, values equal `VisitTime`.
#' @param DropParamControl Control-arm dropout parameters: a numeric vector of length `NumVisit` when `DropMethod = 1`, or a numeric scalar when `DropMethod = 2`.
#' @param DropParamTrt Treatment-arm dropout parameters: a numeric vector of length `NumVisit` when `DropMethod = 1`, or a numeric scalar when `DropMethod = 2`.
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#'
#' @return A list containing one of the supported dropout representations and an optional status code:
#'   \describe{
#'     \item{CensorInd1, ..., CensorIndNumVisit}{Visit-specific integer vectors of length `NumSub`; 0 indicates dropout and 1 indicates completion. If supplied, no other dropout representation is required.}
#'     \item{DropoutVisitID}{Integer vector of length `NumSub` containing the 1-based visit after which each subject dropped out. If supplied, `DropOutTime` is optional.}
#'     \item{DropOutTime}{Numeric vector of length `NumSub` containing dropout times; `Inf` indicates no dropout.}
#'     \item{ErrorCode}{Optional integer status code; 0 indicates no error, a positive value aborts the current simulation but allows subsequent simulations, and a negative value stops further simulation.}
#'   }
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArm, NumVisit, VisitTime, TreatmentID,
                     DropMethod, ByTime, DropParamControl, DropParamTrt, UserParam = NULL )
{
  # TO DO : Modify this function appropriately
    nError    <- 0
    initval   <- c()
    retval    <- list()

    # Initializing CensorInd Arrays
    for( i in 1:NumVisit )
    {
        strCensorIndName <- paste0( "CensorInd", i )
        CensorInd        <- rep( 1, NumSub )
        retval[[ strCensorIndName ] ] <- as.integer( CensorInd )
    }

    # Initializing DropOutTime and DropoutVisitID to Inf
    # This effectively means that all the patients have dropped out at an infinite time,
    # i.e., effectively they haven't dropped out at all, meaning that they all are completers
    for( i in 1:NumSub )
    {
        initval[ i ] <- Inf
    }

    retval$DropoutVisitID <- as.integer( initval )
    retval$DropOutTime    <- as.double( rep( NumVisit, NumSub ) )

    # Use appropriate error handling and modify the
    # Error appropriately in each of the methods
    retval$ErrorCode <- as.integer( nError )

    # Repeated Measures Dropout Output Hierarchy
    # Step 1: If user has returned Censor Indicator arrays CensorInd1, CensorInd2, ..., CensorInd[NumVisit] from their R code,
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

    #retval is one of the options: 1) CensorID, 2) VisitID, 3) DropOutTime

    return( retval )
}
