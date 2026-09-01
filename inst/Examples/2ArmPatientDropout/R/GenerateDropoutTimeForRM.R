######################################################################################################################## .
#' @name GenerateDropoutTimeForRM
#' @title Generate Dropout Times for Repeated Measures
#' @author Shubham Lahoti
#' @description This function generates dropout time for a Repeated Measures design with Dropout method on East Horizon as 'Cumulative Probability of Dropout by Time'.
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
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'                    User should access the variables using names, for example UserParam$Var1 and not order.
#'                    These variables can be of the following types: Integer, Numeric, or Character
#'
#' @return A named list containing numeric vector `DropOutTime` and integer `ErrorCode`.
######################################################################################################################## .
GenerateDropoutTimeForRM <- function( NumSub, NumArm, NumVisit, VisitTime, TreatmentID, DropMethod, ByTime, DropParamControl, DropParamTrt, UserParam = NULL )
{
    nError                        <- 0
    # Initializing Censor Dropout Times to Inf
    # This effectively means that all the patients have dropped out at an infinite time,
    # i.e., effectively they haven't dropped out at all, meaning that they all are completers
    # We modify this vector later
    vDropoutTime                   <- rep( Inf, NumSub )

    #Identify the patients from Control and Experimental arm
    vIndexControl              <- which( TreatmentID == 0 )
    vIndexExperiment           <- which( TreatmentID == 1 )

    nQtyOfPatientOnControl     <- length( vIndexControl )
    nQtyOfPatientsOnExperiment <- length( vIndexExperiment )

    if( DropMethod == 2 )    # Cumulative Probability of Dropout by Time
    {
        # Generate a random sample from Exponential distribution using control and experiment rate parameter. These are the dropout times.

        if( DropParamControl > 0 )  # generate dropout time only in case of Non - zero dropout probability
        {
            dExpDropoutControlRate           <-  -log( 1 - DropParamControl ) / ByTime

            vDropoutTime[ vIndexControl ]    <- rexp( nQtyOfPatientOnControl, rate = dExpDropoutControlRate )
        }
        if( DropParamTrt > 0 )             # generate dropout time only in case of Non - zero dropout probability
        {
            dExpDropoutExperimentRate        <-  -log( 1 - DropParamTrt ) / ByTime

            vDropoutTime[ vIndexExperiment ] <- rexp( nQtyOfPatientsOnExperiment, rate = dExpDropoutExperimentRate )
        }
    }

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

    return( list( DropOutTime = as.double( vDropoutTime ), ErrorCode = as.integer( nError ) ) )
}
