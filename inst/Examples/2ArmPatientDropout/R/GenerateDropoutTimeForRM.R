######################################################################################################################## .
#' @name GenerateDropoutTimeForRM
#'
#' @title Generate dropout for repeated-measures outcomes
#'
#' @description This function generates dropout time for a Repeated Measures design with Dropout method on East
#'   Horizon as 'Cumulative Probability of Dropout by Time'.
#'
#' @author Shubham Lahoti
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArm Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#'
#' @param NumVisit Integer number of visits. The engine sets this to 1 when DropMethod = 2.
#'
#' @param VisitTime Numeric vector of visit times measured from enrollment, of length NumVisit and ordered by
#'   visit.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param DropMethod Integer dropout input method for repeated measures: 1 = cumulative probability of dropout by
#'   visit; 2 = cumulative probability of dropout by time.
#'
#' @param ByTime Numeric dropout assessment times. For DropMethod = 1, a vector of length NumVisit equal to
#'   VisitTime; for DropMethod = 2, a single numeric time.
#'
#' @param DropParamControl Control-arm cumulative dropout probabilities. For DropMethod = 1, a numeric vector of
#'   length NumVisit ordered by visit; for DropMethod = 2, a single numeric probability by ByTime.
#'
#' @param DropParamTrt Experimental-arm cumulative dropout probabilities. For DropMethod = 1, a numeric vector of
#'   length NumVisit ordered by visit; for DropMethod = 2, a single numeric probability by ByTime.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{CensorInd1, ..., CensorIndNumVisit}{Integer censor-indicator vectors, one per visit: 0 =
#'     dropout/non-completer; 1 = completer.}
#'   \item{DropOutTime}{Numeric vector of generated dropout times measured from each subject's enrollment, with one
#'     element per subject. Inf indicates no dropout.}
#'   \item{DropoutVisitID}{Integer vector of 1-based visit IDs after which subjects drop out, with one element per
#'     subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' @details Return at least one of the three supported dropout representations. East Horizon uses CensorInd1, ...,
#'   CensorIndNumVisit first, then DropoutVisitID if censor indicators are absent, then DropOutTime if neither of
#'   the other representations is returned. Inf dropout times indicate no dropout. This example implements only
#'   two-arm dropout by time (NumArm = 2 and DropMethod = 2); unsupported configurations return ErrorCode = -1.
######################################################################################################################## .

GenerateDropoutTimeForRM <- function( NumSub, NumArm, NumVisit, VisitTime, TreatmentID, DropMethod, ByTime, DropParamControl, DropParamTrt, UserParam = NULL ) {
    nErrorCode <- 0
    # Initializing Censor Dropout Times to Inf
    # This effectively means that all the patients have dropped out at an infinite time,
    # i.e., effectively they haven't dropped out at all, meaning that they all are completers
    # We modify this vector later
    vDropoutTime <- rep( Inf, NumSub )

    if ( NumArm != 2 || DropMethod != 2 ) {
        return( list( DropOutTime = vDropoutTime, ErrorCode = -1L ) )
    }

    # Identify the patients from Control and Experimental arm
    vIndexControl <- which( TreatmentID == 0 )
    vIndexExperiment <- which( TreatmentID == 1 )

    nQtyOfPatientOnControl <- length( vIndexControl )
    nQtyOfPatientsOnExperiment <- length( vIndexExperiment )

    if ( DropMethod == 2 ) { # Cumulative Probability of Dropout by Time
        # Generate a random sample from Exponential distribution using control and experiment rate parameter. These are the dropout times.

        if ( DropParamControl > 0 ) { # generate dropout time only in case of Non - zero dropout probability
            dExpDropoutControlRate <- -log( 1 - DropParamControl ) / ByTime

            vDropoutTime[ vIndexControl ] <- stats::rexp( nQtyOfPatientOnControl, rate = dExpDropoutControlRate )
        }
        if ( DropParamTrt > 0 ) { # generate dropout time only in case of Non - zero dropout probability
            dExpDropoutExperimentRate <- -log( 1 - DropParamTrt ) / ByTime

            vDropoutTime[ vIndexExperiment ] <- stats::rexp( nQtyOfPatientsOnExperiment, rate = dExpDropoutExperimentRate )
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

    return( list( DropOutTime = as.double( vDropoutTime ), ErrorCode = as.integer( nErrorCode ) ) )
}
