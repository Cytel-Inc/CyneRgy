######################################################################################################################## .
#' @name GenerateDropoutTimeForSurvival
#' @title Generate subject dropout times for survival outcomes
#' @description The following function generates dropout time for 2-arm survival design.
#' @author Shubham Lahoti
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#' @param DropMethod Integer dropout input method: 1 = dropout hazard rates; 2 = cumulative probability of dropout
#'   by time.
#' @param NumPrd Integer number of dropout periods. Equals 1 when DropMethod = 2.
#' @param PrdTime Numeric vector of length NumPrd specifying dropout-period starting times when DropMethod = 1, or
#'   the times at which cumulative dropout probabilities are specified when DropMethod = 2.
#' @param DropParam Numeric array of dropout parameters with NumPrd rows and NumArm columns. Column 1 is control;
#'   subsequent columns are experimental arms. DropParam[i, j] is the dropout hazard rate for period i and arm j
#'   when DropMethod = 1, or the cumulative probability of dropout by PrdTime[i] when DropMethod = 2.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{DropOutTime}{Numeric vector of generated dropout times measured from each subject's enrollment, with one
#'     element per subject. Inf indicates no dropout.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details The example code implements two arms (NumArm = 2) and a single dropout period (NumPrd = 1). For multiple hazard periods, extend
#'   the generation logic to use the appropriate row of DropParam for each period. Unsupported configurations
#'   return ErrorCode = -1 rather than silently using the wrong dropout parameters.
######################################################################################################################## .

GenerateDropoutTimeForSurvival <- function( NumSub, NumArm, TreatmentID, DropMethod,
                                            NumPrd, PrdTime, DropParam, UserParam = NULL ) {
    nErrorCode <- 0

    # Initializing Censor Dropout Times to Inf
    # This effectively means that all the patients have dropped out at an infinite time,
    # i.e., effectively they haven't dropped out at all, meaning that they all are completers

    vDropoutTime <- rep( Inf, NumSub )

    if ( NumArm != 2 || NumPrd != 1 || !( DropMethod %in% c( 1, 2 ) ) ) {
        return( list( DropOutTime = vDropoutTime, ErrorCode = -1L ) )
    }

    # Identify the patients from Control and Experimental arm
    vIndexControl <- which( TreatmentID == 0 )
    vIndexExperiment <- which( TreatmentID == 1 )

    nQtyOfPatientsOnControl <- length( vIndexControl )
    nQtyOfPatientsOnExperiment <- length( vIndexExperiment )

    if ( DropMethod == 1 ) { # Dropout Hazard Rates
        # Generate a random sample from Exponential distribution using control and experiment rate parameter. These are
        #   the dropout times.

        if ( DropParam[ 1, 1 ] > 0 ) { # generate dropout time only in case of Non - zero dropout probability
            vDropoutTime[ vIndexControl ] <- stats::rexp( nQtyOfPatientsOnControl, rate = DropParam[ 1, 1 ] )
        }
        if ( DropParam[ 1, 2 ] > 0 ) { # generate dropout time only in case of Non - zero dropout probability
            vDropoutTime[ vIndexExperiment ] <- stats::rexp( nQtyOfPatientsOnExperiment, rate = DropParam[ 1, 2 ] )
        }
    }

    if ( DropMethod == 2 ) { # Probability of Dropout
        # Conversion of dropout probabilities into Hazard rates

        dExpDropoutControlRate <- -log( 1 - DropParam[ 1, 1 ] ) / PrdTime
        dExpDropoutExperimentRate <- -log( 1 - DropParam[ 1, 2 ] ) / PrdTime

        # Generate a random sample from Exponential distribution using control and experiment rate parameter. These are
        #   the dropout times.

        if ( DropParam[ 1, 1 ] > 0 ) { # generate dropout time only in case of Non - zero dropout probability
            vDropoutTime[ vIndexControl ] <- stats::rexp( nQtyOfPatientsOnControl, rate = dExpDropoutControlRate )
        }
        if ( DropParam[ 1, 2 ] > 0 ) { # generate dropout time only in case of Non - zero dropout probability
            vDropoutTime[ vIndexExperiment ] <- stats::rexp(
                nQtyOfPatientsOnExperiment, rate = dExpDropoutExperimentRate
            )
        }
    }

    return( list( DropOutTime = as.double( vDropoutTime ), ErrorCode = as.integer( nErrorCode ) ) )
}
