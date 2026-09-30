######################################################################################################################## .
#' @name SimulatePatientOutcomeBinaryWithAssurance
#'
#' @title Simulate binary patient outcomes using a Beta distribution prior
#'
#' @description Generate patient outcomes for a binary response trial while incorporating uncertainty about the
#'   true response rates by sampling them from a Beta distribution prior.
#'
#' @author Gabriel Potvin, Valeria A. G. Mazzanti, J. Kyle Wathen
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArm Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param PropResp Numeric vector of response probabilities by arm, with the control arm first, followed by
#'   experimental arms in TreatmentID order. Each probability is between 0 and 1.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' If UserParam must be supplied, the list must contain the following named elements:
#' \describe{
#'    \item{UserParam$dParameter1Ctrl}{For control treament, the design prior parameter 1 in the Beta distribution
#'      }
#'    \item{UserParam$dParameter2Ctrl}{For control treament, the design prior parameter 2 in the Beta distribution
#'      }
#'    \item{UserParam$dParameter1Exp}{For experimental treament, the design prior parameter 1 in the Beta
#'      distribution }
#'    \item{UserParam$dParameter2Exp}{For experimental treament, the design prior parameter 2 in the Beta
#'      distribution }
#' }
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject. Required.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

SimulatePatientOutcomeBinaryWithAssurance <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL ) {
    # If the user did not specify the user parameters, but still called this function: error
    if ( is.null( UserParam ) ) {
        nErrorCode <- 100
        lReturn <- list( Response = as.double( rep( NA, NumSub ) ), ErrorCode = as.integer( nErrorCode ) )
        return( lReturn )
    }

    # Step 1: Sample true probability of response

    dTrueProbCtrl <- stats::rbeta( 1, UserParam$dParameter1Ctrl, UserParam$dParameter2Ctrl )
    dTrueProbExp <- stats::rbeta( 1, UserParam$dParameter1Exp, UserParam$dParameter2Exp )

    vTrueProb <- c( dTrueProbCtrl, dTrueProbExp )

    nErrorCode <- 0 # Code for no errors occurred
    vPatientOutcome <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    # Loop over the patients and simulate the outcome according to the treatment they
    for ( nPatIndx in 1:NumSub ) {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index
        vPatientOutcome[ nPatIndx ] <- stats::rbinom( 1, 1, vTrueProb[ nTreatmentID ] )
    }

    if ( any( is.na( vPatientOutcome ) == TRUE ) ) {
        nErrorCode <- -100
    }

    # True Probability of Responses have to be a vector of same length to number of subjects

    lReturn <- list(
        Response = as.double( vPatientOutcome ),
        ErrorCode = as.integer( nErrorCode ),
        TrueProbabilityControl = as.double( rep( dTrueProbCtrl, NumSub ) ),
        TrueProbabilityExperimental = as.double( rep( dTrueProbExp, NumSub ) )
    )
    return( lReturn )
}
