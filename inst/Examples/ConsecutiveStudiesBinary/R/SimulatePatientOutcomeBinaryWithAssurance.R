######################################################################################################################## .
#' @name SimulatePatientOutcomeBinaryWithAssurance
#' @title Simulate binary patient outcomes using a Beta distribution prior
#' @author Gabriel Potvin, Valeria A. G. Mazzanti, J. Kyle Wathen
#'
#' @description Generate patient outcomes for a binary response trial while incorporating uncertainty about the true
#' response rates by sampling them from a Beta distribution prior.
#'
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param PropResp Numeric vector of length `NumArm`, containing response probabilities for control followed by each experimental arm.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' If UserParam must be supplied, the list must contain the following named elements:
#' \describe{
#'    \item{UserParam$dParameter1Ctrl}{For control treament, the design prior parameter 1 in the Beta distribution }
#'    \item{UserParam$dParameter2Ctrl}{For control treament, the design prior parameter 2 in the Beta distribution }
#'    \item{UserParam$dParameter1Exp}{For experimental treament, the design prior parameter 1 in the Beta distribution }
#'    \item{UserParam$dParameter2Exp}{For experimental treament, the design prior parameter 2 in the Beta distribution }
#' }
#' @return A named list containing `Response`, `ErrorCode`, and subject-level vectors for the sampled control and
#' experimental response probabilities.
######################################################################################################################## .

SimulatePatientOutcomeBinaryWithAssurance <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL )
{

    # If the user did not specify the user parameters, but still called this function: error
    if( is.null( UserParam ) )
    {
        nError <- 100
        lReturn <- list( Response = as.double( rep( NA, NumSub ) ), ErrorCode = as.integer( nError ) )
        return( lReturn )
    }

    # Step 1: Sample true probability of response

    dTrueProbCtrl <- rbeta( 1, UserParam$dParameter1Ctrl, UserParam$dParameter2Ctrl )
    dTrueProbExp  <- rbeta( 1, UserParam$dParameter1Exp, UserParam$dParameter2Exp )

    vTrueProb <- c( dTrueProbCtrl, dTrueProbExp )

    nError           <- 0 # Code for no errors occurred
    vPatientOutcome  <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    # Loop over the patients and simulate the outcome according to the treatment they
    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID                <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index
        vPatientOutcome[ nPatIndx ] <- rbinom( 1, 1, vTrueProb[ nTreatmentID ] )
    }

    if( any( is.na( vPatientOutcome ) == TRUE ) )
        nError <- -100

    # True Probability of Responses have to be a vector of same length to number of subjects

    lReturn <- list( Response = as.double( vPatientOutcome ),
                     ErrorCode = as.integer( nError ),
                     TrueProbabilityControl = as.double( rep( dTrueProbCtrl, NumSub ) ),
                     TrueProbabilityExperimental = as.double( rep( dTrueProbExp, NumSub ) ) )
    return( lReturn )
}
