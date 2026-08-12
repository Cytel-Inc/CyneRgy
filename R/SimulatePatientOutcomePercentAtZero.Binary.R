#################################################################################################### .
#   Program/Function Name: SimulatePatientOutcomePercentAtZero.Binary
#   Description: Simulate binary patient outcomes with optional treatment resistance.
#################################################################################################### .
#' @name SimulatePatientOutcomePercentAtZero.Binary
#' @title Simulate Binary Patient Outcomes
#'
#' @description Simulates a binary response for each subject. `UserParam` can define arm-specific probabilities that a subject is
#' treatment resistant and therefore always has response `0`. Without `UserParam`, outcomes are sampled directly from `PropResp`.
#'
#' @param NumSub Integer number of subjects.
#' @param NumArm Integer number of trial arms.
#' @param ArrivalTime Numeric subject arrival times. Retained for compatibility with the response integration point.
#' @param TreatmentID Integer treatment identifiers beginning at `0`.
#' @param PropResp Numeric response probability for each arm.
#' @param UserParam Optional list containing `dProbOfTreatmentResistantCtrl` and `dProbOfTreatmentResistantExp`.
#'
#' @return A list containing numeric `Response` and integer `ErrorCode`.
#'
#' @export
#################################################################################################### .

SimulatePatientOutcomePercentAtZero.Binary <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL )
{
    if( is.null( UserParam ) )
        UserParam <- list( dProbOfTreatmentResistantCtrl = 0, dProbOfTreatmentResistantExp = 0 )

    vProbabilityOfTreatmentResistant <- c( UserParam$dProbOfTreatmentResistantCtrl,
                                           UserParam$dProbOfTreatmentResistantExp )

    if( NumSub < 1 || NumArm != 2 || length( ArrivalTime ) != NumSub || length( TreatmentID ) != NumSub ||
        length( PropResp ) != NumArm || any( !TreatmentID %in% 0:( NumArm - 1 ) ) ||
        any( !is.finite( PropResp ) ) || any( PropResp < 0 | PropResp > 1 ) ||
        length( vProbabilityOfTreatmentResistant ) != NumArm ||
        any( !is.finite( vProbabilityOfTreatmentResistant ) ) )
        stop( "Inputs must describe NumSub subjects in two arms with probabilities between 0 and 1.", call. = FALSE )

    vProbabilityOfTreatmentResistant <- pmin( 1, pmax( 0, vProbabilityOfTreatmentResistant ) )
    vPatientOutcome                  <- rep( 0, NumSub )

    for( nPatIndx in 1:NumSub )
    {
        nTreatmentID     <- TreatmentID[ nPatIndx ] + 1
        bCanRespond      <- stats::rbinom( 1, 1, 1 - vProbabilityOfTreatmentResistant[ nTreatmentID ] )
        if( bCanRespond == 1 )
            vPatientOutcome[ nPatIndx ] <- stats::rbinom( 1, 1, PropResp[ nTreatmentID ] )
    }

    nError <- if( any( is.na( vPatientOutcome ) ) ) -100 else 0

    return( list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ) )
}
