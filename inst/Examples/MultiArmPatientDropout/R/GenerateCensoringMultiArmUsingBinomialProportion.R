######################################################################################################################## .
#' @name GenerateCensoringMultiArmUsingBinomialProportion
#' @title Generate Multi-Arm Censoring Indicators
#' @description Generates a censoring indicator for each subject using the dropout probability for the subject's arm.
#' @author Gabriel Potvin and Anoop Singh Rawat
#' @param NumSub Integer. Number of subjects in the trial.
#' @param ProbDrop Numeric vector containing the dropout probability for each arm.
#' @param NumArm Integer. Number of arms in the trial, including control.
#' @param TreatmentID Integer vector of length `NumSub` containing arm indices, with 0 denoting control.
#' @param UserParam Optional list of user-defined parameters. This example does not use it. Defaults to `NULL`.
#' @return A list containing `CensorInd`, an integer vector of length `NumSub` where 0 denotes dropout and 1 denotes
#' completion, and `ErrorCode`, an integer status code where 0 indicates success.
######################################################################################################################## .

GenerateCensoringMultiArmUsingBinomialProportion <- function( NumSub, ProbDrop, NumArm, TreatmentID, UserParam = NULL )
{
    nError <- 0

    vCensoringIndicator <- numeric( NumSub )

    for( i in 1:NumSub )
    {
        # Get the arm index since TreatmentID uses 0 for control, 1, 2, ... for other arms
        nArmIndex <- TreatmentID[ i ] + 1

        # Generate dropout indicator based on the arm-specific probability
        # 1 - ProbDrop[armIndex] gives the probability of completion (not dropping out)
        vCensoringIndicator[ i ] <- rbinom( n = 1, size = 1, prob = 1 - ProbDrop[ nArmIndex ] )
    }

    return( list( CensorInd = as.integer( vCensoringIndicator ), ErrorCode = as.integer( nError ) ) )
}
