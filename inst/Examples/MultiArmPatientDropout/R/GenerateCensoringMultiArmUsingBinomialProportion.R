######################################################################################################################## .
#' @name GenerateCensoringMultiArmUsingBinomialProportion
#' @title Generate Multi-Arm Censoring Indicators
#' @description Generates a censoring indicator for each subject using the dropout probability for the subject's arm.
#' @author Anoop Singh Rawat
#' @param NumSub Integer number of subjects in the trial.
#' @param ProbDrop Required numeric vector of length `NumArm`, specifying the dropout probability for each arm.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#' @return A list containing:
#'   \describe{
#'     \item{CensorInd}{Required integer vector of length `NumSub`; 0 indicates dropout or non-completion and 1 indicates completion.}
#'     \item{ErrorCode}{Optional integer status code; 0 indicates no error, a positive value aborts the current simulation but allows subsequent simulations, and a negative value stops further simulation.}
#'   }
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
