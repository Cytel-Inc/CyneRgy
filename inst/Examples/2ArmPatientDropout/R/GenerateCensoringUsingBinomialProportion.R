######################################################################################################################## .
#' @name GenerateCensoringUsingBinomialProportion
#' @title Generate dropout indicators for binary or continuous outcomes
#' @description Generate censoring indicator ( CensorInd ) for 2 arm designs with Normal and Binomial Endpoint
#'   using a single dropout probability.
#' @author Shubham Lahoti, J. Kyle Wathen
#' @param NumSub Integer number of subjects in the trial.
#' @param ProbDrop Numeric dropout probability for a two-arm design, shared by both arms; for a multi-arm design, a
#'   numeric vector of length NumArm with the control probability first and experimental-arm probabilities
#'   thereafter. Values must be between 0 and 1.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{CensorInd}{Integer vector of censor indicators, with one element per subject: 0 = dropout/non-completer;
#'     1 = completer.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details These binary/continuous dropout functions return CensorInd for standard binary and continuous outcomes.
######################################################################################################################## .

GenerateCensoringUsingBinomialProportion <- function( NumSub, ProbDrop, UserParam = NULL ) {
    nErrorCode <- 0

    vCensoringIndicator <- stats::rbinom( n = NumSub, size = 1, prob = 1 - ProbDrop )

    return( list( CensorInd = as.integer( vCensoringIndicator ), ErrorCode = as.integer( nErrorCode ) ) )
}
