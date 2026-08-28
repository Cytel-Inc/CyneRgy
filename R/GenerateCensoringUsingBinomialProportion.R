#################################################################################################### .
#   Program/Function Name: GenerateCensoringUsingBinomialProportion
#   Description: Generate independent dropout indicators.
#################################################################################################### .
#' @name GenerateCensoringUsingBinomialProportion
#' @title Generate Dropout Indicators
#'
#' @description Generates an independent censoring indicator for each subject using one dropout probability. A value of `1`
#' indicates a completer and `0` indicates a dropout.
#'
#' @param NumSub Integer number of subjects.
#' @param ProbDrop Numeric dropout probability shared by both arms.
#' @param UserParam Optional list of user-defined parameters. Retained for compatibility with the dropout integration point.
#'
#' @return A list containing integer vectors `CensorInd` and `ErrorCode`. `ErrorCode` is `0` on success.
#'
#' @export
#################################################################################################### .

GenerateCensoringUsingBinomialProportion <- function( NumSub, ProbDrop, UserParam = NULL )
{
    if( NumSub < 1 || length( ProbDrop ) != 1 || !is.finite( ProbDrop ) || ProbDrop < 0 || ProbDrop > 1 )
        stop( "NumSub must be positive and ProbDrop must be between 0 and 1.", call. = FALSE )

    Error               <- 0
    vCensoringIndicator <- stats::rbinom( n = NumSub, size = 1, prob = 1 - ProbDrop )

    return( list( CensorInd = as.integer( vCensoringIndicator ), ErrorCode = as.integer( Error ) ) )
}
