######################################################################################################################## .
#' @name GenerateCensoringUsingBinomialProportion
#' @title Generate Censoring Indicators from a Dropout Probability
#' @author Shubham Lahoti
#' @description Generate censoring indicator ( CensorInd ) for 2 arm designs with Normal and Binomial Endpoint using a single dropout probability.
#'
#' @param NumSub Integer number of subjects in the trial.
#' @param ProbDrop Required numeric scalar specifying the dropout probability applied to all arms.
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#'
#' @return A list containing:
#'   \describe{
#'     \item{CensorInd}{Required integer vector of length `NumSub`; 0 indicates dropout or non-completion and 1 indicates completion.}
#'     \item{ErrorCode}{Optional integer status code; 0 indicates no error, a positive value aborts the current simulation but allows subsequent simulations, and a negative value stops further simulation.}
#'   }
######################################################################################################################## .
GenerateCensoringUsingBinomialProportion <- function( NumSub, ProbDrop, UserParam = NULL )
{

    nError              <- 0

    vCensoringIndicator <- rbinom( n = NumSub, size = 1, prob = 1 - ProbDrop )

    return( list( CensorInd = as.integer( vCensoringIndicator ), ErrorCode = as.integer( nError ) ) )
}
