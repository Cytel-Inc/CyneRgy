######################################################################################################################## .
#' @name ProbExpGreaterCtrlBeta
#' @title Compute Posterior Probability of Experimental Treatment Being Greater than Control
#'
#' @description 
#' Function to perform statistical analysis using a Beta-Binomial Bayesian model. 
#' It computes the posterior probability that the success rate of an experimental treatment 
#' exceeds that of a control treatment, based on observed outcomes.
#'
#' @param vOutcomesS A vector of binary outcomes (0 or 1) for the control treatment.
#' @param vOutcomesE A vector of binary outcomes (0 or 1) for the experimental treatment.
#' @param dAlphaS The alpha parameter of the Beta prior for the control treatment.
#' @param dBetaS The beta parameter of the Beta prior for the control treatment.
#' @param dAlphaE The alpha parameter of the Beta prior for the experimental treatment.
#' @param dBetaE The beta parameter of the Beta prior for the experimental treatment.
#' 
#' @details 
#' In the Beta-Binomial model, it is assumed that the probability of success (\eqn{\pi}) follows a Beta distribution:
#' \eqn{\pi \sim Beta(\alpha, \beta)}. Given observed binary outcomes, the posterior distribution of \eqn{\pi} is:
#' \eqn{\pi | \text{data} \sim \text{Beta}(\alpha + \text{\# successes}, \beta + \text{\# non-successes})}. 
#' This function samples from the posterior distributions of the success probabilities for both control and experimental treatments, 
#' and calculates the posterior probability that the experimental treatment has a higher success rate than the control treatment.
#'
#' @return 
#' A list containing:
#' \item{dPostProb}{The posterior probability that the success rate of the experimental treatment 
#' is greater than that of the control treatment.}
#'
#' @export
######################################################################################################################## .

ProbExpGreaterCtrlBeta <- function( vOutcomesS, vOutcomesE, dAlphaS, dBetaS, dAlphaE, dBetaE ) 
{
    # In the beta-binomial model if we make the assumption that 
    # pi ~ Beta( a, b )
    # then the posterior of pi is:
    # pi | data ~ Beta( a + # success, b + # non-successes )
    
    # Compute the posterior parameters for control treatment 
    dAlphaS <- dAlphaS + sum( vOutcomesS )
    dBetaS  <- dBetaS  + length( vOutcomesS ) - sum( vOutcomesS )
    
    # Compute the posterior parameters for Exp treatment 
    dAlphaE  <- dAlphaE + sum( vOutcomesE )
    dBetaE   <- dBetaE  + length( vOutcomesE ) - sum( vOutcomesE )
    
    # There are much more efficient ways to compute this, but for simplicity, we are just sampling the posteriors
    vPiCtrl    <- rbeta( 10000, dAlphaS, dBetaS )
    vPiExp     <- rbeta( 10000, dAlphaE,  dBetaE  )
    dPostProb  <- ifelse( vPiExp > vPiCtrl, 1, 0 )
    dPostProb  <- sum( dPostProb )/length( dPostProb )
    
    return( list( dPostProb = dPostProb ) )
}
