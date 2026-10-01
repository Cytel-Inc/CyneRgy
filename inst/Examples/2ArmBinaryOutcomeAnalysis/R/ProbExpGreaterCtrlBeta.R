######################################################################################################################## .
#' @name ProbExpGreaterCtrlBeta
#' @title Compute the posterior probability of a higher experimental response rate
#' @description Estimate the posterior probability that the experimental response probability exceeds the control
#'   response probability by sampling from the two beta posterior distributions.
#' @author J. Kyle Wathen and Gabriel Potvin
#' @param vOutcomesS Integer vector of observed control-arm binary outcomes (0 = non-response, 1 = response).
#' @param vOutcomesE Integer vector of observed experimental-arm binary outcomes (0 = non-response, 1 = response).
#' @param dAlphaS Positive numeric alpha parameter of the control-arm beta prior.
#' @param dBetaS Positive numeric beta parameter of the control-arm beta prior.
#' @param dAlphaE Positive numeric alpha parameter of the experimental-arm beta prior.
#' @param dBetaE Positive numeric beta parameter of the experimental-arm beta prior.
#' @return List containing dPostProb, the estimated posterior probability that the experimental response
#'   probability exceeds the control response probability, and dDelta, the experimental-minus-control
#'   posterior mean response-probability difference.
######################################################################################################################## .

ProbExpGreaterCtrlBeta <- function( vOutcomesS, vOutcomesE, dAlphaS, dBetaS, dAlphaE, dBetaE ) {
    # In the beta-binomial model if we make the assumption that
    # pi ~ Beta( a, b )
    # then the posterior of pi is:
    # pi | data ~ Beta( a + # success, b + # non-successes )

    # Compute the posterior parameters for control treatment
    dAlphaS <- dAlphaS + sum( vOutcomesS )
    dBetaS <- dBetaS + length( vOutcomesS ) - sum( vOutcomesS )

    # Compute the posterior parameters for Exp treatment
    dAlphaE <- dAlphaE + sum( vOutcomesE )
    dBetaE <- dBetaE + length( vOutcomesE ) - sum( vOutcomesE )

    # There are much more efficient ways to compute this, but for simplicity, we are just sampling the posteriors
    vPiCtrl <- stats::rbeta( 10000, dAlphaS, dBetaS )
    vPiExp <- stats::rbeta( 10000, dAlphaE, dBetaE )
    dPostProb <- ifelse( vPiExp > vPiCtrl, 1, 0 )
    dPostProb <- sum( dPostProb ) / length( dPostProb )

    dDelta <- dAlphaE / ( dAlphaE + dBetaE ) - dAlphaS / ( dAlphaS + dBetaS )
    return( list( dPostProb = dPostProb, dDelta = dDelta ) )
}

######################################################################################################################## .
#' @name ComputeBayesianPredictiveProbabilityWithBayesianAnalysis
#' @title Compute the Bayesian predictive probability of trial success
#' @description Sample response probabilities from the current beta posteriors, simulate the remaining
#'   binary outcomes, and estimate the probability that the final Bayesian analysis meets its success cutoff.
#' @author J. Kyle Wathen and Gabriel Potvin
#' @param dataS Integer vector of observed control-arm binary outcomes (0 = non-response, 1 = response).
#' @param dataE Integer vector of observed experimental-arm binary outcomes (0 = non-response, 1 = response).
#' @param priorAlphaS Positive numeric alpha parameter of the control-arm beta prior used for prediction.
#' @param priorBetaS Positive numeric beta parameter of the control-arm beta prior used for prediction.
#' @param priorAlphaE Positive numeric alpha parameter of the experimental-arm beta prior used for prediction.
#' @param priorBetaE Positive numeric beta parameter of the experimental-arm beta prior used for prediction.
#' @param nQtyOfPatsS Integer planned final number of control subjects, including subjects already observed.
#' @param nQtyOfPatsE Integer planned final number of experimental subjects, including subjects already observed.
#' @param nSimulations Positive integer number of simulated future trials.
#' @param finalBoundary Numeric posterior-probability cutoff for final success, between 0 and 1.
#' @param lAnalysisParams Named list of positive numeric beta-prior parameters for the final analysis:
#'   dAlphaCtrl, dBetaCtrl, dAlphaExp, and dBetaExp.
#' @return Named list containing predictiveProbabilityS, the estimated predictive probability of final success.
######################################################################################################################## .

ComputeBayesianPredictiveProbabilityWithBayesianAnalysis <- function( dataS, dataE, priorAlphaS, priorBetaS,
                                                                      priorAlphaE, priorBetaE, nQtyOfPatsS,
                                                                      nQtyOfPatsE, nSimulations, finalBoundary,
                                                                      lAnalysisParams ) {
    # Compute the posterior parameters based on observed data
    dPosteriorAlphaS <- priorAlphaS + sum( dataS )
    dPosteriorBetaS <- priorBetaS + length( dataS ) - sum( dataS )

    dPosteriorAlphaE <- priorAlphaE + sum( dataE )
    dPosteriorBetaE <- priorBetaE + length( dataE ) - sum( dataE )

    # Initialize counters for successful trials
    nSuccessfulTrials <- 0

    # Simulate the remaining trials and compute the predictive probability
    for ( iSimulation in seq_len( nSimulations ) ) {
        # Sample response rates from posterior distributions
        dPosteriorRateS <- stats::rbeta( 1, dPosteriorAlphaS, dPosteriorBetaS )
        dPosteriorRateE <- stats::rbeta( 1, dPosteriorAlphaE, dPosteriorBetaE )

        # Simulate patient outcomes for the current virtual trial based on sampled rates
        # Combine the observed interim data with outcomes simulated for the remaining subjects.
        vRemainingDataS <- stats::rbinom( nQtyOfPatsS - length( dataS ), size = 1, prob = dPosteriorRateS )
        vCombinedDataS <- c( dataS, vRemainingDataS )

        vRemainingDataE <- stats::rbinom( nQtyOfPatsE - length( dataE ), size = 1, prob = dPosteriorRateE )
        vCombinedDataE <- c( dataE, vRemainingDataE )

        # Perform the analysis with combined data to check if the trial is successful
        lResult <- ProbExpGreaterCtrlBeta(
            vCombinedDataS, vCombinedDataE,
            lAnalysisParams$dAlphaCtrl, lAnalysisParams$dBetaCtrl,
            lAnalysisParams$dAlphaExp, lAnalysisParams$dBetaExp
        )

        # Check whether the final analysis meets the success cutoff
        if ( lResult$dPostProb >= finalBoundary ) {
            nSuccessfulTrials <- nSuccessfulTrials + 1
        }
    }

    # Compute the Bayesian predictive probability of success
    dPredictiveProbability <- nSuccessfulTrials / nSimulations

    # Return the predictive probability
    return( list( predictiveProbabilityS = dPredictiveProbability ) )
}
