######################################################################################################################## .
#' @title Compute Bayesian Predictive Probability of Success
#'
#' @description 
#' Function to compute the Bayesian predictive probability of success for a clinical trial using 
#' Bayesian analysis. The function simulates future patient outcomes based on posterior distributions 
#' derived from observed interim data and evaluates the probability of trial success at the end.
#'
#' @param dataS A vector of binary outcomes (0 or 1) for the control treatment observed at the interim analysis.
#' @param dataE A vector of binary outcomes (0 or 1) for the experimental treatment observed at the interim analysis.
#' @param priorAlphaS The alpha parameter of the Beta prior for the control treatment.
#' @param priorBetaS The beta parameter of the Beta prior for the control treatment.
#' @param priorAlphaE The alpha parameter of the Beta prior for the experimental treatment.
#' @param priorBetaE The beta parameter of the Beta prior for the experimental treatment.
#' @param nQtyOfPatsS The total number of patients for the control treatment expected by the end of the trial.
#' @param nQtyOfPatsE The total number of patients for the experimental treatment expected by the end of the trial.
#' @param nSimulations The number of virtual trials to simulate for predictive probability computation.
#' @param finalBoundary The cutoff threshold for posterior probability to determine trial success.
#' @param lAnalysisParams A list of analysis parameters for posterior computation, 
#' including priors for the control and experimental treatments.
#'
#' @details 
#' This function computes the Bayesian predictive probability of success for a clinical trial. 
#' It uses observed interim data to update the Beta priors into posterior distributions for success probabilities 
#' of both control and experimental treatments. Future patient outcomes are simulated based on these posteriors, 
#' and the trial success is evaluated based on the probability that the experimental treatment has a higher success rate 
#' than the control treatment. The predictive probability is calculated as the proportion of simulated trials 
#' meeting the success criteria.
#'
#' @return 
#' A list containing:
#' \item{predictiveProbabilityS}{The Bayesian predictive probability of trial success.}
#'
#' @export
######################################################################################################################## .

ComputeBayesianPredictiveProbabilityWithBayesianAnalysis <- function(dataS, dataE, priorAlphaS, priorBetaS, priorAlphaE, priorBetaE, nQtyOfPatsS, nQtyOfPatsE, nSimulations, finalBoundary, lAnalysisParams) {
    # Compute the posterior parameters based on observed data
    posteriorAlphaS <- priorAlphaS + sum(dataS)
    posteriorBetaS  <- priorBetaS + length(dataS) - sum(dataS)
    
    posteriorAlphaE <- priorAlphaE + sum(dataE)
    posteriorBetaE  <- priorBetaE + length(dataE) - sum(dataE)
    
    #lAnalysisParams <- list( dAlphaCtrl = priorAlphaS,
    #                         dBetaCtrl  = priorBetaS,
    #                         dAlphaExp  = priorAlphaE,
    #                         dBetaExp   = priorBetaE )
    
    # Initialize counters for successful trials
    successfulTrials <- 0
    
    # Simulate the remaining trials and compute the predictive probability
    for (i in 1:nSimulations) {
        # Sample response rates from posterior distributions
        posteriorRateS <- rbeta(1, posteriorAlphaS, posteriorBetaS)
        posteriorRateE <- rbeta(1, posteriorAlphaE, posteriorBetaE)
        
        # Simulate patient outcomes for for the current virtual trial based on sampled rates
        # The data at the end of the trial is a combination of the data at the interim, dataS, and the simulated data to the end of the trial, remainingDataS
        remainingDataS <- SimulatePatientOutcome(nQtyOfPatsS - length(dataS), posteriorRateS)
        combinedDataS  <- c(dataS, remainingDataS)
        
        remainingDataE <- SimulatePatientOutcome(nQtyOfPatsE - length(dataE), posteriorRateE)
        combinedDataE  <- c(dataE, remainingDataE)
        
        
        # Perform the analysis with combined data to check if the trial is successful
        result <- ProbExpGreaterCtrlBeta(combinedDataS, combinedDataE, lAnalysisParams )
        
        # Check if the result meets the cutoff for success
        if (result$dPostProb <= finalBoundary) {
            successfulTrials <- successfulTrials + 1
        }
    }
    
    # Compute the Bayesian predictive probability of success
    predictiveProbabilityS <- successfulTrials / nSimulations
    
    # Return the result
    return(list(predictiveProbabilityS = predictiveProbabilityS))
}
