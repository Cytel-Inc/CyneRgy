

# Function for performing statistical analysis using a Beta-Binomial Bayesian model

ProbExpGreaterCtrlBeta <- function(vOutcomesS, vOutcomesE, dAlphaS, dBetaS, dAlphaE, dBetaE) 
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
    
    return(list(dPostProb = dPostProb))
}




# Function to compute Bayesian predictive probability of success
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



