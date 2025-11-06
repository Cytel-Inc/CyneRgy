# Check and verify your code before running.
# Run the code to see the results.
# Extract the function before saving or uploading to East Horizon.
# Visit this help page for more information: https://cytel-inc.github.io/CyneRgy/articles/IntegrationPointAnalysisBinaryTwoArm.html

PerformBayesianAnalysis <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL) {
    # Initialize variables
    
    library( CyneRgy )
    
    nError <- 0
    nDecision <- 0
    dTestStatistic <- 0
    
    # Step 1: Determine the current look and number of patients in analysis
    if (!is.null(LookInfo)) {
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfLooks <- LookInfo$NumLooks
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[nLookIndex]
    } else {
        nLookIndex <- 1
        nQtyOfLooks <- 1
        nQtyOfPatsInAnalysis <- nrow(SimData)
    }
    
    # Step 2: Extract patient outcomes and treatment assignments
    vPatientOutcome <- SimData$Response[1:nQtyOfPatsInAnalysis]
    vPatientTreatment <- SimData$TreatmentID[1:nQtyOfPatsInAnalysis]
    
    # Separate outcomes by treatment group
    vOutcomesCtrl <- vPatientOutcome[vPatientTreatment == 0]
    vOutcomesExp <- vPatientOutcome[vPatientTreatment == 1]
    
    # Step 3: Perform Bayesian analysis for posterior probability
    lPosterior <- ProbExpGreaterCtrlBeta(
        vOutcomesCtrl, vOutcomesExp,
        UserParam$dAlphaCtrl, UserParam$dBetaCtrl,
        UserParam$dAlphaExp, UserParam$dBetaExp
    )
    
    # Check efficacy boundary
    strDecision <- "INVALID"
    dPredProb   <- NA
    if( nLookIndex < nQtyOfLooks )
    {
        # Interim analysis
        if (lPosterior$dPostProb > UserParam$PU) 
        {
            strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, bIAEfficacyCondition = TRUE )
            nDecision   <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo  )  # Stop for success
        } 
        else 
        {
            nFinalNExp <- DesignParam$MaxCompleters *( DesignParam$AllocInfo[1]/( DesignParam$AllocInfo[1]+ 1) )
            nFinalNCtrl  <-  DesignParam$MaxCompleters - nFinalNExp
            # Step 4: Compute Bayesian predictive probability for futility
            if( UserParam$FutilityCheck == 0 )
            {
                lPredictive <- list(predictiveProbabilityS= 1) # Skip futility check
            }
            else
            {
                lPredictive <- ComputeBayesianPredictiveProbabilityWithBayesianAnalysis(
                    vOutcomesCtrl, vOutcomesExp,
                    UserParam$dAlphaCtrl, UserParam$dBetaCtrl,
                    UserParam$dAlphaExp, UserParam$dBetaExp,
                    nFinalNCtrl,
                    nFinalNExp,
                    UserParam$nSimulations, UserParam$PUFinal )
            }
            
            # Check futility boundary
            dPredProb <- lPredictive$predictiveProbabilityS
            if (dPredProb < UserParam$PL) {
                # Stop for futility
                strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, bIAFutilityCondition = TRUE )
                nDecision   <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo  )  # Stop for success
                
            }
            else
            {
                strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, bIAFutilityCondition = FALSE )
            }
        }
    }
    else
    {
        if (lPosterior$dPostProb > UserParam$PUFinal) 
        {
            #Success at FA
            strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, bFAEfficacyCondition = TRUE )
            nDecision   <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo  )  # Success at FA
        }
        else
        {
            #Futility at FA
            strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, bFAFutilityCondition = TRUE )
            nDecision   <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo  )  # Success at FA
        }
    }
    
    # Return results
    return(list( strDecision = strDecision,
                 dPredProb = as.double( dPredProb ),
        TestStat = as.double(lPosterior$dPostProb),
        Decision = as.integer(nDecision),
        ErrorCode = as.integer(nError)
    ))
}

# Function to compute posterior probability using Beta-Binomial model
ProbExpGreaterCtrlBeta <- function(vOutcomesCtrl, vOutcomesExp, dAlphaCtrl, dBetaCtrl, dAlphaExp, dBetaExp) {
    # Update posterior parameters for control group
    # For analysis hard coding this for conference only want prior data in the prediciton not analysis
    
    dAlphaCtrl <- 0.2 + sum(vOutcomesCtrl) # dAlphaCtrl
    dBetaCtrl <- 0.8  + length(vOutcomesCtrl) - sum(vOutcomesCtrl) # dBetaCtrl
    
    # Update posterior parameters for experimental group
    dAlphaExp <- dAlphaExp + sum(vOutcomesExp)
    dBetaExp <- dBetaExp + length(vOutcomesExp) - sum(vOutcomesExp)
    
    # Sample posterior distributions
    vPiCtrl <- rbeta(10000, dAlphaCtrl, dBetaCtrl)
    vPiExp <- rbeta(10000, dAlphaExp, dBetaExp)
    
    # Compute posterior probability
    dPostProb <- mean(vPiExp > vPiCtrl)
    
    return(list(dPostProb = dPostProb))
}

# Function to compute Bayesian predictive probability of success
ComputeBayesianPredictiveProbabilityWithBayesianAnalysis <- function(
        dataCtrl, dataExp, priorAlphaCtrl, priorBetaCtrl, priorAlphaExp, priorBetaExp,
        nFinalCtrl, nFinalExp, nSimulations, finalBoundary
) {
    # Update posterior parameters based on observed data
     
    posteriorAlphaCtrl <- priorAlphaCtrl + sum(dataCtrl)
    posteriorBetaCtrl <- priorBetaCtrl + length(dataCtrl) - sum(dataCtrl)
    
    
    posteriorAlphaExp <- priorAlphaExp + sum(dataExp)
    posteriorBetaExp <- priorBetaExp + length(dataExp) - sum(dataExp)
    
    # Initialize counter for successful trials
    successfulTrials <- 0
    
    # Simulate remaining trials and compute predictive probability
    for (i in 1:nSimulations) {
        # Sample response rates from posterior distributions
        posteriorRateCtrl <- rbeta(1, posteriorAlphaCtrl, posteriorBetaCtrl)
        posteriorRateExp <- rbeta(1, posteriorAlphaExp, posteriorBetaExp)
        
        # Simulate outcomes for remaining patients
        remainingCtrl <- rbinom(nFinalCtrl - length(dataCtrl), 1, posteriorRateCtrl)
        remainingExp <- rbinom(nFinalExp - length(dataExp), 1, posteriorRateExp)
        
        # Combine observed and simulated data
        combinedCtrl <- c(dataCtrl, remainingCtrl)
        combinedExp <- c(dataExp, remainingExp)
        
        # Perform Bayesian analysis on combined data
        lResult <- ProbExpGreaterCtrlBeta(
            combinedCtrl, combinedExp,
            priorAlphaCtrl, priorBetaCtrl,
            priorAlphaExp, priorBetaExp
        )
        
        # Check if trial meets success criteria
        if (lResult$dPostProb > finalBoundary) {
            successfulTrials <- successfulTrials + 1
        }
    }
    
    # Compute predictive probability of success
    predictiveProbabilityS <- successfulTrials / nSimulations
    
    return(list(predictiveProbabilityS = predictiveProbabilityS))
}
