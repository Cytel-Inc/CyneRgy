# Check and verify your code before running.
# Run the code to see the results.
# Extract the function before saving or uploading to East Horizon.
# Visit this help page for more information: https://cytel-inc.github.io/CyneRgy/articles/IntegrationPointAnalysisBinaryTwoArm.html

RunAnalysis.HBayes <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL) {
    # Initialize variables
    
    library( CyneRgy )
    library( rjags )
    library( coda )
    
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
    
    # Hard coding the prior trial data because there is an issue loading data into to many simuaitons in EH
    dfPriorStudies <- data.frame( Y = c( 10, 33,   37, 11,  29,  38,  24,  19,  34, 16),
                                  N = c( 61, 154, 164, 65, 143, 215,  88, 121, 129, 91))
    

    
    # nTry <- 0
    # while( !exists( "dfPriorStudies") && nTry < 10 )
    # {
    #     tryCatch({       
    #         print(paste0( "Try ", nTry ))
    #         dfPriorStudies <<- readRDS(  "Inputs/PriorStudies.Rds")
    #         }, error=function(e){
    #             nTry <<- nTry + 1
    #             Sys.sleep( runif( 1, 1,5) )
    #         })
    # }
    
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
                lPredictive <- ComputeBayesianPredictiveProbabilityWithHierBayesianAnalysis(dfPriorStudies,
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
    dAlphaCtrl <- dAlphaCtrl + sum(vOutcomesCtrl)
    dBetaCtrl <- dBetaCtrl + length(vOutcomesCtrl) - sum(vOutcomesCtrl)
    
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
ComputeBayesianPredictiveProbabilityWithHierBayesianAnalysis <- function( dfPriorStudies,
        dataCtrl, dataExp, priorAlphaCtrl, priorBetaCtrl, priorAlphaExp, priorBetaExp,
        nFinalCtrl, nFinalExp, nSimulations, finalBoundary
) {
    # Update posterior parameters based on observed data
    nYCtrl <-  sum(dataCtrl)
    nNCtrl <- length(dataCtrl) 
    
    posteriorAlphaExp <- priorAlphaExp + sum(dataExp)
    posteriorBetaExp <- priorBetaExp + length(dataExp) - sum(dataExp)
    
    # Initialize counter for successful trials
    successfulTrials <- 0
    
    vY <- c( nYCtrl, dfPriorStudies$Y )
    vN <- c( nNCtrl, dfPriorStudies$N )
    
    lFit <- RunHierBinomJAGS( vY = vY,vN = vN, nIter = nSimulations)
    vPostRateCtrl <- lFit$vCurrentStudy
    
    # Simulate remaining trials and compute predictive probability
    for (i in 1:nSimulations) {
        # Sample response rates from posterior distributions
        posteriorRateCtrl <- vPostRateCtrl[ i ]
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



RunHierBinomJAGS <- function( vY, vN, nIter = 1000, nBurn = 3000, nChains =2, nThin = 2, nAdapt = 1000 ) {
    # vY: integer vector of responders per study
    # vN: integer vector of totals per study (same length as vY)
    # Returns: list with MCMC samples and a summary data frame
    
    if ( length( vY ) != length( vN ) ) {
        stop( "vY and vN must have equal length" )
    }
    if ( any( vY < 0 ) || any( vN <= 0 ) || any( vY > vN ) ) {
        stop( "Counts must satisfy 0 <= vY[i] <= vN[i], with vN[i] > 0" )
    }
    
    #set.seed( nSeed )
    suppressPackageStartupMessages( library( rjags ) )
    suppressPackageStartupMessages( library( coda ) )
    
    nStud <- length( vY )
    
    strModel <- "
  model {
    for (i in 1:Nstud) {
      y[i] ~ dbin(p[i], n[i])
      logit(p[i]) <- mu + u[i]
      u[i] ~ dnorm(0, prec)
    }
    mu  ~ dnorm(0, 1.0E-4) # prior mean 0.2
    tau ~ dunif(0, 5)
    tau2 <- tau * tau
    prec <- 1 / tau2
    pOverallLogitMean <- ilogit(mu)
  }"
    
    lData <- list(
        Nstud = nStud,
        y = as.integer( vY ),
        n = as.integer( vN )
    )
    
    MakeInits <- function() {
        # Initialize around empirical logit with small jitter
        dMuStart <- qlogis( ( sum( vY ) + 0.5 ) / ( sum( vN ) + 1.0 ) )
        lInit <- list(
            mu = rnorm( 1, dMuStart, 0.5 ),
            tau = runif( 1, 0.1, 1.0 ),
            u = rnorm( nStud, 0, 0.2 )
        )
        return( lInit )
    }
    lInits <- replicate( nChains, MakeInits(), simplify = FALSE )
    
    mJags <- jags.model(
        file = textConnection( strModel ),
        data = lData,
        inits = lInits,
        n.chains = nChains,
        n.adapt = nAdapt
    )
    
    update( mJags, n.iter = nBurn )
    
    vParams <- c( "mu", "tau", "pOverallLogitMean", "p" )
    mcmc <- coda.samples(
        model = mJags,
        variable.names = vParams,
        n.iter = nIter,
        thin = nThin
    )
    smry <- summary( mcmc )
    dfQuant <- as.data.frame( smry$quantiles )
    dfStats <- as.data.frame( smry$statistics )
    dfOut <- cbind( Parameter = rownames( dfStats ), dfStats, dfQuant )
    rownames( dfOut ) <- NULL
    
    strCurrentStudy <- paste0( "p[", 1, "]" )
    vCurrentStudy <- as.numeric(
        do.call(
            rbind,
            lapply( mcmc, function( x ) x[ , strCurrentStudy, drop = TRUE ] )
        )
    )
    
    lRet <- list(
        samples = mcmc,
        summary = dfOut,
        vCurrentStudy = vCurrentStudy
    )
    return( lRet )
}



