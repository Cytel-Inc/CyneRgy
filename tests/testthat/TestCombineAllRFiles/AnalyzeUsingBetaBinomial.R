######################################################################################################################## .
#' Analyze Using Bayesian Beta-Binomial Model for Binary Data
#' Analyze for efficacy using a beta prior to compute the posterior probability that experimental is better than standard of care. 
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East. The default must be NULL.
#' If UseParam is supplied, the list must contain the following named elements:
#' \describe{
#'    \item{UserParam$dAlphaS}{}
#'    \item{UserParam$dBetaS}{}
#'    \item{UserParam$dAlphaE}{}
#'    \item{UserParam$dBetaE}{}
#'    \item{UserParam$dMAV}{}
#'    \item{UserParam$dUpperCutoffEfficacy}{ A value (0,1) that specifies the upper cutoff for the efficacy check. Above this value will declare efficacy }
#'    \item{UserParam$dLowerCutoffForFutility}{A value (0,1) that specified the lower cutoff for the futility check. Below this value will declare futility. }
#'    }
#' @description In this version, the analysis for efficacy is to assume a beta prior to compute the posterior probability that experimental is better than standard of care.
#'              The futility is based on a Bayesian predictive probability.  
#'              The prior for the prediction and the analysis do NOT need to be the same.  
#'              This function requires more info in the glDesign than the previous AnalyzeUsingBetaBinomBayesianModel
#'              
#' @return TestStat A double value of the computed test statistic
#' @return Decision An integer value: Decision = 0 --> No boundary crossed
#'                                    Decision = 1 --> Lower Efficacy Boundary Crossed
#'                                    Decision = 2 --> Upper Efficacy Boundary Crossed
#'                                    Decision = 3 --> Futility Boundary Crossed
#'                                    Decision = 4 --> Equivalence Boundary Crossed
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted

#'@note In this example we assume a Bayesian model and use posterior probabilities for decision making
#       If user variables are not specified we assume non-informative uniform prior equivalent to observing two patient outcomes:
#       pi_S ~ beta( 1, 1 ); 
#       pi_E ~ beta( 1, 1 ); 
#' @note 
#' When using simulation to obtain the frequentist Operating Characteristic (OC) of a Bayesian design, you should set dLowerCutoffForFutility = 0
#' when simulating under the null case in order to obtain the false-positive rate of the non-binding futility rule.  
#' When you set dLowerCutoffForFutility > 0, simulation will provide the OC of the binding futility rule because the rule is ALWAYS followed. 
#'@note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.
#' TODO(Kyle): I am not sure how to define the alpha and beta user parameters. Could you define and add to documentation?
#' @export
######################################################################################################################## .
AnalyzeUsingBetaBinomial <- function(SimData, DesignParam, LookInfo, UserParam = NULL)
{
    # The below lines set the values of the parameters if a user does not specify a value
    
    if( is.null( UserParam ) )
    {
        # Default values are non-informative uniform prior
        UserParam <- list(dAlphaS = 0.5, dBetaS = 0.5, 
                          dAlphaE = 0.5, dBetaE = 0.5, 
                          dMAV = 0, 
                          dUpperCutoffEfficacy = 0.975,
                          dLowerCutoffForFutility = 0.1 )
    }

    
    
    # Pull important information from the input parameters that were sent from East
    nQtyOfLooks          <- LookInfo$NumLooks
    nLookIndex           <- LookInfo$CurrLookIndex
    nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
    
    nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
    
    # Create the vector of simulated data for this IA - East sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Create vectors of data for each treatment 
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]
  
    # Perform the desired analysis - for this case a Bayesian analysis.  If Posterior Probability is > Cutoff --> Efficacy ####
    # The function ProbExpGreaterStdPlusDeltaBeta is provided below in this file.
    
    lRet                 <- ProbExpGreaterStdPlusDeltaBeta( vOutcomesS, vOutcomesE, UserParam$dAlphaS, UserParam$dBetaS, UserParam$dAlphaE, UserParam$dBetaE, UserParam$dMAV )
    nDecision            <- ifelse( lRet$dPostProb > UserParam$dUpperCutoffEfficacy, 2, 0 )  # Above the cutoff --> Efficacy ( 2 is East code for Efficacy)
    
    if( nDecision == 0 )
    {
        # Did not hit efficacy, so check futility 
        # We are at the FA, efficacy decision was not made yet so the decision is futility
        if( nLookIndex == nQtyOfLooks ) 
        {
            nDecision <- 3 # East code for futility 
        }
        else 
        {
            # Check for futility ####
            if( lRet$dPostProb <  UserParam$dLowerCutoffForFutility ) # We are at the FA, efficacy decision was not made yet so the decision is futility
                nDecision <- 3 # East code for futility 
        }
        
    }
    
    if( !file.exists( paste0( "SimData", nLookIndex, ".Rds") ))
    {
        saveRDS( SimData, paste0( "SimData", nLookIndex, ".Rds") )
        saveRDS( DesignParam, paste0( "DesignParam", nLookIndex, ".Rds") )
        saveRDS( LookInfo, paste0( "LookInfo", nLookIndex, ".Rds") )
    }
    Error 	<- 0
    #retval 	= 0
    
    
    return(list(TestStat = as.double(lRet$dPostProb), ErrorCode = as.integer(Error), Decision = as.integer( nDecision ) ))
}




# Function for performing statistical analysis using a Beta-Binomial Bayesian model

ProbExpGreaterStdPlusDeltaBeta <- function(vOutcomesS, vOutcomesE, dAlphaS, dBetaS, dAlphaE, dBetaE, dDelta = 0.0) 
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
    vPiStd     <- rbeta( 10000, dAlphaS, dBetaS )
    vPiExp     <- rbeta( 10000, dAlphaE,  dBetaE  )
    dPostProb  <- ifelse( vPiExp > vPiStd + dDelta, 1, 0 )
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
        result <- ProbSGreaterEBeta(combinedDataS, combinedDataE, lAnalysisParams )
        
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



