######################################################################################################################## .
#' @param AnalyzeUsingBetaBinomial
#' @title Analyze for efficacy using a beta( alpha, beta ) prior to compute the posterior probability that experimental is better than control treatment care. 
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam Input Parameters which user may need to compute test statistic and perform test. 
#'                    User should access the variables using names, for example, DesignParam$Alpha, and not order. 
#' @param LookInfo A list containing input parameters related to multiple looks, which the user may need to compute 
#'                 test statistics and perform tests. Users should access the variables using their names 
#'                 (e.g., `LookInfo$NumLooks`) rather than by their order. Important variables in group sequential designs include:
#'                 
#'                 - `LookInfo$NumLooks`: An integer representing the number of looks in the study.
#'                 - `LookInfo$CurrLookIndex`: An integer representing the current index look, starting from 1.
#'                 - `LookInfo$CumEvents`: A vector of length `LookInfo$NumLooks`, containing the cumulative number of events at each look.
#'                 - `LookInfo$RejType`: A code representing rejection types. Possible values are:
#'                   - **Efficacy Only:**
#'                     - `0`: 1-Sided Efficacy Upper.
#'                     - `2`: 1-Sided Efficacy Lower.
#'                   - **Futility Only:**
#'                     - `1`: 1-Sided Futility Upper.
#'                     - `3`: 1-Sided Futility Lower.
#'                   - **Efficacy and Futility:**
#'                     - `4`: 1-Sided Efficacy Upper and Futility Lower.
#'                     - `5`: 1-Sided Efficacy Lower and Futility Upper.
#' @param UserParam A list of user defined parameters in East or East Horizon.
#'                  UserParam must be supplied and contain the following named elements:
#'  \describe{
#'      \item{UserParam$dAlphaCtrl}{Prior alpha parameter for control treatment.  Equivalent to the prior number of treatment successes.}
#'      \item{UserParam$dBetaCtrl}{Prior beta parameter for control treatment.  Equivalent to the prior number of treatment failures.}
#'      \item{UserParam$dAlphaExp}{Prior alpha parameter for experimental treatment. Equivalent to the prior number of treatment successes.}
#'      \item{UserParam$dBetaExp}{Prior beta parameter for experimental treatment. Equivalent to the prior number of treatment failures.}
#'      \item{UserParam$dUpperCutoffEfficacy}{A value (0,1) that specifies the upper cutoff for the efficacy check. Above this value will declare efficacy.}
#'      \item{UserParam$dLowerCutoffForFutility}{A value (0,1) that specified the lower cutoff for the futility check. Below this value will declare futility.}
#'  }
#'  If user variables are not specified then a Beta( 1, 1 ) prior is utilized for both standard of care and experimental.
#'  
#' @description In this version, the analysis for efficacy is to assume a beta prior to compute the posterior probability that experimental is better than control treatment.
#'              The futility is based on posterior probability being less than dLowerCutoffForFutility.  
#'              In this example we assume a Bayesian model and use posterior probabilities for decision making
#'              If user variables are not specified we assume:
#'              pi_Ctrl ~ beta( 10, 40 ); to reflect that knowledge that on control treatment 10/50 previous patients responded
#'              pi_Exp ~ beta( 0.2, 0.8 ); non-informative prior for Experimental to have the same prior mean as S but only 1 prior patient observed
#'              
#'              At an IA: If Pr( pi_Ctrl > pi_Exp | data ) > 0.95 --> Stop for efficacy.
#'              Otherwise if  Pr( pi_Ctrl > pi_Exp | data ) < 0.1 --> Stop for futility.
#'              At an FA: If Pr( pi_Ctrl > pi_Exp | data ) > 0.95 --> Declare efficacy, otherwise, declare futility.
#'              
#'              When using simulation to obtain the frequentist Operating Characteristic (OC) 
#'              of a Bayesian design, you should set dLowerCutoffForFutility = 0
#'              when simulating under the null case in order to obtain the false-positive rate of the non-binding futility rule.  
#'              When you set dLowerCutoffForFutility > 0, simulation will provide the OC of the binding futility rule because the rule is ALWAYS followed. 
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{Decision}{Required value. Integer Value with the following meaning:
#'                                  \describe{
#'                                    \item{Decision = 0}{when No boundary, futility or efficacy is  crossed}
#'                                    \item{Decision = 1}{when the Lower Efficacy Boundary Crossed}
#'                                    \item{Decision = 2}{when the Upper Efficacy Boundary Crossed}
#'                                    \item{Decision = 3}{when the Futility Boundary Crossed}
#'                                    \item{Decision = 4}{when the Equivalence Boundary Crossed}
#'                                    } 
#'                                    }
#'                  \item{AnalysisTime} {Optional Numeric value to be computed and returned by the user. }
#'                  \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'                  \item{Delta}{Estimated different between experimental and standard of care}
#'                  }
#'
#'@note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.
######################################################################################################################## .

AnalyzeUsingBetaBinomial <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL)
{
    library(CyneRgy)
    
    # Step 1: Retrieve necessary information from the objects East sent. You may not need all the variables ####
    if(  !is.null( LookInfo )  )
    {
        # Group sequential design
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfLooks          <- LookInfo$NumLooks
        nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        RejType              <- LookInfo$RejType
        TailType             <- DesignParam$TailType
    }
    else
    {
        # Fixed Design
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfEvents         <- DesignParam$MaxCompleters
        nQtyOfPatsInAnalysis <- nrow( SimData )
        TailType             <- DesignParam$TailType
    }
    
    
    if( is.null( UserParam ) )
    {
        
        # FATAL ERROR AS WE DON'T KNOW WHAT THE USER WANTS TO DO.  
        # Creating a FATAL error will avoid misleading results when UserParam is not supplied
        return(list(TestStat  = as.double(0), 
                    ErrorCode = as.integer(-1), 
                    Decision  = as.integer( 0 ),
                    Delta     = as.double( 0 )))
    }
    
    
    # Step 2 - Create the vector of simulated data for this IA - East sends all of the simulated data ####
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Create vectors of data for each treatment 
    vOutcomesCtrl        <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesExp         <- vPatientOutcome[ vPatientTreatment == 1 ]
    

    # Step 3 -Perform the desired analysis - for this case a Bayesian analysis.  If Posterior Probability is > Cutoff --> Efficacy ####
    # The function PerformAnalysisBetaBinomial is provided below in this file.
    lRet                 <- ProbExpGreaterCtrlBeta( vOutcomesCtrl, vOutcomesExp, UserParam$dAlphaCtrl, UserParam$dBetaCtrl, UserParam$dAlphaExp, UserParam$dBetaExp )
    
    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, 
                                               bIAEfficacyCondition = lRet$dPostProb > UserParam$dUpperCutoffEfficacy, 
                                               bIAFutilityCondition = lRet$dPostProb <  UserParam$dLowerCutoffForFutility,
                                               bFAEfficacyCondition = lRet$dPostProb > UserParam$dUpperCutoffEfficacy)
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    Error 	<- 0
    
    return(list(TestStat = as.double(lRet$dPostProb), ErrorCode = as.integer(Error), Decision = as.integer( nDecision ), Delta = as.double( lRet$dDelta ) ) )
}




# Function for performing statistical analysis using a Beta-Binomial Bayesian model

ProbExpGreaterCtrlBeta <- function( vOutcomesCtrl, vOutcomesExp, dAlphaCtrl, dBetaCtrl, dAlphaExp, dBetaExp ) 
{
    # In the beta-binomial model if we make the assumption that 
    # pi ~ Beta( a, b )
    # then the posterior of pi is:
    # pi | data ~ Beta( a + # success, b + # non-successes )
    
    # Compute the posterior parameters for control treatment 
    dAlphaCtrl  <- dAlphaCtrl + sum( vOutcomesCtrl )
    dBetaCtrl   <- dBetaCtrl  + length( vOutcomesCtrl ) - sum( vOutcomesCtrl )
    
    # Compute the posterior parameters for Exp treatment 
    dAlphaExp   <- dAlphaExp + sum( vOutcomesExp )
    dBetaExp    <- dBetaExp  + length( vOutcomesExp ) - sum( vOutcomesExp )
    
    # There are much more efficient ways to compute this, but for simplicity, we are just sampling the posteriors
    vPiCtrl    <- rbeta( 10000, dAlphaCtrl, dBetaCtrl )
    vPiExp     <- rbeta( 10000, dAlphaExp,  dBetaExp  )
    dPostProb  <- ifelse( vPiExp > vPiCtrl, 1, 0 )
    dPostProb  <- sum( dPostProb )/length( dPostProb )
    
    # Compute Delta: mean( Pi_E ) - mean( Pi_C )
    dDelta     <- ( dAlphaExp/( dAlphaExp + dBetaExp) ) - ( dAlphaCtrl/( dAlphaCtrl +  dBetaCtrl ))
    return(list(dPostProb = dPostProb, dDelta = dDelta))
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
