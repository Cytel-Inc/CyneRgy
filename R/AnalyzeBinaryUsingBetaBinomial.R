######################################################################################################################## .
#' @name AnalyzeBinaryUsingBetaBinomial
#' @title Analyze Binary Data Using Beta-Binomial Model
#' 
#' @description Perform analysis for efficacy using a Beta(\eqn{\alpha}, \eqn{\beta}) prior to compute the posterior probability that the experimental treatment is better than the control treatment care.
#' The analysis assumes a Bayesian model and uses posterior probabilities for decision-making:
#'
#' - **Efficacy:** If \eqn{Pr(\pi_{Exp} > \pi_{Ctrl} | \text{data}) > \text{Upper Cutoff Efficacy}}, declare efficacy.
#' - **Futility:** If \eqn{Pr(\pi_{Exp} > \pi_{Ctrl} | \text{data}) < \text{Lower Cutoff Futility}}, declare futility.
#' - At final analysis (FA): Declare efficacy or futility based on the posterior probability.
#'
#' When simulating under the null case, setting \eqn{dLowerCutoffForFutility = 0} provides the false-positive rate for the non-binding futility rule. 
#' Setting \eqn{dLowerCutoffForFutility > 0} provides the operating characteristics (OC) of the binding futility rule, as the rule is always followed.
#'
#' @param SimData A data frame containing the data generated in the current simulation.
#' @param DesignParam A list of input parameters necessary to compute the test statistic and perform the test. 
#'                    Variables should be accessed using names (e.g., `DesignParam$Alpha`).
#' @param LookInfo A list of input parameters related to multiple looks in group sequential designs. 
#' Variables should be accessed by names (e.g., `LookInfo$NumLooks`). Important variables include:
#'
#' - `LookInfo$NumLooks`: Integer, number of looks in the study.
#' - `LookInfo$CurrLookIndex`: Integer, current look index (starting from 1).
#' - `LookInfo$CumEvents`: Vector, cumulative number of events at each look.
#' - `LookInfo$RejType`: Code representing rejection types. Possible values include:
#'  - **Efficacy Only:**
#'      - `0`: 1-Sided Efficacy Upper.
#'      - `2`: 1-Sided Efficacy Lower.
#'  - **Futility Only:**
#'      - `1`: 1-Sided Futility Upper.
#'      - `3`: 1-Sided Futility Lower.
#'  - **Efficacy and Futility:**
#'      - `4`: 1-Sided Efficacy Upper and Futility Lower.
#'      - `5`: 1-Sided Efficacy Lower and Futility Upper.
#'
#' @param UserParam A list of user-defined parameters. Must contain the following named elements:
#'  \describe{
#'      \item{dAlphaCtrl}{Prior alpha parameter for control treatment (prior successes).}
#'      \item{dBetaCtrl}{Prior beta parameter for control treatment (prior failures).}
#'      \item{dAlphaExp}{Prior alpha parameter for experimental treatment (prior successes).}
#'      \item{dBetaExp}{Prior beta parameter for experimental treatment (prior failures).}
#'      \item{dUpperCutoffEfficacy}{Upper cutoff (0,1) for efficacy check. Above this value declares efficacy.}
#'      \item{dLowerCutoffForFutility}{Lower cutoff (0,1) for futility check. Below this value declares futility.}
#'  }
#'  If not specified, a Beta(1, 1) prior is used for both control and experimental treatments.
#'
#' @return A list containing the following elements:
#'  \describe{
#'      \item{TestStat}{A double representing the computed test statistic.}
#'      \item{Decision}{Required integer value indicating the decision made:
#'                      \describe{
#'                        \item{0}{No boundary crossed (neither efficacy nor futility).}
#'                        \item{1}{Lower efficacy boundary crossed.}
#'                        \item{2}{Upper efficacy boundary crossed.}
#'                        \item{3}{Futility boundary crossed.}
#'                        \item{4}{Equivalence boundary crossed.}
#'                      }}
#'      \item{ErrorCode}{Optional integer value:
#'                      \describe{
#'                        \item{0}{No error.}
#'                        \item{> 0}{Non-fatal error; current simulation is aborted but subsequent simulations continue.}
#'                        \item{< 0}{Fatal error; no further simulations are attempted.}
#'                      }}
#'      \item{Delta}{Estimated difference between experimental and control treatments.}
#'  }
#' @export
######################################################################################################################## .

AnalyzeBinaryUsingBetaBinomial <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Step 1: Retrieve necessary information from the objects East or East Horizon sent. You may not need all the variables ####
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
    
    # Create a fatal error when user parameters are missing to avoid misleading results
    vRequiredParams <- c( "dAlphaCtrl", "dBetaCtrl", "dAlphaExp", "dBetaExp", "dUpperCutoffEfficacy", "dLowerCutoffForFutility" )
    vMissingParams <- vRequiredParams[ !vRequiredParams %in% names( UserParam ) ]
    
    if( is.null( UserParam ) || length( vMissingParams ) > 0 )
    {
        return( list( TestStat  = as.double( 0 ), 
                      ErrorCode = as.integer( -1 ), 
                      Decision  = as.integer( 0 ),
                      Delta     = as.double( 0 ) ) )
    }
    
    # Step 2 - Create the vector of simulated data for this IA - East or East Horizon sends all of the simulated data ####
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
                                               bFAEfficacyCondition = lRet$dPostProb > UserParam$dUpperCutoffEfficacy )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    
    Error 	<- 0
    
    return( list( TestStat = as.double( lRet$dPostProb ), ErrorCode = as.integer( Error ), Decision = as.integer( nDecision ), Delta = as.double( lRet$dDelta ) ) )
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
    dPostProb  <- mean( dPostProb )
    
    # Compute Delta: mean( Pi_E ) - mean( Pi_C )
    dDelta     <- ( dAlphaExp/( dAlphaExp + dBetaExp ) ) - ( dAlphaCtrl/( dAlphaCtrl +  dBetaCtrl ) )
    return( list( dPostProb = dPostProb, dDelta = dDelta ) )
}




# Function to compute Bayesian predictive probability of success
ComputeBayesianPredictiveProbabilityWithBayesianAnalysis <- function( dataS, dataE, priorAlphaS, priorBetaS, priorAlphaE, priorBetaE, nQtyOfPatsS, nQtyOfPatsE, nSimulations, finalBoundary, lAnalysisParams ) {
    # Compute the posterior parameters based on observed data
    posteriorAlphaS <- priorAlphaS + sum( dataS )
    posteriorBetaS  <- priorBetaS + length( dataS ) - sum( dataS )
    
    posteriorAlphaE <- priorAlphaE + sum( dataE )
    posteriorBetaE  <- priorBetaE + length( dataE ) - sum( dataE )
    
    # Initialize counters for successful trials
    successfulTrials <- 0
    
    # Simulate the remaining trials and compute the predictive probability
    for ( i in 1:nSimulations ) {
        # Sample response rates from posterior distributions
        posteriorRateS <- rbeta( 1, posteriorAlphaS, posteriorBetaS )
        posteriorRateE <- rbeta( 1, posteriorAlphaE, posteriorBetaE )
        
        # Simulate patient outcomes for for the current virtual trial based on sampled rates
        # The data at the end of the trial is a combination of the data at the interim, dataS, and the simulated data to the end of the trial, remainingDataS
        remainingDataS <- SimulatePatientOutcome( nQtyOfPatsS - length( dataS ), posteriorRateS )
        combinedDataS  <- c( dataS, remainingDataS )
        
        remainingDataE <- SimulatePatientOutcome( nQtyOfPatsE - length( dataE ), posteriorRateE )
        combinedDataE  <- c( dataE, remainingDataE )
        
        
        # Perform the analysis with combined data to check if the trial is successful
        result <- ProbSGreaterEBeta( combinedDataS, combinedDataE, lAnalysisParams )
        
        # Check if the result meets the cutoff for success
        if ( result$dPostProb <= finalBoundary ) {
             successfulTrials <- successfulTrials + 1
        }
    }
    
    # Compute the Bayesian predictive probability of success
    predictiveProbabilityS <- successfulTrials / nSimulations
    
    # Return the result
    return( list( predictiveProbabilityS = predictiveProbabilityS ) )
}