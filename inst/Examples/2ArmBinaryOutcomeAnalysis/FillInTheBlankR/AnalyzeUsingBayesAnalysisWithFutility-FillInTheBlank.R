######################################################################################################################## .
#' @name AnalyzeUsingBayesAnalysisWithFutility
#' @title Analyze for efficacy using a beta prior to compute the posterior probability that experimental is better than standard of care.
#' @author J. Kyle Wathen and Gabriel Potvin
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#'        \describe{
#'          \item{ArrivalTime}{A numeric value with the time the patient arrived in the trial}
#'          \item{TreatmentID}{An integer value where 0 indicates control treatment and 1 experimental treatment.}
#'          \item{Response}{An integer value where 1 indicates response and 0 indicates no response.}
#'          \item{CensorIndOrg}{An integer value indicating whether the subject was censored or not.}
#'        }
#' @param DesignParam List of design and simulation parameters needed to compute test statistics and perform testing. Access elements by name, for example `DesignParam$Alpha`, rather than by position.
#'      \describe{
#'          \item{SampleSize}{Sample size of the trial}
#'          \item{Alpha}{Type I Error}
#'          \item{TestType}{Values are One side: 0; Two Sided: 1, Two Sided, Asymmetric: 2}
#'          \item{TailType}{Values are Left Tailed: 0, Right Tailed: 1}
#'          \item{LowerAlpha}{Lower Type I error. Present for Left Tailed and Two Sided Asymmetric Tests }
#'          \item{UpperAlpha}{Upper Type I error. Present for Right Tailed and Two Sided Asymmetric Tests }
#'          \item{MaxCompleters}{Maximum Number of Completers}
#'          \item{FollowUpType}{Follow-up type: 0 for until the end of the study, or 1 for a fixed period.}
#'          \item{AllocInfo}{Vector of ratios of treatment sample sizes to control sample size. Length = Number of treatment arms. }
#'          \item{CriticalPoint}{Critical Value. Present in Fixed Sample designs only }
#'          \item{UpperCriticalPoint}{Upper Critical Value. Present in Right Tail Fixed Sample designs only }
#'          \item{LowerCriticalPoint}{Lower Critical Value. Present in Left Tail Fixed Sample designs only }
#'          \item{RespLag}{Follow up duration}
#'          \item{TrtEffNull}{Treatment Effect under Null on natural scale. Applicable for Non-inferiority trials.}
#'
#'      }
#' @param LookInfo List of parameters for the current analysis look. It is `NULL` for fixed-sample designs. Access elements by name, for example `LookInfo$NumLooks`, rather than by position.
#'                 \describe{
#'                      \item{NumLooks}{An integer value with the number of looks in the study}
#'                      \item{CurrLookIndex}{An integer value with the current index look, starting from 1}
#'                      \item{CumCompleters}{Cumulative number of completer for all non time-to-event studies.}
#'                      \item{InfoFrac}{Information fraction}
#'                      \item{RejType}{Rejection type identifying the enabled efficacy and futility boundaries.}
#'                      \item{CumAlpha}{Cumulative alpha spent. Present in one sided tests only }
#'                      \item{CumAlphaUpper}{Upper cum. alpha spent. Present in right tailed and two sided tests only }
#'                      \item{CumAlphaLower}{Lower cum. alpha spent. Present in left tailed and two sided tests only }
#'                      \item{EffBdryScale}{Efficacy boundary scale.  Possible values are: Z Scale: 0, p-Value Scale: 1}
#'                      \item{EffBdry}{Vector of efficacy boundaries. Present in one sided tests only }
#'                      \item{EffBdryUpper}{Vector of upper efficacy boundaries. Present in right tailed and two sided tests only }
#'                      \item{EffBdryLower}{Vector of lower efficacy boundary. Present in left tailed and two sided tests only }
#'                      \item{FutBdryScale}{Futility boundary scale. Possible value are: Z Scale: 0, p-Value Scale: 1, Delta Scale: 2, Conditional Power Scale: 3}
#'                      \item{FutBdry}{Vector of futility boundaries. Present in one sided tests only }
#'                      \item{FutBdryUpper}{Vector of upper futility boundaries. Present in left tailed and two sided tests only }
#'                      \item{FutBdryLower}{Vector of lower futility boundaries. Present in right tailed and two sided tests only }
#'                      \item{CPDeltaOption}{Conditional-power treatment-effect option: 0 for design Delta or 1 for estimated Delta.}
#'                      \item{BindingType}{Futility binding type: 0 for non-binding or 1 for binding.}
#'                 }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'                  If UserParam is supplied, the list must contain the following named elements:
#'                  \describe{
#'                    \item{UserParam$dAlphaS}{First Beta-prior shape parameter for the standard-of-care response probability.}
#'                    \item{UserParam$dBetaS}{Second Beta-prior shape parameter for the standard-of-care response probability.}
#'                    \item{UserParam$dAlphaE}{First Beta-prior shape parameter for the experimental response probability.}
#'                    \item{UserParam$dBetaE}{Second Beta-prior shape parameter for the experimental response probability.}
#'                    \item{UserParam$dUpperCutoffEfficacy}{Posterior-probability threshold above which efficacy is declared.}
#'                    \item{UserParam$dLowerCutoffForFutility}{Predictive-probability threshold below which futility is declared.}
#'                  }
#' @description In this version, the analysis for efficacy is to assume a beta prior to compute the posterior probability that experimental is better than standard of care.
#'              The futility is based on a Bayesian predictive probability.
#'              The prior for the prediction and the analysis do NOT need to be the same.
#'              This function requires more info in the glDesign than the previous AnalyzeUsingBetaBinomBayesianModel
#'
#' @return After the blanks are completed, a named list containing `TestStat`, `ErrorCode`, and `Decision`.
#'@note In this example we assume a Bayesian model and use posterior probabilities for decision making
#' If user variables are not specified, the example uses beta priors defined in the function.
######################################################################################################################## .

AnalyzeUsingBayesAnalysisWithFutility <- function( SimData, DesignParam, LookInfo, UserParam = NULL )
{

    # In this example, the majority of the code is provided.  The fill in the blank areas are noted by _____________________.
    # This is done to allow you to practice creating these examples. You will need to remove the ____________ and enter the correct code.
    # The fully worked examples are provided in the corresponding example R files.

    # The below lines set the values of the parameters if a user does not specify a value

    if( is.null( UserParam ) )
    {
        UserParam <- list( dAlphaS = 10, dBetaS = 40, dAlphaE = 0.2, dBetaE = 0.8,
                   dUpperCutoffEfficacy = 0.975, dLowerCutoffForFutility = 0.1 )
    }

    # Pull important information from the input parameters that were sent from East Horizon
    nQtyOfLooks          <- LookInfo$NumLooks
    nLookIndex           <- LookInfo$CurrLookIndex
    nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]

    nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]

    # Create the vector of simulated data for this IA - East Horizon sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]

    # Create vectors of data for each treatment
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- ___________[ vPatientTreatment == 1 ]

    # Important Note:
    # When using simulation to obtain the frequentist Operating Characteristic (OC) of a Bayesian design, you should set dLowerCutoffForFutility = 0
    # when simulating under the null case in order to obtain the false-positive rate of the non-binding futility rule.
    # When you set dLowerCutoffForFutility > 0, simulation will provide the OC of the binding futility rule because the rule is ALWAYS followed.

    # Perform the desired analysis - for this case a Bayesian analysis.  If Posterior Probability is > Cutoff --> Efficacy ####
    # The function PerformAnalysisBetaBinomial is provided below in this file.
    lRet                 <- PerformAnalysisBetaBinomial( vOutcomesS, vOutcomesE, UserParam$dAlphaS, UserParam$dBetaS, UserParam$dAlphaE, UserParam$dBetaE )
    nDecision            <- ifelse( lRet$dPostProb > ____________, 2, 0 ) # Above the cutoff --> Efficacy

    if( nDecision == 0 )
    {
        # Did not hit efficacy, so check futility
        # We are at the FA, efficacy decision was not made yet so the decision is futility
        if( nLookIndex == nQtyOfLooks )
        {
            nDecision <- 3 # Futility
        }
        else if( lRet$dPostProb < ______________ ) # We are at the FA, efficacy decision was not made yet so the decision is futility
        {
            nDecision <- 3 # Futility
        }

    }

    nError     <- 0
    # retval <- 0

    return( list( ______ = as.double( lRet$dPostProb ), ErrorCode = as.integer( nError ), Decision = as.integer( nDecision ) ) )
}

# Function for performing statistical analysis using a Beta-Binomial Bayesian model

PerformAnalysisBetaBinomial <- function( vOutcomesS, vOutcomesE, dAlphaS, dBetaS, dAlphaE, dBetaE )
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
    vPiExp     <- rbeta( 10000, dAlphaE, dBetaE )
    dPostProb  <- ifelse( vPiExp > vPiCtrl, 1, 0 )
    dPostProb  <- sum( dPostProb ) / length( dPostProb )

    return( list( dPostProb = dPostProb ) )
}

# Function to compute Bayesian predictive probability of success
ComputeBayesianPredictiveProbabilityWithBayesianAnalysis <- function( dataS, dataE, priorAlphaS, priorBetaS, priorAlphaE, priorBetaE, nQtyOfPatsS, nQtyOfPatsE, nSimulations, finalBoundary, lAnalysisParams )
{
    # Compute the posterior parameters based on observed data
    posteriorAlphaS <- priorAlphaS + sum( dataS )
    posteriorBetaS  <- priorBetaS + length( dataS ) - sum( dataS )

    posteriorAlphaE <- priorAlphaE + sum( dataE )
    posteriorBetaE  <- priorBetaE + length( dataE ) - sum( dataE )

    #lAnalysisParams <- list( dAlphaCtrl = priorAlphaS,
    #                         dBetaCtrl  = priorBetaS,
    #                         dAlphaExp  = priorAlphaE,
    #                         dBetaExp   = priorBetaE )

    # Initialize counters for successful trials
    successfulTrials <- 0

    # Simulate the remaining trials and compute the predictive probability
    for( i in 1:nSimulations )
    {
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
        result <- PerformAnalysisBetaBinomial( combinedDataS, combinedDataE, lAnalysisParams )

        # Check if the result meets the cutoff for success
        if( result$dPostProb <= finalBoundary )
        {
            successfulTrials <- successfulTrials + 1
        }
    }

    # Compute the Bayesian predictive probability of success
    predictiveProbabilityS <- successfulTrials / nSimulations

    # Return the result
    return( list( predictiveProbabilityS = predictiveProbabilityS ) )
}
