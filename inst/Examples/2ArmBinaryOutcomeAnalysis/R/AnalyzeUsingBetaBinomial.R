######################################################################################################################## .
#' @name AnalyzeUsingBetaBinomial
#' @title Analyze for efficacy using a beta( alpha, beta ) prior to compute the posterior probability that experimental is better than control treatment care.
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
#' @return A named list containing posterior probability `TestStat`, integer `ErrorCode`, integer `Decision`, and estimated response difference `Delta`.
#' @details
#' ## CyneRgy Decision Helpers
#'
#' The analysis may use `CyneRgy::GetDecisionString()` and
#' `CyneRgy::GetDecision()` to determine the decision returned to
#' East Horizon Explore.
#'
#' When these helpers are used, the following input fields are required
#' and MUST be included when generating sample/test data:
#'
#' DesignParam:
#'   - TailType: Integer indicating the direction of the statistical test.
#'       0 = Left-tailed
#'       1 = Right-tailed
#'
#' LookInfo (for group sequential designs, NULL for fixed designs):
#' When not NULL, must contain the following fields:
#'   - NumLooks: Total number of looks.
#'   - CurrLookIndex: Current look index, starting at 1.
#'   - RejType: Integer identifying which stopping boundaries are enabled.
#'       0 = 1-Sided Efficacy Upper
#'       1 = 1-Sided Futility Upper
#'       2 = 1-Sided Efficacy Lower
#'       3 = 1-Sided Futility Lower
#'       4 = 1-Sided Efficacy Upper and Futility Lower
#'       5 = 1-Sided Efficacy Lower and Futility Upper
#'       6 = 2-Sided Efficacy Only (not used in East Horizon Explore)
#'       7 = 2-Sided Futility Only (not used in East Horizon Explore)
#'       8 = 2-Sided Efficacy and Futility (not used in East Horizon Explore)
#'       9 = Equivalence (not used in East Horizon Explore)
#'
######################################################################################################################## .

AnalyzeUsingBetaBinomial <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Step 1: Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
    if( !is.null( LookInfo ) )
    {
        # Group sequential design
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfLooks          <- LookInfo$NumLooks
        nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        nRejType             <- LookInfo$RejType
        nTailType            <- DesignParam$TailType
    }
    else
    {
        # Fixed Design
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfEvents         <- DesignParam$MaxCompleters
        nQtyOfPatsInAnalysis <- nrow( SimData )
        nTailType            <- DesignParam$TailType
    }

    if( is.null( UserParam ) )
    {

        # FATAL ERROR AS WE DON'T KNOW WHAT THE USER WANTS TO DO.
        # Creating a FATAL error will avoid misleading results when UserParam is not supplied
        return( list( TestStat  = as.double( 0 ),
                    ErrorCode = as.integer( -1 ),
                    Decision  = as.integer( 0 ),
                    Delta     = as.double( 0 ) ) )
    }

    # Step 2 - Create the vector of simulated data for this IA - East Horizon sends all of the simulated data ####
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

    nError     <- 0

    return( list( TestStat = as.double( lRet$dPostProb ), ErrorCode = as.integer( nError ), Decision = as.integer( nDecision ), Delta = as.double( lRet$dDelta ) ) )
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
    vPiExp     <- rbeta( 10000, dAlphaExp, dBetaExp )
    dPostProb  <- ifelse( vPiExp > vPiCtrl, 1, 0 )
    dPostProb  <- sum( dPostProb ) / length( dPostProb )

    # Compute Delta: mean( Pi_E ) - mean( Pi_C )
    dDelta     <- ( dAlphaExp / ( dAlphaExp + dBetaExp ) ) - ( dAlphaCtrl / ( dAlphaCtrl + dBetaCtrl ) )
    return( list( dPostProb = dPostProb, dDelta = dDelta ) )
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
        result <- ProbSGreaterEBeta( combinedDataS, combinedDataE, lAnalysisParams )

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
