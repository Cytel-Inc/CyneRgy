######################################################################################################################## .
#' @name AnalyzeUsingBetaBinomial
#' @title Analyze for efficacy using a beta( alpha, beta ) prior to compute the posterior probability that
#'   experimental is better than control treatment.
#' @description In this version, the analysis for efficacy is to assume a beta prior to compute the posterior
#'   probability that experimental is better than control treatment. The futility is based on posterior probability
#'   being less than dLowerCutoffForFutility. In this example we assume a Bayesian model and use posterior
#'   probabilities for decision making. UserParam is required; illustrative priors are: pi_Ctrl ~ beta( 10, 40 ); to
#'   reflect the knowledge that on control treatment 10/50 previous patients responded pi_Exp ~ beta( 0.2, 0.8 );
#'   weak prior for the experimental arm with the same prior mean as control and an effective sample size of 1.
#'
#' At an IA: If Pr( pi_Exp > pi_Ctrl | data ) > 0.95 --> Stop for efficacy. Otherwise if Pr( pi_Exp > pi_Ctrl |
#'   data ) < 0.1 --> Stop for futility. At an FA: If Pr( pi_Exp > pi_Ctrl | data ) > 0.95 --> Declare efficacy,
#'   otherwise, declare futility.
#'
#' When using simulation to obtain the frequentist Operating Characteristic (OC) of a Bayesian design, you should
#'   set dLowerCutoffForFutility = 0 when simulating under the null case in order to obtain the false-positive rate
#'   of the non-binding futility rule. When you set dLowerCutoffForFutility > 0, simulation will provide the OC of
#'   the binding futility rule because the rule is ALWAYS followed.
#' @author J. Kyle Wathen and Gabriel Potvin
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$TreatmentID`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#' }
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$TailType`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{TailType}{Integer. Nature of critical region: - `0`: Left-tailed. - `1`: Right-tailed. East Horizon
#'     Design: Only available if `Test Type = One-sided`.}
#'   \item{MaxCompleters}{Integer maximum number of completers in the trial.}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumLooks}{Integer total number of analysis looks.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{CumCompleters}{Integer vector of cumulative completer counts, one per look.}
#'   \item{CumEvents}{Cumulative event-count vector indexed by LookInfo$CurrLookIndex in this example.}
#'   \item{RejType}{Integer one-sided rejection type supported by the decision helpers: 0 = upper efficacy;
#'     1 = upper futility; 2 = lower efficacy; 3 = lower futility; 4 = upper efficacy and lower futility;
#'     5 = lower efficacy and upper futility.}
#' }
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' UserParam must be supplied and contain the following named elements:
#'  \describe{
#'      \item{UserParam$dAlphaCtrl}{Prior alpha parameter for control treatment.  Equivalent to the prior number of
#'        treatment successes.}
#'      \item{UserParam$dBetaCtrl}{Prior beta parameter for control treatment.  Equivalent to the prior number of
#'        treatment failures.}
#'      \item{UserParam$dAlphaExp}{Prior alpha parameter for experimental treatment. Equivalent to the prior number
#'        of treatment successes.}
#'      \item{UserParam$dBetaExp}{Prior beta parameter for experimental treatment. Equivalent to the prior number
#'        of treatment failures.}
#'      \item{UserParam$dUpperCutoffEfficacy}{A value (0,1) that specifies the upper cutoff for the efficacy check.
#'        Above this value will declare efficacy.}
#'      \item{UserParam$dLowerCutoffForFutility}{A value (0,1) that specifies the lower cutoff for the futility
#'        check. Below this value will declare futility.}
#'  }
#'  UserParam is required. Missing user parameters return ErrorCode = -1.
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore).}
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale.}
#'   \item{Delta}{Estimated experimental-minus-control treatment effect (proportion difference for binary outcomes;
#'     mean difference for continuous outcomes).}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details This example returns the posterior probability in TestStat for reporting and supplies Decision
#'   to apply its Bayesian stopping rule. The reported posterior probability is not a Wald Z statistic.
#'
######################################################################################################################## .

AnalyzeUsingBetaBinomial <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {

    # Step 1: Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
    if ( !is.null( LookInfo ) ) {
        # Group sequential design
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfLooks <- LookInfo$NumLooks
        nQtyOfEvents <- LookInfo$CumEvents[ nLookIndex ]
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        nRejType <- LookInfo$RejType
        nTailType <- DesignParam$TailType
    } else {
        # Fixed Design
        nLookIndex <- 1
        nQtyOfLooks <- 1
        nQtyOfEvents <- DesignParam$MaxCompleters
        nQtyOfPatsInAnalysis <- nrow( SimData )
        nTailType <- DesignParam$TailType
    }

    if ( is.null( UserParam ) ) {
        # FATAL ERROR AS WE DON'T KNOW WHAT THE USER WANTS TO DO.
        # Creating a FATAL error will avoid misleading results when UserParam is not supplied
        return( list(
            TestStat = as.double( 0 ),
            ErrorCode = as.integer( -1 ),
            Decision = as.integer( 0 ),
            Delta = as.double( 0 )
        ) )
    }

    # Step 2 - Create the vector of simulated data for this IA - East Horizon sends all of the simulated data ####
    vPatientOutcome <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]

    # Create vectors of data for each treatment
    vOutcomesCtrl <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesExp <- ____________[ vPatientTreatment == 1 ]

    # Step 3 -Perform the desired analysis - for this case a Bayesian analysis.  If Posterior Probability is > Cutoff --> Efficacy ####
    # The function PerformAnalysisBetaBinomial is provided below in this file.
    lRet <- ProbExpGreaterCtrlBeta( vOutcomesCtrl, vOutcomesExp, UserParam$dAlphaCtrl, UserParam$dBetaCtrl, UserParam$dAlphaExp, UserParam$dBetaExp )

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
        bIAEfficacyCondition = lRet$dPostProb > _____________________________,
        bIAFutilityCondition = lRet$dPostProb < _________________________________,
        bFAEfficacyCondition = lRet$dPostProb > _____________________________
    )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    nErrorCode <- 0

    return( list( ________ = as.double( lRet$dPostProb ), ErrorCode = as.integer( nErrorCode ), Decision = as.integer( nDecision ), Delta = as.double( lRet$dDelta ) ) )
}

######################################################################################################################## .
#' @name ProbExpGreaterCtrlBeta
#' @title Compute the posterior probability of a higher experimental response rate
#' @description Estimate the posterior probability that the experimental response probability exceeds the control
#'   response probability by sampling from the two beta posterior distributions.
#' @author J. Kyle Wathen and Gabriel Potvin
#' @param vOutcomesCtrl Integer vector of observed control-arm binary outcomes (0 = non-response, 1 = response).
#' @param vOutcomesExp Integer vector of observed experimental-arm binary outcomes (0 = non-response, 1 = response).
#' @param dAlphaCtrl Positive numeric alpha parameter of the control-arm beta prior.
#' @param dBetaCtrl Positive numeric beta parameter of the control-arm beta prior.
#' @param dAlphaExp Positive numeric alpha parameter of the experimental-arm beta prior.
#' @param dBetaExp Positive numeric beta parameter of the experimental-arm beta prior.
#' @return List containing dPostProb, the estimated posterior probability that the experimental response
#'   probability exceeds the control response probability, and dDelta, the experimental-minus-control
#'   posterior mean response-probability difference.
######################################################################################################################## .

ProbExpGreaterCtrlBeta <- function( vOutcomesCtrl, vOutcomesExp, dAlphaCtrl, dBetaCtrl, dAlphaExp, dBetaExp ) {
    # In the beta-binomial model if we make the assumption that
    # pi ~ Beta( a, b )
    # then the posterior of pi is:
    # pi | data ~ Beta( a + # success, b + # non-successes )

    # Compute the posterior parameters for control treatment
    dAlphaCtrl <- dAlphaCtrl + sum( vOutcomesCtrl )
    dBetaCtrl <- dBetaCtrl + length( vOutcomesCtrl ) - sum( vOutcomesCtrl )

    # Compute the posterior parameters for Exp treatment
    dAlphaExp <- dAlphaExp + sum( vOutcomesExp )
    dBetaExp <- dBetaExp + length( vOutcomesExp ) - sum( vOutcomesExp )

    # There are much more efficient ways to compute this, but for simplicity, we are just sampling the posteriors
    vPiCtrl <- stats::rbeta( 10000, dAlphaCtrl, dBetaCtrl )
    vPiExp <- stats::rbeta( 10000, dAlphaExp, dBetaExp )
    dPostProb <- ifelse( vPiExp > vPiCtrl, 1, 0 )
    dPostProb <- sum( dPostProb ) / length( dPostProb )

    dDelta <- dAlphaExp / ( dAlphaExp + dBetaExp ) - dAlphaCtrl / ( dAlphaCtrl + dBetaCtrl )
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

ComputeBayesianPredictiveProbabilityWithBayesianAnalysis <- function( dataS, dataE, priorAlphaS, priorBetaS, priorAlphaE, priorBetaE, nQtyOfPatsS, nQtyOfPatsE, nSimulations, finalBoundary, lAnalysisParams ) {
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
        # The data at the end of the trial is a combination of the data at the interim, dataS, and the simulated data to the end of the trial, vRemainingDataS
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

        # Check if the lResult meets the cutoff for success
        if ( lResult$dPostProb >= finalBoundary ) {
            nSuccessfulTrials <- nSuccessfulTrials + 1
        }
    }

    # Compute the Bayesian predictive probability of success
    dPredictiveProbability <- nSuccessfulTrials / nSimulations

    # Return the lResult
    return( list( predictiveProbabilityS = dPredictiveProbability ) )
}
