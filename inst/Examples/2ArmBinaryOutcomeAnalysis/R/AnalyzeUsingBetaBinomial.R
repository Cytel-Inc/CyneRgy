######################################################################################################################## .
#' @name AnalyzeUsingBetaBinomial
#'
#' @title Analyze for efficacy using a beta( alpha, beta ) prior to compute the posterior probability that
#'   experimental is better than control treatment care.
#'
#' @description In this version, the analysis for efficacy is to assume a beta prior to compute the posterior
#'   probability that experimental is better than control treatment. The futility is based on posterior probability
#'   being less than dLowerCutoffForFutility. In this example we assume a Bayesian model and use posterior
#'   probabilities for decision making UserParam is required; illustrative priors are: pi_Ctrl ~ beta( 10, 40 ); to
#'   reflect that knowledge that on control treatment 10/50 previous patients responded pi_Exp ~ beta( 0.2, 0.8 );
#'   non-informative prior for Experimental to have the same prior mean as S but only 1 prior patient observed
#'
#' At an IA: If Pr( pi_Exp > pi_Ctrl | data ) > 0.95 --> Stop for efficacy. Otherwise if Pr( pi_Exp > pi_Ctrl |
#'   data ) < 0.1 --> Stop for futility. At an FA: If Pr( pi_Exp > pi_Ctrl | data ) > 0.95 --> Declare efficacy,
#'   otherwise, declare futility.
#'
#' When using simulation to obtain the frequentist Operating Characteristic (OC) of a Bayesian design, you should
#'   set dLowerCutoffForFutility = 0 when simulating under the null case in order to obtain the false-positive rate
#'   of the non-binding futility rule. When you set dLowerCutoffForFutility > 0, simulation will provide the OC of
#'   the binding futility rule because the rule is ALWAYS followed.
#'
#' @author J. Kyle Wathen and Gabriel Potvin
#'
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the native fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#'   \item{CensorInd}{Integer vector of censor indicators, with one element per subject: 0 = dropout/non-completer;
#'     1 = completer.}
#'   \item{CensorIndOrg}{Original integer vector of censor indicators before any analysis-time adjustment: 0 =
#'     dropout/non-completer; 1 = completer.}
#' }
#'
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below.
#' \describe{
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{LowerAlpha}{Numeric. Lower Type I Error. Same as Alpha if left-tailed one-sided test. Only makes sense
#'     to use for two-sided asymmetric tests. East Horizon Explore: Only available if `Tail Type = Left-tailed`.
#'     Two-sided tests do not exist, so this variable is not useful: use Alpha instead. East Horizon Design: Only
#'     available if `Test Type = One-sided` and `Tail Type = Left-tailed`, or `Test Type = Two-sided asymmetric`.}
#'   \item{UpperAlpha}{Numeric. Upper Type I Error. Same as Alpha if right-tailed one-sided test. Only makes sense
#'     to use for two-sided asymmetric tests. East Horizon Explore: Only available if `Tail Type = Right-tailed`.
#'     Two-sided tests do not exist, so this variable is not useful: use Alpha instead. East Horizon Design: Only
#'     available if `Test Type = One-sided` and `Tail Type = Right-tailed`, or `Test Type = Two-sided asymmetric`.}
#'   \item{TrialType}{Integer. Trial Type: – `0`: Superiority. – `1`: Non-inferiority. – `2`: Equivalence. – `3`:
#'     Super-superiority. East Horizon Explore: Type 2 (equivalence) does not exist.}
#'   \item{TestType}{Integer. Test Type: – `0`: One-sided. – `1`: Two-sided symmetric. – `2`: Two-sided asymmetric.
#'     East Horizon Explore: Types 1 and 2 (two-sided) do not exist.}
#'   \item{TailType}{Integer. Nature of critical region: – `0`: Left-tailed. – `1`: Right-tailed. East Horizon
#'     Design: Only available if `Test Type = One-sided`.}
#'   \item{AllocInfo}{Vector of Numeric. Vector of length equal to the number of treatment arms (number of arms -
#'     1), containing the ratios of the treatment group sample sizes to control group sample size.}
#'   \item{CriticalPoint}{Numeric. Critical value (for one-sided tests). East Horizon Explore: Only available if
#'     `Statistical Design = Fixed Sample`. East Horizon Design: Only available if `Test Type = One-sided` and
#'     `Statistical Design = Fixed Sample`.}
#'   \item{LowerCriticalPoint}{Numeric. Lower critical value. Same as CriticalPoint if left-tailed one-sided test.
#'     Only makes sense to use for two-sided asymmetric tests. East Horizon Explore: Only available if `Statistical
#'     Design = Fixed Sample` and `Tail Type = Left-tailed`. Two-sided tests do not exist, so this variable is not
#'     useful: use CriticalPoint instead. East Horizon Design: Only available if `Statistical Design = Fixed
#'     Sample`. Only available if `Test Type = One-sided` and `Tail Type = Left-tailed`, or `Test Type = Two-sided
#'     symmetric/asymmetric`.}
#'   \item{UpperCriticalPoint}{Numeric. Upper critical value. Same as CriticalPoint if right-tailed one-sided test.
#'     Only makes sense to use for two-sided asymmetric tests. East Horizon Explore: Only available if `Statistical
#'     Design = Fixed Sample` and `Tail Type = Right-tailed`. Two-sided tests do not exist, so this variable is not
#'     useful: use CriticalPoint instead. East Horizon Design: Only available if `Statistical Design = Fixed
#'     Sample`. Only available if `Test Type = One-sided` and `Tail Type = Right-tailed`, or `Test Type = Two-sided
#'     symmetric/asymmetric`.}
#'   \item{SampleSize}{Integer planned total sample size of the trial.}
#'   \item{MaxCompleters}{Integer maximum number of completers in the trial.}
#'   \item{RespLag}{Numeric follow-up duration from enrollment to response measurement.}
#'   \item{TestStatType}{Integer. Test statistic type. For `Time-to-Event` tests: - `0`: Logrank. - `1`: Wilcoxon
#'     Gehan. - `2`: Harrington Fleming. - `3`: Stratified Logrank. - `4`: Stratified Wilcoxon Gehan. - `5`:
#'     Stratified Harrington Fleming. For `Continuous` test: - `3`: Z-test. - `4`: t-test. For `Binary` test: -
#'     `5`: Wald. - `6`: Score. East Horizon Explore: Not available. East Horizon Design: Not available for `Test =
#'     Difference of Proportions or Odds Ratio of Proportions` (Binary).}
#'   \item{VarType}{Integer. Variance type. For `Continuous` test: - `4`: Equal - `5`: Unequal. For `Difference of
#'     Proportions` (Binary) test: - `0`: Pooled. - `1`: Unpooled. For `Ratio of Proportions` (Binary): - `2`: Null
#'     - `3`: Empirical. East Horizon Explore: Not available. East Horizon Design: Not available for
#'     `Time-to-Event` tests or `Test = Odds Ratio of Proportions` (Binary).}
#'   \item{TrtEffNull}{Numeric. Treatment effect under null on natural scale. East Horizon Explore: Not available
#'     for `Endpoint Type = Continuous with Repeated Measures`. Set to `0` for `Trial Type = Superiority`. Set to
#'     `Delta_0 = log(HR_0)` for `Endpoint Type = Time-to-Event`. Set to `1 - rho_0` for Vaccine Efficacy
#'     (`Endpoint Type = Binary` with Lower Value and `Test = 1 - Ratio of Proportions or 1 - Ratio of Poisson
#'     Rates`). East Horizon Design: Set to `0` for `Trial Type = Superiority`. Set to `Delta_0 = log(HR_0)` for
#'     `Time-to-Event` tests.}
#'   \item{PiC}{Numeric. Design proportion for the control arm. East Horizon Explore: Not available. East Horizon
#'     Design: Only available for `Binary` tests.}
#' }
#'
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumLooks}{Integer total number of analysis looks.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{InfoFrac}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the information fraction
#'     for each look.}
#'   \item{CumAlpha}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the cumulative alpha spent
#'     (for one-sided tests) for each look. East Horizon Design: Only available if `Test Type = One-sided`.}
#'   \item{CumAlphaLower}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the lower cumulative
#'     alpha spent (for two-sided tests) for each look. Same as CumAlpha if left-tailed one-sided test. Only makes
#'     sense to use for two-sided asymmetric tests. East Horizon Explore: Only available if `Tail Type =
#'     Left-tailed`. Two-sided tests do not exist, so this variable is not useful: use CumAlpha instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Left-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
#'   \item{CumAlphaUpper}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the upper cumulative
#'     alpha spent (for two-sided tests) for each look. Same as CumAlpha if right-tailed one-sided test. Only makes
#'     sense to use for two-sided asymmetric tests. East Horizon Explore: Only available if `Tail Type =
#'     Right-tailed`. Two-sided tests do not exist, so this variable is not useful: use CumAlpha instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Right-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
#'   \item{CumCompleters}{Vector of Integer. Vector of length `LookInfo$NumLooks`, containing the cumulative number
#'     of completers for each look. East Horizon Explore: Not available for `Endpoint Type = Time-to-Event` and for
#'     Vaccine Efficacy (`Endpoint Type = Binary` with Lower Value and `Test = 1 - Ratio of Proportions or 1 -
#'     Ratio of Poisson Rates`). East Horizon Design: Not available for `Time-to-Event` tests.}
#'   \item{CumEvents}{Vector of Integer. Vector of length `LookInfo$NumLooks`, containing the cumulative number of
#'     events for each look. East Horizon Explore: Only available for `Endpoint Type = Time-to-Event` and for
#'     Vaccine Efficacy (`Endpoint Type = Binary` with Lower Value and `Test = 1 - Ratio of Proportions or 1 -
#'     Ratio of Poisson Rates`). East Horizon Design: Only available for `Time-to-Event` tests.}
#'   \item{LookTime}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the calendar time of each
#'     time-based look. East Horizon Design: Only available for `Time-to-Event` tests if `Look Fix Option =
#'     Time-based`.}
#'   \item{RejType}{Integer. Rejection type. East Horizon Explore: Possible values: – `0`: One-sided efficacy
#'     upper. – `1`: One-sided futility upper. – `2`: One-sided efficacy lower. – `3`: One-sided futility lower. –
#'     `4`: One-sided efficacy upper, futility lower. – `5`: One-sided efficacy lower, futility upper. East Horizon
#'     Design: Possible values: – `0`: One-sided efficacy upper. – `1`: One-sided futility upper. – `2`: One-sided
#'     efficacy lower. – `3`: One-sided futility lower. – `4`: One-sided efficacy upper, futility lower. – `5`:
#'     One-sided efficacy lower, futility upper. – `6`: Two-sided efficacy only. – `7`: Two-sided futility only. –
#'     `8`: Two-sided efficacy, futility. – `9`: Equivalence.}
#'   \item{EffBdryScale}{Integer. Efficacy boundary scale. East Horizon Explore: Possible values: – `0`: Z scale.
#'     East Horizon Design: Possible values: – `0`: Z scale. – `1`: p-value scale.}
#'   \item{EffBdry}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the efficacy boundary
#'     values (for one-sided tests) for each look. East Horizon Explore: Set to `NA` for `Endpoint Type =
#'     Continuous with Repeated Measures`. East Horizon Design: Only available if `Test Type = One-sided`.}
#'   \item{EffBdryLower}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the lower efficacy
#'     boundary values (for two-sided tests) for each look. East Horizon Explore: Only available if `Tail Type =
#'     Left-tailed`. Two-sided tests do not exist, so this variable is not useful: use EffBdry instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Left-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
#'   \item{EffBdryUpper}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the upper efficacy
#'     boundary values (for two-sided tests) for each look. East Horizon Explore: Only available if `Tail Type =
#'     Right-tailed`. Two-sided tests do not exist, so this variable is not useful: use EffBdry instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Right-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
#'   \item{FutBdryScale}{Integer. Futility boundary scale. East Horizon Explore: Possible values: – `0`: Z scale. –
#'     `2`: Delta scale. East Horizon Design: Possible values: – `0`: Z scale. – `1`: p-value scale. – `2`: Delta
#'     scale. – `3`: Conditional power scale.}
#'   \item{CPDeltaOption}{Integer. Delta option for conditional power computation: 0 = design Delta; 1 = estimated
#'     Delta. East Horizon Design only; available when the futility boundary scale is conditional power.}
#'   \item{FutBdry}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the futility boundary
#'     values (for one-sided tests) for each look. East Horizon Design: Only available if `Test Type = One-sided`.}
#'   \item{FutBdryLower}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the lower futility
#'     boundary values (for two-sided tests) for each look. East Horizon Explore: Only available if `Tail Type =
#'     Left-tailed`. Two-sided tests do not exist, so this variable is not useful: use FutBdry instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Left-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
#'   \item{FutBdryUpper}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the upper futility
#'     boundary values (for two-sided tests) for each look. East Horizon Explore: Only available if `Tail Type =
#'     Right-tailed`. Two-sided tests do not exist, so this variable is not useful: use FutBdry instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Right-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
#'   \item{BindingType}{Integer. Binding type: - `0`: Non-binding. - `1`: Binding.}
#' }
#'
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
#'      \item{UserParam$dLowerCutoffForFutility}{A value (0,1) that specified the lower cutoff for the futility
#'        check. Below this value will declare futility.}
#'  }
#'  If user variables are not specified then a Beta( 1, 1 ) prior is utilized for both standard of care and
#'    experimental.
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore).}
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale.}
#'   \item{Delta}{Estimated experimental-minus-control treatment effect (proportion difference for binary outcomes;
#'     mean difference for continuous outcomes).}
#'   \item{CtrlCompleters}{Number of completers in the control arm. Required when the selected conditional-power
#'     rule uses the estimated treatment effect.}
#'   \item{TrmtCompleters}{Number of completers in the experimental arm. Required when the selected
#'     conditional-power rule uses the estimated treatment effect.}
#'   \item{CtrlPi}{Observed proportion of responders in the control arm. Return only when required by the selected
#'     conditional-power rule.}
#'   \item{AnalysisTime}{Optional numeric calendar time of the analysis: the look time at an interim analysis and
#'     the study duration at the final analysis. Compute and return this value in the R function.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#'   \item{StdError}{Numeric standard error of the estimated treatment effect. Required when the chosen
#'     conditional-power rule uses the estimated effect and its standard error.}
#' }
#'
#' @details For ordinary analysis designs, return either Decision to apply custom stopping logic or TestStat to let
#'   the engine apply its boundaries. Delta, event/completer counts, and standard errors may also be required for
#'   Delta-scale or conditional-power futility. Sample size re-estimation designs require a decision and the
#'   re-estimated total event/completer count. This example may use only a subset of the documented design fields.
######################################################################################################################## .

AnalyzeUsingBetaBinomial <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    library( CyneRgy )

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
    vOutcomesExp <- vPatientOutcome[ vPatientTreatment == 1 ]


    # Step 3 -Perform the desired analysis - for this case a Bayesian analysis.  If Posterior Probability is > Cutoff --> Efficacy ####
    # The function PerformAnalysisBetaBinomial is provided below in this file.
    lRet <- ProbExpGreaterCtrlBeta( vOutcomesCtrl, vOutcomesExp, UserParam$dAlphaCtrl, UserParam$dBetaCtrl, UserParam$dAlphaExp, UserParam$dBetaExp )

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
        bIAEfficacyCondition = lRet$dPostProb > UserParam$dUpperCutoffEfficacy,
        bIAFutilityCondition = lRet$dPostProb < UserParam$dLowerCutoffForFutility,
        bFAEfficacyCondition = lRet$dPostProb > UserParam$dUpperCutoffEfficacy
    )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    nErrorCode <- 0

    return( list( TestStat = as.double( lRet$dPostProb ), ErrorCode = as.integer( nErrorCode ), Decision = as.integer( nDecision ), Delta = as.double( lRet$dDelta ) ) )
}




# Function for performing statistical analysis using a Beta-Binomial Bayesian model

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

    # Compute Delta: mean( Pi_E ) - mean( Pi_C )
    dDelta <- ( dAlphaExp / ( dAlphaExp + dBetaExp ) ) - ( dAlphaCtrl / ( dAlphaCtrl + dBetaCtrl ) )
    return( list( dPostProb = dPostProb, dDelta = dDelta ) )
}




# Function to compute Bayesian predictive probability of success
ComputeBayesianPredictiveProbabilityWithBayesianAnalysis <- function( dataS, dataE, priorAlphaS, priorBetaS, priorAlphaE, priorBetaE, nQtyOfPatsS, nQtyOfPatsE, nSimulations, finalBoundary, lAnalysisParams ) {
    # Compute the posterior parameters based on observed data
    posteriorAlphaS <- priorAlphaS + sum( dataS )
    posteriorBetaS <- priorBetaS + length( dataS ) - sum( dataS )

    posteriorAlphaE <- priorAlphaE + sum( dataE )
    posteriorBetaE <- priorBetaE + length( dataE ) - sum( dataE )

    # lAnalysisParams <- list( dAlphaCtrl = priorAlphaS,
    #                         dBetaCtrl  = priorBetaS,
    #                         dAlphaExp  = priorAlphaE,
    #                         dBetaExp   = priorBetaE )

    # Initialize counters for successful trials
    successfulTrials <- 0

    # Simulate the remaining trials and compute the predictive probability
    for ( i in 1:nSimulations ) {
        # Sample response rates from posterior distributions
        posteriorRateS <- stats::rbeta( 1, posteriorAlphaS, posteriorBetaS )
        posteriorRateE <- stats::rbeta( 1, posteriorAlphaE, posteriorBetaE )

        # Simulate patient outcomes for the current virtual trial based on sampled rates
        # The data at the end of the trial is a combination of the data at the interim, dataS, and the simulated data to the end of the trial, remainingDataS
        remainingDataS <- stats::rbinom( nQtyOfPatsS - length( dataS ), size = 1, prob = posteriorRateS )
        combinedDataS <- c( dataS, remainingDataS )

        remainingDataE <- stats::rbinom( nQtyOfPatsE - length( dataE ), size = 1, prob = posteriorRateE )
        combinedDataE <- c( dataE, remainingDataE )


        # Perform the analysis with combined data to check if the trial is successful
        result <- ProbExpGreaterCtrlBeta(
            combinedDataS, combinedDataE,
            lAnalysisParams$dAlphaCtrl, lAnalysisParams$dBetaCtrl,
            lAnalysisParams$dAlphaExp, lAnalysisParams$dBetaExp
        )

        # Check if the result meets the cutoff for success
        if ( result$dPostProb >= finalBoundary ) {
            successfulTrials <- successfulTrials + 1
        }
    }

    # Compute the Bayesian predictive probability of success
    predictiveProbabilityS <- successfulTrials / nSimulations

    # Return the result
    return( list( predictiveProbabilityS = predictiveProbabilityS ) )
}
