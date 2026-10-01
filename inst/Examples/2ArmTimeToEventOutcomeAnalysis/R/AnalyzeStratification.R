######################################################################################################################## .
#' @name AnalyzeStratification
#' @title Analyze time-to-event subject outcomes
#' @description Computes a stratified log-rank test, hazard ratio, analysis time and decision at a given interim
#'   analysis.
#'
#' \enumerate{
#'   \item Prepares observed data up to the interim analysis time
#'   \item Computes the test statistic
#'   \item Computes the HR
#'   \item Generates a decision at the current look (efficacy, continue, or futility at final look)
#' }
#' @author Anoop Singh Rawat, Shubham Lahoti, and Gabriel Potvin
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject.}
#'   \item{DropOutTime}{Numeric vector of generated dropout times measured from each subject's enrollment, with one
#'     element per subject. Inf indicates no dropout.}
#' }
#'
#' Subject-level stratification factor columns are named by DesignParam$TestStratFactors (Explore) or
#'   names(DesignParam$StratFactors) (Design), with each row containing the corresponding subject's factor levels.
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
#'   \item{AllocInfo}{Vector of Numeric. Vector of length equal to the number of experimental arms (number of arms -
#'     1), containing the ratios of the experimental group sample sizes to the control group sample size.}
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
#'   \item{LookFixOption}{Integer. Look option: - `0`: Event-based. - `1`: Time-based. East Horizon Explore: Not
#'     available. East Horizon Design: Only available for `Time-to-Event` tests.}
#'   \item{MaxEvents}{Integer maximum number of events in the trial.}
#'   \item{MaxStudyDur}{Integer. Maximum study duration. East Horizon Explore: Not available. East Horizon Design:
#'     Only available for `Time-to-Event` tests with `Look Fix Option = 1 (Time-based)`.}
#'   \item{FollowUpType}{Integer. Follow-up type: – `0`: Until the end of the study. – `1`: For a fixed period.
#'     East Horizon Explore: Only available for `Endpoint Type = Time-to-Event`. East Horizon Design: Only
#'     available for `Time-to-Event` tests.}
#'   \item{FollowUpDur}{Numeric. Follow-up duration. East Horizon Explore: Only available for `Endpoint Type =
#'     Time-to-Event`. East Horizon Design: Only available for `Time-to-Event` tests.}
#'   \item{TestStatType}{Integer. Test statistic type. For `Time-to-Event` tests: - `0`: Logrank. - `1`: Wilcoxon
#'     Gehan. - `2`: Harrington Fleming. - `3`: Stratified Logrank. - `4`: Stratified Wilcoxon Gehan. - `5`:
#'     Stratified Harrington Fleming. For `Continuous` test: - `3`: Z-test. - `4`: t-test. For `Binary` test: -
#'     `5`: Wald. - `6`: Score. East Horizon Explore: Not available. East Horizon Design: Not available for `Test =
#'     Difference of Proportions or Odds Ratio of Proportions` (Binary).}
#'   \item{HFParam1}{Numeric. First parameter of Harrington Fleming. East Horizon Explore: Not available. East
#'     Horizon Design: Only available for `Time-to-Event` tests.}
#'   \item{HFParam2}{Numeric. Second parameter of Harrington Fleming. East Horizon Explore: Not available. East
#'     Horizon Design: Only available for `Time-to-Event` tests.}
#'   \item{TrtEffNull}{Numeric. Treatment effect under null on natural scale. East Horizon Explore: Not available
#'     for `Endpoint Type = Continuous with Repeated Measures`. Set to `0` for `Trial Type = Superiority`. Set to
#'     `Delta_0 = log(HR_0)` for `Endpoint Type = Time-to-Event`. Set to `1 - rho_0` for Vaccine Efficacy
#'     (`Endpoint Type = Binary` with Lower Value and `Test = 1 - Ratio of Proportions or 1 - Ratio of Poisson
#'     Rates`). East Horizon Design: Set to `0` for `Trial Type = Superiority`. Set to `Delta_0 = log(HR_0)` for
#'     `Time-to-Event` tests.}
#'   \item{NumHzrdPrd}{Integer. Number of Hazard pieces. East Horizon Explore: Not available. East Horizon Design:
#'     Only available for `Time-to-Event` tests.}
#'   \item{PrdAt}{Numeric. Period starting value. East Horizon Explore: Not available. East Horizon Design: Only
#'     available for `Time-to-Event` tests.}
#'   \item{LambdaC}{Numeric. Control Hazard rate. East Horizon Explore: Not available. East Horizon Design: Only
#'     available for `Time-to-Event` tests.}
#'   \item{NumStratFactors}{Integer. Number of stratification factors. East Horizon Explore: Only available for
#'     `Endpoint Type = Time-to-Event` with `Stratification` turned on. East Horizon Design: Only available for
#'     `Time-to-Event` tests with `Stratification` turned on.}
#'   \item{StratFactors}{Named List. Named list of length `NumStratFactors`, indicating stratification factors
#'     details. For example, `StratFactors["Factor1"]` is an array of level names for Factor 1. East Horizon
#'     Explore: Only available for `Endpoint Type = Time-to-Event` with `Stratification` turned on. East Horizon
#'     Design: Only available for `Time-to-Event` tests with `Stratification` turned on.}
#'   \item{TestStratFactors}{Array of Character. Array of factor names included in the analysis. Length is between
#'     1 and `NumStratFactors`. East Horizon Explore: Only available for `Endpoint Type = Time-to-Event` with
#'     `Stratification` turned on. East Horizon Design: Not available.}
#'   \item{NumSubPops}{Integer. Number of subpopulations. East Horizon Explore: Only available for `Endpoint Type =
#'     Time-to-Event` with `Stratification` and `Subpopulations` turned on. East Horizon Design: Not available.}
#'   \item{SubPops}{Named List. Named list of length equal to the number of subpopulations, indicating factor
#'     details. For example, `SubPops["Factor1"]` is an array of level names for Factor 1. East Horizon Explore:
#'     Only available for `Endpoint Type = Time-to-Event` with `Stratification` and `Subpopulations` turned on.
#'     East Horizon Design: Not available.}
#'   \item{SubpopName}{Array of Character. Array of length equal to the number of subpopulations, indicating the
#'     subpopulation names. East Horizon Explore: Only available for `Endpoint Type = Time-to-Event` with
#'     `Stratification` and `Subpopulations` turned on. East Horizon Design: Not available.}
#'   \item{WindCond}{Integer. Winning condition: - `1`: At least subpopulation 1. - `2`: At least subpopulation 2.
#'     - `3`: At least subpopulation 3. - `4`: At least subpopulation 4. - `5`: At least one subpopulation. - `6`:
#'     At least two subpopulations. - `7`: At least three subpopulations. - `8`: At least four subpopulations. -
#'     `9`: At least full population. - `10`: All populations. East Horizon Explore: Only available for `Endpoint
#'     Type = Time-to-Event` with `Stratification` and `Subpopulations` turned on. East Horizon Design: Not
#'     available.}
#'   \item{PlanEndTrial}{Integer. Planned end of trial: - `1`: Subpopulation 1. - `2`: Subpopulation 2. - `3`:
#'     Subpopulation 3. - `4`: Subpopulation 4. - `5`: Full population. East Horizon Explore: Only available for
#'     `Endpoint Type = Time-to-Event` with `Stratification` and `Subpopulations` turned on. East Horizon Design:
#'     Not available.}
#'   \item{TransitionMatrix}{Matrix of Numeric. Transition matrix between all populations, including full
#'     population and subpopulations. Dimension of `(NumSubPops + 1) x (NumSubPops + 1)`. East Horizon Explore:
#'     Only available for `Endpoint Type = Time-to-Event` with `Stratification` and `Subpopulations` turned on.
#'     East Horizon Design: Not available.}
#'   \item{PropAlpha}{Vector of Numeric. Vector of length `(NumSubPops + 1)`, indicating the proportion of alpha
#'     for all populations, including full population and subpopulations. East Horizon Explore: Only available for
#'     `Endpoint Type = Time-to-Event` with `Stratification` and `Subpopulations` turned on. East Horizon Design:
#'     Not available.}
#' }
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
#'   \item{CumEvents}{Integer cumulative number of events at the current look. These examples also accept a vector of
#'     per-look cumulative event counts, indexed by LookInfo$CurrLookIndex. East Horizon Explore: Only available for
#'     `Endpoint Type = Time-to-Event` and for Vaccine Efficacy (`Endpoint Type = Binary` with Lower Value and `Test =
#'     1 - Ratio of Proportions or 1 - Ratio of Poisson Rates`). East Horizon Design: Only available for `Time-to-
#'     Event` tests.}
#'   \item{LookTime}{Numeric calendar time of the current look. East Horizon Design: Only available for `Time-to-Event`
#'     tests if `Look Fix Option = Time-based`.}
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
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore).}
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale.}
#'   \item{HR}{Estimated treatment-to-control hazard ratio.}
#'   \item{Delta}{Estimated log hazard ratio (natural logarithm of HR).}
#'   \item{AnalysisTime}{Optional numeric calendar time of the analysis: the look time at an interim analysis and
#'     the study duration at the final analysis. Compute and return this value in the R function.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#'   \item{StdError}{Numeric standard error of the estimated treatment effect. Required when the chosen
#'     conditional-power rule uses the estimated effect and its standard error.}
#'   \item{CtrlCompleters}{Number of completers in the control arm. Required when the selected conditional-power
#'     rule uses the estimated treatment effect.}
#'   \item{TrmtCompleters}{Number of completers in the experimental arm. Required when the selected
#'     conditional-power rule uses the estimated treatment effect.}
#'   \item{CtrlPi}{Observed proportion of responders in the control arm. Return only when required by the selected
#'     conditional-power rule.}
#' }
#' @details This example applies one-sided efficacy boundaries on the Z scale. Extend its boundary logic
#'   before using p-value-scale boundaries or two-sided designs.
#'
#' For ordinary analysis designs, return either Decision to apply custom stopping logic or TestStat to let
#'   the engine apply its boundaries. Delta, event/completer counts, and standard errors may also be required for
#'   Delta-scale or conditional-power futility. Sample size re-estimation designs require a decision and the
#'   re-estimated total event/completer count.
#'
#' SimData also contains the subject-level stratification-factor columns named in DesignParam$StratFactors. When
#'   subpopulations are enabled, Decision, TestStat, and HR are named lists with one element for the full
#'   population and each subpopulation, using the population names defined by the design.
######################################################################################################################## .

AnalyzeStratification <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    nErrorCode <- 0
    nDecision <- 0
    dTestStatistic <- 0
    dTimeOfAnalysis <- 0

    # Determine number of events for analysis
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfEvents <- LookInfo$CumEvents
        if ( length( nQtyOfEvents ) > 1 ) {
            nQtyOfEvents <- nQtyOfEvents[ nLookIndex ]
        }
    } else {
        nQtyOfLooks <- 1
        nLookIndex <- 1
        nQtyOfEvents <- DesignParam$MaxEvents
    }

    # Prepare analysis dataset
    SimData$TimeOfEvent <- SimData$ArrivalTime + SimData$SurvivalTime
    # Censor survival observations at dropout and any fixed follow-up limit.
    SimData$TimeOfObservation <- SimData$TimeOfEvent
    SimData$Event <- 1L
    vCensorTimes <- rep( Inf, nrow( SimData ) )
    if ( "DropOutTime" %in% names( SimData ) ) {
        vCensorTimes <- SimData$DropOutTime
    }
    if ( !is.null( DesignParam$FollowUpType ) && DesignParam$FollowUpType == 1 ) {
        vCensorTimes <- pmin( vCensorTimes, DesignParam$FollowUpDur )
    }
    SimData$Event <- as.integer( SimData$SurvivalTime <= vCensorTimes )
    SimData$TimeOfObservation <- SimData$ArrivalTime + pmin( SimData$SurvivalTime, vCensorTimes )
    SimData <- SimData[ order( SimData$TimeOfObservation ), ]
    vObservedEventTimes <- SimData$TimeOfEvent[ SimData$Event == 1 ]
    if ( nQtyOfEvents < 1 || nQtyOfEvents > length( vObservedEventTimes ) ) {
        return( list( ErrorCode = 1L ) )
    }
    dTimeOfAnalysis <- vObservedEventTimes[ nQtyOfEvents ]
    SimData <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ]
    SimData$Event <- SimData$Event * as.integer( SimData$TimeOfEvent <= dTimeOfAnalysis )
    SimData$ObservedTime <- ifelse( SimData$TimeOfObservation > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfObservation - SimData$ArrivalTime )

    vObservedTime <- SimData$ObservedTime
    vEvent <- SimData$Event
    vTreatmentID <- SimData$TreatmentID

    # Determine which stratification factors to use
    if ( !all( is.na( DesignParam$TestStratFactors ) ) ) {
        vStratFactors <- DesignParam$TestStratFactors
    } else {
        # For Design, as TestStratFactors is NA
        vStratFactors <- names( DesignParam$StratFactors )
    }

    # Convert each stratification column to a factor
    for ( strFactor in vStratFactors ) {
        SimData[[ strFactor ]] <- factor( SimData[[ strFactor ]],
            levels = unique( SimData[[ strFactor ]] )
        )
    }

    # Construct the formula for strata dynamically
    fStrataFormula <- stats::as.formula(
        paste0(
            "survival::Surv(vObservedTime, vEvent) ~ vTreatmentID + ",
            paste0( "survival::strata(`", vStratFactors, "`)", collapse = " + " )
        )
    )

    # Perform stratified log-rank test
    cSurvDiff <- survival::survdiff( fStrataFormula, data = SimData )
    dTestStatistic <- sqrt( cSurvDiff$chisq )

    # Compute Hazard Ratio (HR)
    cCoxFit <- survival::coxph( fStrataFormula, data = SimData )
    dHR <- exp( stats::coef( cCoxFit ) )

    dTestStatistic <- ifelse( unname( dHR ) < 1, dTestStatistic * -1, dTestStatistic )

    # Decision logic based on boundaries
    if ( !is.na( dTestStatistic ) ) {
        dEffBdry <- DesignParam$CriticalPoint
        if ( !is.null( LookInfo ) ) {
            dEffBdry <- LookInfo$EffBdry[ nLookIndex ]
        }

        bEfficacyCondition <- FALSE
        if ( !is.null( dEffBdry ) && !is.na( dEffBdry ) ) {
            if ( DesignParam$TailType == 0 ) {
                bEfficacyCondition <- dTestStatistic < dEffBdry
            } else {
                bEfficacyCondition <- dTestStatistic > dEffBdry
            }
        }
        strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
            bIAEfficacyCondition = bEfficacyCondition,
            bFAEfficacyCondition = bEfficacyCondition
        )
        nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    }

    lRet <- list(
        TestStat = as.double( dTestStatistic ),
        AnalysisTime = as.double( dTimeOfAnalysis ),
        HR = as.double( dHR ),
        Decision = as.integer( nDecision ),
        ErrorCode = as.integer( nErrorCode )
    )
    return( lRet )
}
