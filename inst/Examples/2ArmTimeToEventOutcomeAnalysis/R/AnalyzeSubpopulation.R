######################################################################################################################## .
#' @name AnalyzeSubpopulation
#'
#' @title Analyze time-to-event subject outcomes
#'
#' @description Computes test statistic, hazard ratio, analysis time and decision at a given interim analysis while
#'   having multiple subpopulations.
#'
#' This function:
#' \enumerate{
#'   \item Determines the number of events required at the current interim look
#'         (based on \code{LookInfo} if provided; otherwise on \code{DesignParam}).
#'
#'   \item Prepares the analysis dataset by:
#'         \itemize{
#'           \item Computing event times and observed follow-up
#'           \item Ordering subjects by event time to determine analysis cutoff
#'           \item Censoring subjects whose events occur after the analysis time
#'           \item Restricting subjects to those enrolled before the cutoff
#'         }
#'
#'   \item Reads all pre-specified population definitions:
#'         \itemize{
#'           \item Full population
#'           \item Subpopulations in \code{DesignParam$SubPops}
#'           \item Alpha allocation weights for GMCP
#'         }
#'
#'   \item Constructs logical filters that identify subjects belonging to the
#'         full population and each subpopulation.
#'
#'   \item Identifies all stratification factors used by any subpopulation and
#'         ensures they are appropriately factorized.
#'
#'   \item For each population (full population + all subpops), it:
#'         \itemize{
#'           \item Selects the applicable stratification factors
#'           \item Constructs a dynamic stratified log-rank formula
#'           \item Computes the standardized test statistic (sqrt of chi-square)
#'           \item Fits a stratified Cox model and extracts the hazard ratio (HR)
#'         }
#'
#'   \item Collects population-specific test statistics and applies the graphical
#'         multiple testing procedure via \code{ComputeGMCPDecisions()}.
#'
#'   \item Converts GMCP rejection flags into population-specific decision codes:
#'         \itemize{
#'           \item \code{2} = reject null hypothesis (efficacy)
#'           \item \code{0} = continue at interim look
#'           \item \code{3} = no rejection at final look (futility)
#'         }
#'
#'   \item Returns all computed outputs for each population:
#'         \itemize{
#'           \item Test statistic
#'           \item Hazard ratio
#'           \item GMCP decision code
#'         }
#'
#'   \item Returns the overall analysis time at which the interim or final
#'         evaluation was conducted, along with an error flag.
#' }
#'
#' @author Anoop Singh Rawat, Shubham Lahoti, and Gabriel Potvin
#'
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the native fields below when applicable,
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
#'   \item{OS}{Optional custom numeric vector of subject overall-survival times measured from enrollment. Available
#'     when the multi-state response generator returns OS.}
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
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Decision}{Named list of integer boundary-crossing codes, with one element for the full
#'     population and each subpopulation, using the population names defined by the design: 0 = no
#'     boundary crossed; 1 = lower efficacy boundary crossed; 2 = upper efficacy boundary crossed; 3 =
#'     futility boundary crossed; 4 = equivalence boundary crossed (unavailable in East Horizon
#'     Explore).}
#'   \item{TestStat}{Named list of numeric test statistics on the Wald (Z) scale, with one element for
#'     the full population and each subpopulation, using the population names defined by the design.}
#'   \item{HR}{Named list of estimated treatment-to-control hazard ratios, with one element for the full
#'     population and each subpopulation, using the population names defined by the design.}
#'   \item{Delta}{Estimated log hazard ratio (natural logarithm of HR).}
#'   \item{CtrlEvents}{Number of observed events in the control arm.}
#'   \item{TrmtEvents}{Number of observed events in the experimental arm.}
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
#'
#' @details For ordinary analysis designs, return either Decision to apply custom stopping logic or TestStat to let
#'   the engine apply its boundaries. Delta, event/completer counts, and standard errors may also be required for
#'   Delta-scale or conditional-power futility. Sample size re-estimation designs require a decision and the
#'   re-estimated total event/completer count. This example may use only a subset of the documented design fields.
#'
#' SimData also contains the subject-level stratification-factor columns named in DesignParam$StratFactors. When
#'   subpopulations are enabled, Decision, TestStat, and HR are named lists with one element for the full
#'   population and each subpopulation, using the population names defined by the design.
######################################################################################################################## .

AnalyzeSubpopulation <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    nErrorCode <- 0
    dTimeOfAnalysis <- 0

    # Step 1: Determine number of events for analysis
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        vCumEvents <- LookInfo$CumEvents
        nQtyOfEvents <- vCumEvents[ nLookIndex ]
    } else {
        nQtyOfLooks <- 1
        nLookIndex <- 1
        nQtyOfEvents <- DesignParam$MaxEvents
    }

    # Step 2: Prepare analysis dataset
    SimData$TimeOfEvent <- SimData$ArrivalTime + SimData$SurvivalTime
    SimData <- SimData[ order( SimData$TimeOfEvent ), ]
    dTimeOfAnalysis <- SimData[ nQtyOfEvents, ]$TimeOfEvent
    SimData <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ]
    SimData$Event <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )
    SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis,
        dTimeOfAnalysis - SimData$ArrivalTime,
        SimData$TimeOfEvent - SimData$ArrivalTime
    )

    # Step 3: Read population inputs
    nNumSubPops <- DesignParam$NumSubPops
    vPopNames <- DesignParam$SubpopName
    lSubPops <- DesignParam$SubPops
    vPropAlpha <- DesignParam$PropAlpha

    # Step 4: Create population filters
    lPopFilters <- list( )
    lPopFilters[[ "Full Population" ]] <- rep( TRUE, nrow( SimData ) )

    if ( nNumSubPops > 0 ) {
        for ( strSubpopName in names( lSubPops ) ) {
            vSubpopFilter <- rep( TRUE, nrow( SimData ) )
            for ( strFactorName in names( lSubPops[[ strSubpopName ]] ) ) {
                vAllowedValues <- lSubPops[[ strSubpopName ]][[ strFactorName ]]
                vSubpopFilter <- vSubpopFilter & ( SimData[[ strFactorName ]] %in% vAllowedValues )
            }
            lPopFilters[[ strSubpopName ]] <- vSubpopFilter
        }
    }

    # Step 5: Determine all possible factors across all subpopulations
    vAllFactors <- unique( unlist( lapply( lSubPops, names ) ) )
    for ( strFactor in vAllFactors ) {
        if ( strFactor %in% names( SimData ) ) {
            SimData[[ strFactor ]] <- factor( SimData[[ strFactor ]],
                levels = unique( SimData[[ strFactor ]] )
            )
        }
    }

    # Step 6: Initialize output lists
    lTestStatistics <- list( )
    lHazardRatios <- list( )
    lDecisions <- list( )
    vTestStats <- c( )
    vPopOrder <- c( )

    # Step 7: Compute test statistics and collect populations
    for ( strPopName in names( lPopFilters ) ) {
        dfSubsetData <- SimData[ lPopFilters[[ strPopName ]], ]

        if ( nrow( dfSubsetData ) > 0 ) {
            # Identify stratification factors
            if ( strPopName == "Full Population" ) {
                vCurrentStratFactors <- vAllFactors
            } else {
                vCurrentStratFactors <- names( lSubPops[[ strPopName ]] )
            }
            vCurrentStratFactors <- vCurrentStratFactors[ vCurrentStratFactors %in% names( dfSubsetData ) ]

            # Build survival formula
            if ( length( vCurrentStratFactors ) > 0 ) {
                fStrataFormula <- stats::as.formula(
                    paste0(
                        "survival::Surv(ObservedTime, Event) ~ TreatmentID + ",
                        paste0( "survival::strata(`", vCurrentStratFactors, "`)", collapse = " + " )
                    )
                )
            } else {
                fStrataFormula <- survival::Surv( ObservedTime, Event ) ~ TreatmentID
            }

            # Estimate the hazard ratio
            cCoxFit <- survival::coxph( fStrataFormula, data = dfSubsetData )
            dHazardRatio <- exp( stats::coef( cCoxFit ) )

            # Run the log-rank test
            cSurvDiff <- survival::survdiff( fStrataFormula, data = dfSubsetData )
            dTestStat <- sqrt( cSurvDiff$chisq )
            dTestStat <- ifelse( unname( dHazardRatio ) < 1, dTestStat * -1, dTestStat )

            # Store outputs in named lists
            lTestStatistics[[ strPopName ]] <- as.double( dTestStat )
            lHazardRatios[[ strPopName ]] <- as.double( dHazardRatio )
            vTestStats <- c( vTestStats, dTestStat )
            vPopOrder <- c( vPopOrder, strPopName )
        } else {
            lTestStatistics[[ strPopName ]] <- NA
            lHazardRatios[[ strPopName ]] <- NA
            vTestStats <- c( vTestStats, NA )
            vPopOrder <- c( vPopOrder, strPopName )
        }
    }

    # Step 8: Compute GMCP decisions
    lGMCPResult <- ComputeGMCPDecisions(
        vTestStats  = vTestStats,
        nTailType   = DesignParam$TailType,
        dAlpha      = DesignParam$Alpha,
        vWeights    = DesignParam$PropAlpha,
        mTransition = DesignParam$TransitionMatrix
    )

    # Step 9: Map GMCP decisions to populations and store them in a named list
    for ( iPop in seq_along( vPopOrder ) ) {
        strPopName <- vPopOrder[ iPop ]
        nGMCPFlag <- lGMCPResult$decisionFlag[ iPop ]

        if ( nGMCPFlag == 1 ) {
            nFinalDecision <- ifelse( DesignParam$TailType == 0, 1L, 2L )
        } else if ( nLookIndex == nQtyOfLooks ) {
            nFinalDecision <- 3
        } else {
            nFinalDecision <- 0
        }

        lDecisions[[ strPopName ]] <- as.integer( nFinalDecision )
    }

    # Step 10: Return results
    lRet <- list(
        Decision = as.list( lDecisions ),
        TestStat = as.list( lTestStatistics ),
        HR = as.list( lHazardRatios ),
        AnalysisTime = as.double( dTimeOfAnalysis ),
        ErrorCode = as.integer( nErrorCode )
    )

    return( lRet )
}

ComputeGMCPDecisions <- function( vTestStats, nTailType, dAlpha, vWeights, mTransition ) {
    bTestStatMissing <- is.nan( vTestStats ) | is.na( vTestStats )
    if ( any( bTestStatMissing ) ) {
        vTestStats[ which( bTestStatMissing == TRUE ) ] <- ifelse( nTailType == 0, Inf, -Inf )
    }

    # Compute raw p-values
    if ( nTailType == 0 ) {
        vRawPValues <- stats::pnorm( q = vTestStats, lower.tail = TRUE )
    } else {
        vRawPValues <- stats::pnorm( q = vTestStats, lower.tail = FALSE )
    }

    # Create the graph object and apply the gMCP procedure
    cGraph <- gMCPLite::matrix2graph( m = mTransition, weights = vWeights )
    cOutput <- gMCPLite::gMCP(
        graph = cGraph, pvalues = vRawPValues,
        test = "Bonferroni", alpha = dAlpha
    )

    lRet <- list(
        raw.p.values = vRawPValues,
        adj.p.values = cOutput@adjPValues,
        decisionFlag = as.numeric( cOutput@rejected )
    )
    return( lRet )
}
