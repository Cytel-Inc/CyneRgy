######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template: Analyze time-to-event subject outcomes with sample size re-estimation
#' @description Analyze time-to-event subject outcomes. Use this template as a starting point for custom logic.
#'   Preserve the engine-supplied argument names and access named list elements by name. Supply additional
#'   user-defined inputs through UserParam.
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#'   The documented columns below are the engine-defined columns for this integration point; conditional columns are
#'   marked in their descriptions. Columns returned by custom simulation functions are passed through as well.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject.}
#'   \item{ClndrRespTime}{Numeric vector of event or censoring times on the calendar scale, with one element per
#'     subject.}
#'   \item{CensorInd}{Integer vector of censor indicators adjusted for the current analysis look: 0 = censored;
#'     1 = event.}
#'   \item{CensorIndOrg}{Original integer vector of censor indicators before analysis-time adjustment: 0 =
#'     censored; 1 = event.}
#'   \item{DropOutTime}{Numeric vector of generated dropout times measured from each subject's enrollment, with one
#'     element per subject. Inf indicates no dropout.}
#'   \item{LkRespTime}{Numeric event or follow-up time available at the current look.}
#'   \item{LkCensorIndctr}{Integer current-look censor indicator. This is named CensorInd instead when accrual or
#'     dropout processing applies its display-name mapping.}
#'   \item{ArrvRandNum}{Numeric enrollment random numbers. Present with built-in enrollment generation.}
#'   \item{AllocRandNum}{Numeric allocation random numbers. Present with randomized built-in allocation.}
#'   \item{RespRandNum}{Numeric event-time random numbers. Present with built-in survival generation.}
#'   \item{RespPrdRandNum}{Numeric survival-period random numbers. Present with piecewise built-in survival
#'     generation.}
#'   \item{DropOutPrdRandNum}{Numeric dropout-period random numbers. Present with built-in dropout generation.}
#'   \item{DropOutDORandNum}{Numeric dropout-time random numbers. Present with built-in dropout generation.}
#'   \item{CensorIndRandNum}{Numeric censor-indicator random numbers. Present with built-in censoring generation.}
#' }
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below.
#' \describe{
#'   \item{TestID}{Integer test identifier supplied by East Horizon for the selected test.}
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{LowerAlpha}{Numeric. Lower Type I Error. Same as Alpha if left-tailed one-sided test. Only makes sense
#'     to use for two-sided asymmetric tests. East Horizon Explore: Only available if `Tail Type = Left-tailed`.
#'     Two-sided tests do not exist, so this variable is not useful: use Alpha instead. East Horizon Design: Only
#'     available if `Test Type = One-sided` and `Tail Type = Left-tailed`, or `Test Type = Two-sided asymmetric`.}
#'   \item{UpperAlpha}{Numeric. Upper Type I Error. Same as Alpha if right-tailed one-sided test. Only makes sense
#'     to use for two-sided asymmetric tests. East Horizon Explore: Only available if `Tail Type = Right-tailed`.
#'     Two-sided tests do not exist, so this variable is not useful: use Alpha instead. East Horizon Design: Only
#'     available if `Test Type = One-sided` and `Tail Type = Right-tailed`, or `Test Type = Two-sided asymmetric`.}
#'   \item{TrialType}{Integer. Trial Type: - `0`: Superiority. - `1`: Non-inferiority. - `2`: Equivalence. - `3`:
#'     Super-superiority. East Horizon Explore: Type 2 (equivalence) does not exist.}
#'   \item{TestType}{Integer. Test Type: - `0`: One-sided. - `1`: Two-sided symmetric. - `2`: Two-sided asymmetric.
#'     East Horizon Explore: Types 1 and 2 (two-sided) do not exist.}
#'   \item{TailType}{Integer. Nature of critical region: - `0`: Left-tailed. - `1`: Right-tailed. East Horizon
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
#'   \item{LookFixOption}{Integer. Look option: - `0`: Event-based. - `1`: Time-based. East Horizon Explore: Available for
#'     non-MAMS designs. East Horizon Design: Only available for `Time-to-Event` tests.}
#'   \item{MaxEvents}{Integer maximum number of events in the trial.}
#'   \item{MaxStudyDur}{Numeric. Maximum study duration; may be `Inf` when no maximum is set. East Horizon Explore:
#'     Available for non-MAMS designs. East Horizon Design: Only available for `Time-to-Event` tests with `Look Fix
#'     Option = 1 (Time-based)`.}
#'   \item{FollowUpType}{Integer. Follow-up type: - `0`: Until the end of the study. - `1`: For a fixed period.
#'     East Horizon Explore: Only available for `Endpoint Type = Time-to-Event`. East Horizon Design: Only
#'     available for `Time-to-Event` tests.}
#'   \item{FollowUpDur}{Numeric. Follow-up duration. East Horizon Explore: Only available for `Endpoint Type =
#'     Time-to-Event`. East Horizon Design: Only available for `Time-to-Event` tests.}
#'   \item{TestStatType}{Integer survival test statistic type: 0 = logrank; 1 = Wilcoxon Gehan; 2 = Harrington Fleming; 3, 4,
#'     and 5 are the corresponding stratified tests. Available in East Horizon Explore and Design.
#'   }
#'   \item{HFParam1}{Numeric. First parameter of Harrington Fleming; meaningful only when `TestStatType = 2`. Available
#'     for two-arm and stratified `Time-to-Event` tests in East Horizon Explore and Design.}
#'   \item{HFParam2}{Numeric. Second parameter of Harrington Fleming; meaningful only when `TestStatType = 2`. Available
#'     for two-arm and stratified `Time-to-Event` tests in East Horizon Explore and Design.}
#'   \item{TrtEffNull}{Numeric null treatment effect on the log hazard-ratio scale; 0 for superiority.
#'   }
#'   \item{NumHzrdPrd}{Integer. Number of hazard pieces. Available for two-arm and stratified `Time-to-Event` tests in East
#'     Horizon Explore and Design.}
#'   \item{PrdAt}{Numeric vector. Starting time of each hazard piece. Available for two-arm and stratified
#'     `Time-to-Event` tests in East Horizon Explore and Design.}
#'   \item{LambdaC}{Numeric vector. Control hazard rate for each hazard piece. Available for two-arm and stratified
#'     `Time-to-Event` tests in East Horizon Explore and Design.}
#'   \item{MaxCompleters}{Integer maximum number of completers. Set to NA for time-to-event tests.}
#'   \item{RespLag}{Numeric response lag. Set to NA for time-to-event tests.}
#'   \item{VarType}{Integer variance type. Set to NA when not applicable.}
#'   \item{Sigma}{Numeric design standard deviation. Set to NA for time-to-event tests.}
#'   \item{SigmaD}{Numeric standard deviation for paired differences. Set to NA when not applicable.}
#'   \item{SigmaLogRatio}{Numeric standard deviation on the log-ratio scale. Set to NA when not applicable.}
#'   \item{CoeffVar}{Numeric coefficient of variation. Set to NA when not applicable.}
#'   \item{UpperEquiLimit}{Numeric upper equivalence limit. Set to NA outside equivalence tests.}
#'   \item{LowerEquiLimit}{Numeric lower equivalence limit. Set to NA outside equivalence tests.}
#'   \item{EquiMargin}{Numeric equivalence margin. Set to NA outside equivalence tests.}
#'   \item{MuC}{Numeric design control mean. Set to NA for time-to-event tests.}
#'   \item{PiC}{Numeric design control response probability. Set to NA for time-to-event tests.}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, for the sample size re-estimation design. Access
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
#'   \item{CumCompleters}{Integer vector of cumulative completers by look. Set to NA when completion counts are not
#'     applicable.}
#'   \item{CumEvents}{Integer vector of cumulative event counts, of length LookInfo$NumLooks.
#'     Index by LookInfo$CurrLookIndex for the current look. East Horizon Explore: Available for
#'     `Endpoint Type = Time-to-Event`. East Horizon Design: Only available for `Time-to-
#'     Event` tests.}
#'   \item{LookTime}{Numeric vector of calendar analysis times, of length LookInfo$NumLooks.
#'     East Horizon Design: Only available for `Time-to-Event`
#'     tests if `Look Fix Option = Time-based`.}
#'   \item{RejType}{Integer. Rejection type. East Horizon Explore: Possible values: - `0`: One-sided efficacy
#'     upper. - `1`: One-sided futility upper. - `2`: One-sided efficacy lower. - `3`: One-sided futility lower. -
#'     `4`: One-sided efficacy upper, futility lower. - `5`: One-sided efficacy lower, futility upper. East Horizon
#'     Design: Possible values: - `0`: One-sided efficacy upper. - `1`: One-sided futility upper. - `2`: One-sided
#'     efficacy lower. - `3`: One-sided futility lower. - `4`: One-sided efficacy upper, futility lower. - `5`:
#'     One-sided efficacy lower, futility upper. - `6`: Two-sided efficacy only. - `7`: Two-sided futility only. -
#'     `8`: Two-sided efficacy, futility. - `9`: Equivalence.}
#'   \item{EffBdryScale}{Integer. Efficacy boundary scale. East Horizon Explore: Possible values: - `0`: Z scale.
#'     East Horizon Design: Possible values: - `0`: Z scale. - `1`: p-value scale.}
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
#'   \item{FutBdryScale}{Integer. Futility boundary scale. East Horizon Explore: Possible values: - `0`: Z scale. -
#'     `2`: Delta scale. East Horizon Design: Possible values: - `0`: Z scale. - `1`: p-value scale. - `2`: Delta
#'     scale. - `3`: Conditional power scale.}
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
#' @param AdaptInfo Named list of sample size re-estimation parameters for this two-arm time-to-event design. Access elements by name, for example `AdaptInfo$SSRFuncScale`. Available only for
#'   designs with sample size re-estimation.
#' \describe{
#'   \item{AdaptMethod}{Integer. Adaptation method: - `1`: Cui, Hung, and Wang (CHW). - `2`: Chen, DeMets, and Lan
#'     (CDL).}
#'   \item{AdaptInterimScale}{Integer. Adapt at interim analysis scale: - `1`: Interim analysis number. - `2`:
#'     Analysis spacing info (\%). Fixed to `1` for East Horizon Explore and for `AdaptMethod = 2 (CDL)`.}
#'   \item{AdaptInterimNum}{Integer. Interim analysis number at which adaptation will happen. Fixed to
#'     `LookInfo$NumLooks - 1` for `AdaptMethod = 2 (CDL)`.}
#'   \item{AdaptInterimPerc}{Numeric. Analysis spacing info (\%) at which adaptation will happen. Available only
#'     for
#'     `AdaptMethod = 2 (CDL)`.}
#'   \item{StudyDurationUL}{Numeric. Upper limit on study duration. Not available for East Horizon Explore.}
#'   \item{WaldCPThreshold}{Numeric. Threshold beyond which Wald statistics are used. Available only for
#'     `AdaptMethod = 2 (CDL)`.}
#'   \item{EnrollAdaptScale}{Integer. Scale for enrollment rate after adaptation: - `0`: No change. - `1`: Use
#'     multiplier. - `2`: Use fixed rate. `EnrollAdaptScale = 2 (fixed rate)` is only applicable for East Horizon
#'     Design.}
#'   \item{EnrollAdaptMult}{Numeric. Multiplier for enrollment rate after adaptation. Available only for
#'     `EnrollAdaptScale = 1 (use multiplier)`.}
#'   \item{EnrollAdaptRate}{Numeric. Fixed enrollment rate after adaptation. Available only for `EnrollAdaptScale =
#'     2 (use fixed rate)`.}
#'   \item{PromZoneScale}{Integer. Promising zone scale: - `0`: Estimated conditional power. - `1`:
#'     Arbitrary/design conditional power. - `2`: Test statistic. - `3`: Estimated delta/sigma.}
#'   \item{PromZoneSigma}{Numeric. Reference/design sigma for computing promising zone conditional power. Available
#'     only when `PromZoneScale = 1 (Arbitrary/design conditional power)`.}
#'   \item{PromZonePiC}{Numeric. Reference/design control response probability for computing promising zone
#'     conditional power. Available for binary endpoints.}
#'   \item{PromZoneHR}{Numeric. Reference/design hazard ratio for computing promising zone conditional power.
#'     Available for time-to-event endpoints.}
#'   \item{PromZoneDelta}{Numeric. Reference/design delta for computing promising zone conditional power. Available
#'     only when `PromZoneScale = 1 (Arbitrary/design conditional power)`.}
#'   \item{PromZoneMin}{Numeric. Minimum threshold for promising zone.}
#'   \item{PromZoneMax}{Numeric. Maximum threshold for promising zone.}
#'   \item{SSRFuncScale}{Numeric. Sample size re-estimation function scale for promising zone: - `0`: Continuous. -
#'     `1`: Step. - `2`: User-specified R. `SSRFuncScale = 1 (step)` is available only in East Horizon Explore.}
#'   \item{NumSteps}{Integer. Number of steps. Available only for `SSRFuncScale = 1 (step)`.}
#'   \item{MaxSSMultInp}{Named List. Maximum sample size multiplier inputs. If `SSRFuncScale = 0 (continuous)`, it
#'     is a Named List of Numeric: - `MaxSSMultInp["From"] = PromZoneMin`. - `MaxSSMultInp["To"] = PromZoneMax`. -
#'     `MaxSSMultInp["MaxSSMult"]` and `MaxSSMultInp["MaxEventsMult"]` are numeric multipliers. If `SSRFuncScale =
#'     1 (step)`, it is a Named
#'     List of Array of Numeric: - `MaxSSMultInp["From"]` is an array of size `NumSteps` containing the lower
#'     threshold for each step. - `MaxSSMultInp["To"]` is an array of size `NumSteps` containing the upper
#'     threshold for each step. - `MaxSSMultInp["MaxSSMult"]` is an array of size `NumSteps` containing the maximum
#'     sample size multiplier for each step. `MaxSSMultInp["MaxEventsMult"]` contains the corresponding maximum
#'     event multipliers.}
#'   \item{TargetCP}{Numeric. Target conditional power. Available only for `SSRFuncScale = 0 (continuous)`.}
#'   \item{OrigCP}{Numeric. Conditional power computed from the maximum number of completers/events.}
#'   \item{TestStat}{Numeric. Test statistic at the current look.}
#'   \item{Delta}{Numeric. Observed treatment effect at the current look.}
#'   \item{StdError}{Numeric. Standard error of the observed treatment effect at the current look.}
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
#'   \item{Delta}{Estimated treatment-to-control hazard ratio (HR scale, not log HR), used for Delta-scale
#'     futility.}
#'   \item{AnalysisTime}{Optional numeric calendar time of the analysis: the look time at an interim analysis and
#'     the study duration at the final analysis. Compute and return this value in the R function.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#'   \item{ReEstEvents}{Integer re-estimated total event count, required at the adaptation look.}
#' }
#' @details Implement the placeholder sample size re-estimation rule before use.
#'
#' For ordinary analysis designs, return either Decision to apply custom stopping logic or TestStat to let
#'   the engine apply its boundaries. Delta may also be required
#'   for Delta-scale or conditional-power futility. At the adaptation look, sample size re-estimation designs also
#'   require the re-estimated total event/completer count; Decision is not mandatory if TestStat is supplied.
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( SimData, DesignParam, LookInfo = NULL, AdaptInfo = NULL, UserParam = NULL ) {
    nErrorCode <- 0 # Replace the placeholder SSR logic before running simulations.
    nReEstCount <- 0
    nDecision <- 0
    # Write the actual code here.
    # Use appropriate error handling and modify
    # the error appropriately.
    return( list(
        ReEstEvents = as.integer( nReEstCount ),
        Decision = as.integer( nDecision ),
        ErrorCode = as.integer( nErrorCode )
    ) )
}