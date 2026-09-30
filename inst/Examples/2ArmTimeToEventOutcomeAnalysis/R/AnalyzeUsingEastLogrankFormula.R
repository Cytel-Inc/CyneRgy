######################################################################################################################## .
#' @name AnalyzeUsingEastLogrankFormula
#'
#' @title Compute the statistic using formulas Q.242 and Q.243 in the East Horizon manual.
#'
#' @description Use the formulas Q.242 and Q.243 in the East Horizon manual to compute the statistic. The purpose of this
#'   example is to demonstrate how the analysis and decision making can be modified in a simple approach. The test
#'   statistic is compared to the lower boundary computed and sent by East Horizon as an input. This example does NOT
#'   include a futility rule.
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
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore).}
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale.}
#'   \item{HR}{Estimated treatment-to-control hazard ratio.}
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
######################################################################################################################## .

AnalyzeUsingEastLogrankFormula <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    # Step 1: Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
    if ( !is.null( LookInfo ) ) {
        # Look info was provided so use it
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        vCumEvents <- LookInfo$InfoFrac * DesignParam$MaxEvents
        nQtyOfEvents <- vCumEvents[ nLookIndex ]
        dEffBdry <- LookInfo$EffBdryLower[ nLookIndex ]
        nRejType <- LookInfo$RejType
        nTailType <- DesignParam$TailType
    } else { # Look info is not provided for fixed sample designs so fetch the information appropriately
        nQtyOfLooks <- 1
        nLookIndex <- 1
        nQtyOfEvents <- DesignParam$MaxEvents
        dEffBdry <- DesignParam$CriticalPoint
        nTailType <- DesignParam$TailType
    }

    SimData$TimeOfEvent <- SimData$ArrivalTime + SimData$SurvivalTime # This is the calendar time in the trial that the patients event is observed

    # Compute the time of analysis
    SimData <- SimData[ order( SimData$TimeOfEvent ), ]
    dTimeOfAnalysis <- SimData[ nQtyOfEvents, ]$TimeOfEvent

    # Add the Observed Time variable
    SimData <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ] # Exclude any patients that were not enrolled by the time of the analysis
    SimData$Event <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 ) # If the event is observed after the analysis it is not observed, eg censored
    SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )

    # Order the data by observed time for the remainder of the computations
    SimData <- SimData[ order( SimData$ObservedTime ), ]

    # Compute Observed HR

    coxModel <- survival::coxph( survival::Surv( ObservedTime, Event ) ~ TreatmentID, data = SimData )
    dTrueHR <- exp( coxModel$coefficients )

    SimData$EventOnTreatment <- ifelse( SimData$TreatmentID == 1, SimData$Event, 0 ) # If the event is observed on treatment
    SimData$EventOnControl <- ifelse( SimData$TreatmentID == 0, SimData$Event, 0 ) # If the event is observed on control

    # Arm wise count of subjects at risk at the beginning. Is same as arm wise sample size
    nSubjectsAtRiskTreatment <- nrow( SimData[ SimData$TreatmentID == 1, ] )
    nSubjectsAtRiskControl <- nrow( SimData[ SimData$TreatmentID == 0, ] )

    # Initialize intermediate quantities required for test statistic computation
    dNum <- 0
    dDen <- 0

    # Iterate over subjects to calculate dNum and dDen required for test statistic computation
    for ( nSubject in 1:nrow( SimData ) )
    { # Update the count of subjects at risk for each arm for non event times
        if ( SimData$Event[ nSubject ] == 0 ) {
            if ( SimData$TreatmentID[ nSubject ] == 1 ) {
                nSubjectsAtRiskTreatment <- nSubjectsAtRiskTreatment - 1
            }
            if ( SimData$TreatmentID[ nSubject ] == 0 ) {
                nSubjectsAtRiskControl <- nSubjectsAtRiskControl - 1
            }
        } # For subjects with events, compute dNum and dDen
        if ( SimData$Event[ nSubject ] == 1 ) {
            nEventsOnTreatment <- SimData$EventOnTreatment[ nSubject ]
            nEventsOnControl <- SimData$EventOnControl[ nSubject ]
            nEvents <- nEventsOnTreatment + nEventsOnControl
            nSubjectsAtRisk <- nSubjectsAtRiskTreatment + nSubjectsAtRiskControl
            # Equation Q.242 in East Horizon Manual
            dNum <- dNum + nEventsOnTreatment - nSubjectsAtRiskTreatment * nEvents / nSubjectsAtRisk
            # Generate dDen based on number of subjects at risk
            if ( nSubjectsAtRisk != 1 ) { # Equation Q.243 in East Horizon Manual
                dDen <- dDen + nSubjectsAtRiskTreatment * nSubjectsAtRiskControl * ( nSubjectsAtRisk - nEvents ) * nEvents / ( ( nSubjectsAtRisk - 1 ) * nSubjectsAtRisk^2 )
            }
            # Update the count of subjects at risk before the next iteration
            nSubjectsAtRiskTreatment <- nSubjectsAtRiskTreatment - nEventsOnTreatment
            nSubjectsAtRiskControl <- nSubjectsAtRiskControl - nEventsOnControl
        }
    }

    # Compute the logrank test statistic
    dTS <- dNum / sqrt( dDen )

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
        bIAEfficacyCondition = dTS < dEffBdry,
        bFAEfficacyCondition = dTS < dEffBdry
    )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    nErrorCode <- 0

    lRet <- list(
        TestStat = as.double( dTS ),
        Decision = as.integer( nDecision ),
        ErrorCode = as.integer( nErrorCode ),
        HazardRatio = as.double( dTrueHR )
    )
    return( lRet )
}
