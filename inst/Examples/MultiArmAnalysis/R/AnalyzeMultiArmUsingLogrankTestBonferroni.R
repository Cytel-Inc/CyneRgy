######################################################################################################################## .
#' @name AnalyzeMultiArmUsingLogrankTestBonferroni
#'
#' @title Analyze multi-arm time-to-event outcomes using Bonferroni-adjusted log-rank tests.
#'
#' @description Analyze multi-arm time-to-event outcomes using Bonferroni-adjusted log-rank tests. Use the
#'   documented inputs and outputs to integrate this function with the simulation workflow.
#'
#' @author Gabriel Potvin and Anoop Singh Rawat
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
#'   \item{PFSNonCens}{Numeric vector of PFS times relative to patient enrollment}
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject. May contain a
#'     custom binary endpoint, such as the stage 1 response in the two-stage example.}
#' }
#'
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below.
#' \describe{
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{TrialType}{Integer. Trial Type: – `0`: Superiority.}
#'   \item{TestType}{Integer. Test Type: – `0`: One-sided.}
#'   \item{TailType}{Integer. Nature of critical region: – `0`: Left-tailed. – `1`: Right-tailed.}
#'   \item{InitialAllocInfo}{Vector of Numeric. Vector of length equal to the number of treatment arms (number of
#'     arms - 1), containing the ratios of the treatment group sample sizes to control group sample size.}
#'   \item{CriticalPoint}{Numeric. Critical value. East Horizon Explore: Only available if `Statistical Design =
#'     Fixed Sample`. Not available for `Study Objective = Dose Finding`. East Horizon Design: Not available for
#'     `Combining P-Values (MAMS)` tests.}
#'   \item{SampleSize}{Integer planned total sample size of the trial.}
#'   \item{MaxEvents}{Integer maximum number of events in the trial.}
#'   \item{FollowUpType}{Integer. Follow-up type: – `0`: Until the end of the study. – `1`: For a fixed period.
#'     East Horizon Explore: Only available for `Endpoint Type = Time-to-Event`. Not available for `Study Objective
#'     = Dose Finding`. East Horizon Design: Only available for `Time-to-Event` tests.}
#'   \item{FollowUpDur}{Numeric. Follow-up duration. East Horizon Explore: Only available for `Endpoint Type =
#'     Time-to-Event`. Not available for `Study Objective = Dose Finding`. East Horizon Design: Only available for
#'     `Time-to-Event` tests.}
#'   \item{TestStatType}{Integer. Test statistic type: - `3`: Z-test. - `4`: t-test. East Horizon Explore: Only
#'     available for `Endpoint Type = Continuous`. East Horizon Design: Only available for `Continuous` tests.}
#'   \item{PValCombMethod}{Integer. P-value combination method: - `0`: Inverse normal. East Horizon Explore: Not
#'     available. East Horizon Design: Only available for `Combining P-Values (MAMS)` tests.}
#'   \item{w1}{Numeric. Inverse normal weights for stage 1. East Horizon Explore: Not available. East Horizon
#'     Design: Only available for `Combining P-Values (MAMS)` tests.}
#'   \item{w2}{Numeric. Inverse normal weights for stage 2. East Horizon Explore: Not available. East Horizon
#'     Design: Only available for `Combining P-Values (MAMS)` tests.}
#'   \item{MultAdjMethod}{Integer. Multiple comparison procedure. East Horizon Explore: Possible values: – `0`:
#'     Bonferroni. – `3`: Dunnett's Single Step. – `4`: Weighted Bonferroni. – `5`: Fixed Sequence. – `6`:
#'     Fallback. – `7`: Hochberg's Step Up. East Horizon Design: Possible values:- – `0`: Bonferroni. – `1`: Sidak.
#'     – `2`: Simes. – `3`: Dunnett's Single Step. – `4`: Weighted Bonferroni. – `5`: Fixed Sequence. – `6`:
#'     Fallback. – `7`: Hochberg's Step Up. – `10`: Holm's Step Down. – `11`: Hommel's Step Up. – `12`: Dunnett's
#'     Step Down. – `13`: Dunnett's Step Up.}
#'   \item{NumTreatments}{Integer number of experimental treatment arms, excluding control.}
#'   \item{AlphaProp}{Vector of Numeric. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     containing the proportion of Alpha for each treatment arm. East Horizon Explore: Only available for
#'     `Multiple Comparison Procedure = 4 (Weighted Bonferroni) or 6 (Fallback)`. Not available for `Study
#'     Objective = Dose Finding`. East Horizon Design: Only available for `Multiple Comparison Procedure = 4
#'     (Weighted Bonferroni) or 6 (Fallback)`. Not available for `Test = MAMS Difference of Means: Combining
#'     P-Values (Continuous) or MAMS Difference of Proportions: Combining P-Values (Binary) or MAMS Logrank
#'     (Time-to-Event)`.}
#'   \item{TestSeq}{Vector of Integer. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     containing the test sequence for each comparison (each treatment arm). East Horizon Explore: Only available
#'     for `Multiple Comparison Procedure = 5 (Fixed Sequence) or 6 (Fallback)`. Not available for `Study Objective
#'     = Dose Finding`. East Horizon Design: Only available for `Multiple Comparison Procedure = 5 (Fixed Sequence)
#'     or 6 (Fallback)`. Not available for `Test = MAMS Difference of Means: Combining P-Values (Continuous) or
#'     MAMS Difference of Proportions: Combining P-Values (Binary) or MAMS Logrank (Time-to-Event)`.}
#'   \item{IsArmPresent}{Vector or Integer.. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     indicating whether each arm is still in the trial or was dropped in the interim: - `0`: Dropped in the
#'     interim. - `1`: Still present. East Horizon Explore: Fixed to `1` for the first look and for `Statistical
#'     Design = Fixed Sample`. East Horizon Design: Fixed to `1` for the first look and for `Statistical Design =
#'     Fixed Sample`.}
#'   \item{UpdatedAllocInfo}{Vector of Numeric. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     containing the updated ratios of the treatment group sample sizes to control group sample size, which may
#'     have been updated during treatment selection.}
#'   \item{TestID}{Integer test identifier supplied by East Horizon for the selected test.}
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
#'   \item{CumEvents}{Vector of Integer. Vector of length `LookInfo$NumLooks`,containing the cumulative event for
#'     each look. East Horizon Explore: Only available for `Endpoint Type = Time-to-Event`. Not available for
#'     `Study Objective = Dose Finding`. East Horizon Design: Only available for `Time-to-Event` tests.}
#'   \item{RejType}{Integer. Rejection type: – `0`: One-sided efficacy upper. – `1`: One-sided futility upper. –
#'     `2`: One-sided efficacy lower. – `3`: One-sided futility lower. – `4`: One-sided efficacy upper, futility
#'     lower. – `5`: One-sided efficacy lower, futility upper.}
#'   \item{EffBdryScale}{Integer. Efficacy boundary scale. East Horizon Explore: Not available for `Study Objective
#'     = Dose Finding`. Possible values: – `0`: Z scale. East Horizon Design: Possible values: `1`: Adjusted
#'     p-value scale.}
#'   \item{EffBdry}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the efficacy boundary
#'     values for each look. East Horizon Explore: Not available for `Study Objective = Dose Finding`.}
#'   \item{FutBdryScale}{Integer. Futility boundary scale. East Horizon Explore: Possible values: – `2`: Delta
#'     scale. – `6`: Hazard ratio scale. East Horizon Design: Possible values: – `1`: Adjusted p-value scale. –
#'     `2`: Delta scale. – `6`: Hazard ratio scale.}
#'   \item{FutBdry}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the futility boundary
#'     values for each look.}
#'   \item{CumAlphaUpper}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the upper cumulative
#'     alpha spent (for two-sided tests) for each look. Same as CumAlpha if right-tailed one-sided test. Only makes
#'     sense to use for two-sided asymmetric tests. East Horizon Explore: Only available if `Tail Type =
#'     Right-tailed`. Two-sided tests do not exist, so this variable is not useful: use CumAlpha instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Right-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
#'   \item{CumAlphaLower}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the lower cumulative
#'     alpha spent (for two-sided tests) for each look. Same as CumAlpha if left-tailed one-sided test. Only makes
#'     sense to use for two-sided asymmetric tests. East Horizon Explore: Only available if `Tail Type =
#'     Left-tailed`. Two-sided tests do not exist, so this variable is not useful: use CumAlpha instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Left-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
#'   \item{EffBdryUpper}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the upper efficacy
#'     boundary values (for two-sided tests) for each look. East Horizon Explore: Only available if `Tail Type =
#'     Right-tailed`. Two-sided tests do not exist, so this variable is not useful: use EffBdry instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Right-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
#'   \item{EffBdryLower}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the lower efficacy
#'     boundary values (for two-sided tests) for each look. East Horizon Explore: Only available if `Tail Type =
#'     Left-tailed`. Two-sided tests do not exist, so this variable is not useful: use EffBdry instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Left-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
#'   \item{FutBdryUpper}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the upper futility
#'     boundary values (for two-sided tests) for each look. East Horizon Explore: Only available if `Tail Type =
#'     Right-tailed`. Two-sided tests do not exist, so this variable is not useful: use FutBdry instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Right-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
#'   \item{FutBdryLower}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the lower futility
#'     boundary values (for two-sided tests) for each look. East Horizon Explore: Only available if `Tail Type =
#'     Left-tailed`. Two-sided tests do not exist, so this variable is not useful: use FutBdry instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Left-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
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
#'     (unavailable in East Horizon Explore). Return one value per experimental arm in treatment-ID order.}
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale. Return one value per experimental arm in
#'     treatment-ID order.}
#'   \item{HR}{Numeric vector of estimated treatment-to-control hazard ratios, one per experimental arm in
#'     treatment-ID order.}
#'   \item{Delta}{Estimated log hazard ratio (natural logarithm of HR).}
#'   \item{CtrlEvents}{Number of observed events in the control arm.}
#'   \item{TrmtEvents}{Number of observed events in the experimental arm.}
#'   \item{AnalysisTime}{Optional numeric calendar time of the analysis: the look time at an interim analysis and
#'     the study duration at the final analysis. Compute and return this value in the R function.}
#'   \item{AdjPVal}{Numeric vector of p-values adjusted for multiple comparisons, one per experimental arm.}
#'   \item{RawPVal}{Numeric vector of unadjusted p-values, one per experimental arm.}
#'   \item{OutList}{Optional named list used to pass outputs between analysis looks. Return it at one look to
#'     receive the same list as input at the next look; the input is NULL at the first look. Access elements by
#'     name. Available for designs that support passing state between looks.}
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

AnalyzeMultiArmUsingLogrankTestBonferroni <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    # Retrieve necessary information from the objects East Horizon sent
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfEvents <- LookInfo$InfoFrac[ nLookIndex ] * DesignParam$MaxEvents
        dEffBoundary <- LookInfo$EffBdry[ nLookIndex ]

        if ( DesignParam$TailType == 1 ) {
            dBoundaryPScale <- 1 - stats::pnorm( dEffBoundary )
        } else {
            dBoundaryPScale <- stats::pnorm( dEffBoundary )
        }
    } else {
        # Look info is not provided for fixed sample designs so fetch the information appropriately
        nQtyOfLooks <- 1
        nLookIndex <- 1
        nQtyOfEvents <- DesignParam$MaxEvents
        dBoundaryPScale <- DesignParam$Alpha
    }

    vIsTrtPresent <- DesignParam$IsArmPresent

    # This is the calendar time in the trial that the patient event is observed
    SimData$TimeOfEvent <- SimData$ArrivalTime + SimData$SurvivalTime

    # Order the data by observed time for the remainder of the computations
    SimData <- SimData[ order( SimData$TimeOfEvent ), ]

    if ( nrow( SimData ) < nQtyOfEvents ) {
        return( list(
            Decision             = rep( NA_integer_, DesignParam$NumTreatments ),
            ErrorCode            = as.integer( 1 ),
            HR                   = rep( NA_real_, DesignParam$NumTreatments ),
            HazardRatio          = rep( NA_real_, DesignParam$NumTreatments ),
            RawPVal              = rep( NA_real_, DesignParam$NumTreatments ),
            AdjPVal              = rep( NA_real_, DesignParam$NumTreatments ),
            AnalysisTime         = NA_real_
        ) )
    }

    dTimeOfAnalysis <- SimData[ nQtyOfEvents, ]$TimeOfEvent

    SimData <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ]
    SimData$Event <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )
    SimData$ObservedTime <- ifelse(
        SimData$TimeOfEvent > dTimeOfAnalysis,
        dTimeOfAnalysis - SimData$ArrivalTime,
        SimData$TimeOfEvent - SimData$ArrivalTime
    )

    SimData <- SimData[ order( SimData$ObservedTime ), ]

    vPValues <- rep( NA_real_, DesignParam$NumTreatments )
    vHRRatio <- rep( NA_real_, DesignParam$NumTreatments )

    for ( nTrtID in 1:DesignParam$NumTreatments ) {
        if ( vIsTrtPresent[ nTrtID ] == 1 ) {
            SimDataTrt <- SimData[ SimData$TreatmentID %in% c( 0, nTrtID ), ]

            # Compute Observed HR
            coxModel <- survival::coxph(
                survival::Surv( ObservedTime, Event ) ~ TreatmentID,
                data = SimDataTrt
            )

            # Compute the test statistic using survival package
            logrankTest <- survival::survdiff(
                survival::Surv( ObservedTime, Event ) ~ TreatmentID,
                data = SimDataTrt
            )

            vHRRatio[ nTrtID ] <- as.numeric( exp( coxModel$coefficients ) )
            vPValues[ nTrtID ] <- logrankTest$pvalue
        }
    }

    # Calculate Bonferroni adjusted p values
    # Assumes that each present arm has a valid hypothesis test and p-value
    nActiveArms <- sum( vIsTrtPresent == 1, na.rm = TRUE )
    vAdjPValues <- pmin( vPValues * nActiveArms, 1 )

    vDecision <- c( )

    # Perform the desired analysis
    for ( i in 1:DesignParam$NumTreatments ) {
        if ( vIsTrtPresent[ i ] == 1 ) {
            strDecision <- CyneRgy::GetDecisionString(
                LookInfo,
                nLookIndex,
                nQtyOfLooks,
                bIAEfficacyCondition = !is.na( vAdjPValues[ i ] ) &&
                    vAdjPValues[ i ] < dBoundaryPScale,
                bFAEfficacyCondition = !is.na( vAdjPValues[ i ] ) &&
                    vAdjPValues[ i ] < dBoundaryPScale
            )

            nDecision <- CyneRgy::GetDecision(
                strDecision,
                DesignParam,
                LookInfo
            )
        } else {
            nDecision <- NA_integer_
        }

        vDecision <- c( vDecision, nDecision )
    }

    return( list(
        Decision                 = as.integer( vDecision ),
        ErrorCode                = as.integer( 0 ),
        HR                       = as.double( vHRRatio ),
        HazardRatio              = as.double( vHRRatio ),
        RawPVal                  = as.double( vPValues ),
        AdjPVal                  = as.double( vAdjPValues ),
        AnalysisTime             = as.double( dTimeOfAnalysis )
    ) )
}
