######################################################################################################################## .
#' @name AnalyzeMultiArmUsingTTestBonferroni
#' @title Analyze continuous outcome for multi-arm design using the t.test function in the stats package in R.
#' @description Analyze continuous outcome for multi-arm design using the t.test function in the stats package in R. Use the
#'   documented inputs and outputs to integrate this function with the simulation workflow.
#' @author Gabriel Potvin and Anoop Singh Rawat
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the fields below when applicable,
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
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below.
#' \describe{
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{TrialType}{Integer. Trial Type: – `0`: Superiority.}
#'   \item{TestType}{Integer. Test Type: – `0`: One-sided.}
#'   \item{TailType}{Integer. Nature of critical region: – `0`: Left-tailed. – `1`: Right-tailed.}
#'   \item{InitialAllocInfo}{Vector of Numeric. Vector of length equal to the number of experimental arms (number of
#'     arms - 1), containing the ratios of the experimental group sample sizes to the control group sample size.}
#'   \item{CriticalPoint}{Numeric. Critical value. East Horizon Explore: Only available if `Statistical Design =
#'     Fixed Sample`. Not available for `Study Objective = Dose Finding`. East Horizon Design: Not available for
#'     `Combining P-Values (MAMS)` tests.}
#'   \item{SampleSize}{Integer planned total sample size of the trial.}
#'   \item{MaxCompleters}{Integer maximum number of completers in the trial.}
#'   \item{RespLag}{Numeric follow-up duration from enrollment to response measurement.}
#'   \item{TestStatType}{Integer. Test statistic type: - `3`: Z-test. - `4`: t-test. East Horizon Explore: Only
#'     available for `Endpoint Type = Continuous`. East Horizon Design: Only available for `Continuous` tests.}
#'   \item{VarType}{Integer. Variance type. For `Continuous` test: - `4`: Equal - `5`: Unequal. For `Difference of
#'     Proportions` (Binary) test: - `0`: Pooled. `1`: Unpooled. East Horizon Explore: Not available for `Endpoint
#'     Type = Time-to-Event`. East Horizon Design: Not available for `Time-to-Event` tests.}
#'   \item{Sigma}{Numeric. Design standard deviation specified in simulations. East Horizon Explore: Not available.
#'     East Horizon Design: Only available for `Test = MAMS Difference of Means: Combining P-Values` (Continuous)
#'     and `Test Stat Type = 3 (Z-test)`.}
#'   \item{PValCombMethod}{Integer. P-value combination method: - `0`: Inverse normal. East Horizon Explore: Not
#'     available. East Horizon Design: Only available for `Combining P-Values (MAMS)` tests.}
#'   \item{w1}{Numeric. Inverse normal weights for stage 1. East Horizon Explore: Not available. East Horizon
#'     Design: Only available for `Combining P-Values (MAMS)` tests.}
#'   \item{w2}{Numeric. Inverse normal weights for stage 2. East Horizon Explore: Not available. East Horizon
#'     Design: Only available for `Combining P-Values (MAMS)` tests.}
#'   \item{MultAdjMethod}{Integer. Multiple comparison procedure. East Horizon Explore: Possible values: – `0`:
#'     Bonferroni. – `3`: Dunnett's Single Step. – `4`: Weighted Bonferroni. – `5`: Fixed Sequence. – `6`:
#'     Fallback. – `7`: Hochberg's Step Up. East Horizon Design: Possible values: – `0`: Bonferroni. – `1`: Sidak.
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
#'   \item{IsArmPresent}{Vector of Integer. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     indicating whether each arm is still in the trial or was dropped in the interim: - `0`: Dropped in the
#'     interim. - `1`: Still present. East Horizon Explore: Fixed to `1` for the first look and for `Statistical
#'     Design = Fixed Sample`. East Horizon Design: Fixed to `1` for the first look and for `Statistical Design =
#'     Fixed Sample`.}
#'   \item{UpdatedAllocInfo}{Vector of Numeric. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     containing the updated ratios of the treatment group sample sizes to control group sample size, which may
#'     have been updated during treatment selection.}
#'   \item{TestID}{Integer test identifier supplied by East Horizon for the selected test.}
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
#'   \item{CumCompleters}{Vector of Integer. Vector of length `LookInfo$NumLooks`, containing the cumulative number
#'     of completers for each look. East Horizon Explore: Not available for `Endpoint Type = Time-to-Event`. East
#'     Horizon Design: Not available for `Time-to-Event` tests.}
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
#' }
#' @param OutList Optional named list used to pass outputs between analysis looks. Return it at one look to receive
#'   the same list as input at the next look; the input is NULL at the first look. Access elements by name.
#'   Available for designs that support passing state between looks.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore). Return one value per experimental arm in treatment-ID order.}
#'   \item{TestStat}{Numeric vector of test statistics on the Wald (Z) scale, with one value per experimental arm in
#'     treatment-ID order.}
#'   \item{Delta}{Numeric vector of estimated experimental-minus-control treatment effects, one per experimental
#'     arm in treatment-ID order. For binary outcomes, each effect is a proportion difference; for continuous
#'     outcomes, each effect is a mean difference.}
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
#' }
#' @details Return Decision to apply custom stopping logic, or TestStat, AdjPVal, or RawPVal to let East Horizon
#'   apply its supported multiplicity adjustments and boundaries. Return one value per experimental arm
#'   in treatment-ID order and NA for arms absent at the current look. Delta is required for Delta-scale
#'   futility; HR is required for hazard-ratio-scale futility in time-to-event designs. RawPVal cannot be
#'   used for adjusted-p-value-scale futility. Return TestStat alongside RawPVal when the selected
#'   Dunnett procedure requires it. Use OutList to pass custom state between looks. This example may use
#'   only a subset of the documented design fields.
######################################################################################################################## .

AnalyzeMultiArmUsingTTestBonferroni <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL, OutList = NULL ) {
    # Analyze observed completers; simulated dropout responses are not observed data.
    if ( "CensorInd" %in% names( SimData ) ) {
        SimData <- SimData[ !is.na( SimData$CensorInd ) & SimData$CensorInd == 1, , drop = FALSE ]
    } else if ( "CensorIndOrg" %in% names( SimData ) ) {
        SimData <- SimData[ !is.na( SimData$CensorIndOrg ) & SimData$CensorIndOrg == 1, , drop = FALSE ]
    }
    if ( !is.null( LookInfo ) && !is.null( LookInfo$CumCompleters ) ) {
        nTargetCompleters <- LookInfo$CumCompleters[ LookInfo$CurrLookIndex ]
        if ( nTargetCompleters < 1 || nTargetCompleters > nrow( SimData ) ) {
            return( list( ErrorCode = 1L ) )
        }
    }

    # Step 1: Retrieve necessary information from the objects East Horizon sent ####
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        vInfoFrac <- LookInfo$InfoFrac
        vEfficacyBoundary <- gsDesign::gsDesign(
            k = nQtyOfLooks, test.type = 1, alpha = DesignParam$Alpha,
            sfu = gsDesign::sfLDOF, timing = vInfoFrac
        )
        vEfficacyBoundaryPScale <- 1 - stats::pnorm( vEfficacyBoundary$upper$bound )
    } else {
        nQtyOfLooks <- 1
        nLookIndex <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
        vInfoFrac <- 1
        vEfficacyBoundaryPScale <- DesignParam$Alpha
    }

    vIsTrtPresent <- DesignParam$IsArmPresent
    # Create the vector of simulated data for this IA - East Horizon sends all of the simulated data
    vPatientOutcome <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]

    # Create vector of data for control
    vOutcomesS <- vPatientOutcome[ vPatientTreatment == 0 ]

    # Calculate p-value for each hypothesis. Return NA if arm not present in the current analysis
    vPValues <- rep( NA, DesignParam$NumTreatments )
    for ( nTrtID in 1:DesignParam$NumTreatments ) {
        if ( vIsTrtPresent[ nTrtID ] == 1 ) {
            vOutcomesE <- vPatientOutcome[ vPatientTreatment == nTrtID ]
            lAnalysisResult <- stats::t.test( vOutcomesE, vOutcomesS,
                alternative = "greater",
                var.equal = TRUE
            )
            dPValue <- lAnalysisResult$p.value # extract p value for the t test
        } else {
            dPValue <- NA
        }
        vPValues[ nTrtID ] <- dPValue
    }

    # Calculate Bonferroni adjusted p values
    vAdjPValues <- vPValues * sum( vIsTrtPresent )

    # Perform the desired analysis. NA should be returned for arms that are not available at this look
    # vDecision                    <- ifelse( vAdjPValues < vEfficacyBoundaryPScale[ nLookIndex], 2, 0 )  # A decision of 2 means success, 0 means continue the trial
    vDecision <- c( )
    for ( i in 1:length( vAdjPValues ) ) {
        if ( vIsTrtPresent[ i ] == 1 ) {
            strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                bIAEfficacyCondition = vAdjPValues[ i ] < vEfficacyBoundaryPScale[ nLookIndex ],
                bFAEfficacyCondition = vAdjPValues[ i ] < vEfficacyBoundaryPScale[ nLookIndex ]
            )
            nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
        } else {
            nDecision <- NA_integer_
        }
        vDecision <- c( vDecision, nDecision )
    }
    # for( i in 1:length(vDecision) ){
    #     if( vDecision[i] == 0 )
    #     {
    #         # Did not hit efficacy, so check futility
    #         # We are at the FA, efficacy decision was not made yet so the decision is futility
    #         if( nLookIndex == nQtyOfLooks )
    #         {
    #             vDecision[i]     <- 3 # Code for futility
    #         }
    #     }
    # }

    nErrorCode <- 0

    return( list(
        Decision = as.integer( vDecision ),
        ErrorCode = as.integer( nErrorCode )
    ) )
}
