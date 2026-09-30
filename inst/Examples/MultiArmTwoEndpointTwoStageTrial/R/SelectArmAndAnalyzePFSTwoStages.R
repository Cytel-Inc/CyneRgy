######################################################################################################################## .
#' @name SelectArmAndAnalyzePFSTwoStages
#'
#' @title Two-Stage Arm Selection and PFS Analysis
#'
#' @description This function performs a two-stage adaptive analysis for multi-arm clinical trials. Stage 1 selects
#'   the best treatment arm based on binary response endpoint. Stage 2 tests efficacy using progression-free
#'   survival (PFS) via log-rank test.
#'
#' @author Julija Saltane, J. Kyle Wathen
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
#' Example-specific parameters and requirements:
#' Relevant elements include:
#'        \describe{
#'          \item{Stage1NumCompleters}{Number of patients for Stage 1 analysis}
#'          \item{Stage1FutThreshold}{Stage 1 futility threshold}
#'          \item{DropoutProportion}{Proportion of patients who drop out during PFS follow-up}
#'          \item{TargetNumPFSEvents}{Target number of PFS events for Stage 2 timing}
#'          \item{SwitchSign}{Character value ('yes' or 'no') indicating whether the critical-point sign should be
#'            reversed for the PFS analysis}
#'        }
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
#' Example-specific additional output elements:
#' \describe{
#'   \item{NumPatientInStage_i}{Number of patients in stage i (i = 1 for arm selection stage, i = 2 for PFS
#'     analysis stage). Both values populated; other indices are zero.}
#'   \item{NumCompleters_i}{Number of completers (all patients in Stage 1, and additional patients in Stage 2).
#'     Non-zero only for the selected arm, containing its index value; others are zero.}
#'   \item{ChosenArm_i}{Selected treatment arm indicator (i = 1 to `NumTreatments`). Non-zero only for the selected
#'     arm, containing its index value; others are zero.}
#'   \item{Stage2AnalysisTiming_i}{Time of Stage 2 analysis. Non-zero only for the selected arm, containing its
#'     index value; others are zero.}
#'   \item{CriticalPoint_i}{Critical point for arm i (i = 1 to `NumTreatments`). Non-zero only for the selected
#'     arm; others are zero.}
#'   \item{HazardRatio_i}{Estimated hazard ratio from Cox model (only for selected arm).}
#'   \item{Control_Stage_i_Patients}{Number of control patients used in Stage 1 and Stage 2 analysis population.}
#'   \item{Stage1Patients_Arm_i}{Number of Stage 1 patients per treatment arm.}
#'   \item{Stage2Patients_Arm_i}{Number of Stage 2 patients per treatment arm.}
#' }
#'
#' @details For ordinary analysis designs, return either Decision to apply custom stopping logic or TestStat to let
#'   the engine apply its boundaries. Delta, event/completer counts, and standard errors may also be required for
#'   Delta-scale or conditional-power futility. Sample size re-estimation designs require a decision and the
#'   re-estimated total event/completer count. This example may use only a subset of the documented design fields.
#'
#' **IMPORTANT**: Type I error rate is NOT adjusted for interim data unblinding at Stage 1.
#'
#' **Stage 1 - Arm Selection:**
#' \itemize{
#'   \item Uses first `Stage1NumCompleters` enrolled patients
#'   \item Computes the observed binary response-rate difference for each treatment arm versus control
#'   \item Selects arm with maximum response difference (in case of a tie, arm with lower index is chosen)
#'   \item No formal hypothesis testing or multiplicity adjustment
#'   \item If the best observed treatment effect is below `Stage1FutThreshold`, the trial stops early for futility.
#' }
#'
#' **Stage 2 - Efficacy Analysis:**
#' \itemize{
#'   \item Filters data for control and selected arm
#'   \item Analysis timing determined by `TargetNumPFSEvents`
#'   \item Computes log-rank test statistic (East Horizon formulas Q.242, Q.243)
#'   \item Estimates the hazard ratio using a Cox proportional hazards model
#'   \item Makes efficacy decision using critical point
#'   \item Dropout can be incorporated.
#' }
#'
#' Example-specific output usage: TestStat: Numeric vector of log-rank test statistics. In the summary statistics
#'   file, this appears as separate columns (TestStat1, TestStat2, ..., TestStat_n). Vector length equals
#'   `NumTreatments` per engine requirements. Only the element corresponding to the selected arm contains the
#'   actual test statistic; all other elements are zero and should be disregarded. For example, if arm 2 is
#'   selected at Stage 1, `TestStat2` contains the log-rank statistic while `TestStat1` is zero. Decision: Integer
#'   vector of efficacy decisions. Only the selected arm's element is non-zero; others are zero and should be
#'   disregarded. For example, if arm 2 is selected, only `Decision2` contains the decision value. Engine
#'   requirement mandates vector length equals `NumTreatments`.
#'
#' Usage of LookInfo in this example: List with interim analysis information, or NULL for fixed design. **Currently
#'   only fixed design is supported**, adaptive designs not yet implemented.
#'
#' Example-specific error codes:
#' \describe{
#'   \item{ErrorCode = -1}{Missing required UserParam (`Stage1NumCompleters` or `TargetNumPFSEvents`)}
#'   \item{ErrorCode = -2}{LookInfo not NULL (adaptive designs not supported)}
#'   \item{ErrorCode = -3}{Insufficient patients for Stage 1 analysis}
#'   \item{ErrorCode = -4}{No valid treatment arm deltas computed}
#'   \item{ErrorCode = -5}{Insufficient patients for Stage 2 analysis}
#'   \item{ErrorCode = -6}{Test statistic denominator is zero}
#' }
######################################################################################################################## .

SelectArmAndAnalyzePFSTwoStages <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    nTrtArms <- DesignParam$NumTreatments
    nErrorCode <- 0

    # Arranging patients by their arrival time and assigning Patient IDs
    SimData <- SimData[ order( SimData$ArrivalTime ), ]
    SimData$PatientID <- seq_len( nrow( SimData ) )

    # Step 1. Initial Check: User must provide nStage1NumCompleters and TargetNumPFSEvents in UserParam ####
    if ( is.null( UserParam ) || is.null( UserParam$Stage1NumCompleters ) ) {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -1 ) )
    }
    if ( is.null( UserParam$Stage1FutThreshold ) ) {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -1 ) )
    }
    if ( is.null( UserParam$TargetNumPFSEvents ) ) {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -1 ) )
    }
    if ( is.null( UserParam$SwitchSign ) ) {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -1 ) )
    }

    # Step 2. Verify that a fixed design is being used ####
    if ( is.null( LookInfo ) ) {
        nQtyOfLooks <- 1
        nLookIndex <- 1
        # In this implementation, treatment superiority for the log-rank statistic corresponds to negative values. In contrast,
        # the binary difference-in-proportions statistic used in Stage 1 (and in project setup) is defined such that superiority
        # corresponds to positive values. To ensure consistency in the decision rule, we negate the critical point for the log-rank test.
        dSwitchSign <- ifelse( tolower( UserParam$SwitchSign ) == "yes", -1, 1 )
        dCriticalPoint <- dSwitchSign * DesignParam$CriticalPoint
    } else {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -2 ) )
    }

    # Step 3. Setting up the variables ####
    nStage1NumCompleters <- UserParam$Stage1NumCompleters
    nTargetNumPFSEvents <- UserParam$TargetNumPFSEvents

    # Step 4. Adding Absolute PFS Event Time: ####
    SimData$TimeOfPFSEvent <- SimData$ArrivalTime + SimData$PFSNonCens

    #-------------------------------------------------------------------------------------------------------------------------
    # Stage 1 ####

    # Check there are enough patients in the dataset to perform Stage 1 analysis
    if ( nrow( SimData ) < nStage1NumCompleters ) {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -3 ) )
    }

    # Use only first nStage1NumCompleters patients
    dfSimDataStage1 <- SimData[ 1:nStage1NumCompleters, ]

    # Number of Stage 1 patients per arm
    vStage1RecruitedPatientsPerArm <- as.vector( table( dfSimDataStage1$TreatmentID ) )

    # Assign patients enrolled before the Stage 1 cutoff to Stage 1; remaining patients are assigned to Stage 2
    dfSimDataStage1$Stage <- 1
    SimData$Stage <- ifelse( seq_len( nrow( SimData ) ) <= nStage1NumCompleters, 1, 2 )

    vPatientOutcome <- dfSimDataStage1$Response
    vPatientTreatment <- dfSimDataStage1$TreatmentID

    # Control group outcomes
    vOutcomesCtrl <- vPatientOutcome[ vPatientTreatment == 0 ]
    dMeanCtrl <- mean( vOutcomesCtrl )

    # For each treatment arm, compute response rate difference vs control
    vDelta <- rep( NA, nTrtArms )

    for ( nTrtID in 1:nTrtArms ) {
        vOutcomesTrt <- vPatientOutcome[ vPatientTreatment == nTrtID ]

        if ( length( vOutcomesTrt ) > 0 && length( vOutcomesCtrl ) > 0 ) {
            dMeanTrt <- mean( vOutcomesTrt )
            vDelta[ nTrtID ] <- dMeanTrt - dMeanCtrl
        }
    }

    if ( all( is.na( vDelta ) ) ) {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -4 ) )
    }

    # Futility Assessment: if no arm meets minimum effectiveness threshold → stop trial
    dBestDelta <- max( vDelta, na.rm = TRUE )

    if ( dBestDelta < UserParam$Stage1FutThreshold ) {
        strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
            bIAFutilityCondition = TRUE
        )

        nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

        return( ReturnResult(
            nTrtArms = nTrtArms,
            nErrorCode = nErrorCode,
            nBestArm = NA_integer_,
            dCriticalPoint = NA_real_,
            dTS = NA_real_,
            nDecision = nDecision
        ) )
    }

    # Select the arm with the largest response difference (in case of a tie, arm with lower index is chosen)
    nBestArm <- which( vDelta == max( vDelta, na.rm = TRUE ) )

    if ( length( nBestArm ) > 1 ) {
        nBestArm <- min( nBestArm )
    }

    #-------------------------------------------------------------------------------------------------------------------------
    # Stage 2: Efficacy analysis using PFS data for selected arm vs control ####

    # Subset SimData to only include patients from control and selected arm
    vIndxPatientsToBeUsedInStage2 <- which( SimData$TreatmentID == 0 | SimData$TreatmentID == nBestArm )
    dfSimDataStage2 <- SimData[ vIndxPatientsToBeUsedInStage2, ]

    # Create a treatment indicator: 1 = selected treatment arm, 0 = control
    dfSimDataStage2$Trt <- ifelse( dfSimDataStage2$TreatmentID == nBestArm, 1, 0 )

    # Simulate dropout during PFS follow-up
    if ( !is.null( UserParam$DropoutProportion ) && UserParam$DropoutProportion > 0 ) {
        # Number of patients who drop out
        nDropouts <- floor( nrow( dfSimDataStage2 ) * UserParam$DropoutProportion )

        # Randomly select dropout patients
        vDropoutIndices <- if ( nDropouts > 0 ) {
            sample( seq_len( nrow( dfSimDataStage2 ) ), nDropouts )
        } else {
            integer( 0 )
        }

        # Create dropout vector: 0 = patient stays, 1 = patient drops out
        dfSimDataStage2$Dropout <- 0
        dfSimDataStage2$Dropout[ vDropoutIndices ] <- 1

        # Default dropout time = Inf
        dfSimDataStage2$DropoutTime <- Inf

        if ( nDropouts > 0 ) {
            # For dropout patients, simulate uniform dropout time between 0 and PFS
            dfSimDataStage2$DropoutTime[ vDropoutIndices ] <- stats::runif(
                n = length( vDropoutIndices ),
                min = 0,
                max = dfSimDataStage2$PFSNonCens[ vDropoutIndices ]
            )
        }
    } else {
        # No dropout scenario
        dfSimDataStage2$Dropout <- 0
        dfSimDataStage2$DropoutTime <- Inf
    }

    # Calculate actual time of dropout
    dfSimDataStage2$TimeOfDropout <- dfSimDataStage2$ArrivalTime + dfSimDataStage2$DropoutTime

    # PFSEventWithDropout:
    #   0 = patient censored due to dropout before PFS event
    #   1 = PFS event observed before dropout
    dfSimDataStage2$PFSEventWithDropout <- as.integer( dfSimDataStage2$TimeOfDropout >= dfSimDataStage2$TimeOfPFSEvent )

    # Total number of PFS events observed in the data
    vPFSEventTimes <- dfSimDataStage2$TimeOfPFSEvent[ dfSimDataStage2$PFSEventWithDropout == 1 ]

    # Check that the target number of observed PFS events has been reached
    if ( length( vPFSEventTimes ) < nTargetNumPFSEvents ) {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -5 ) )
    }
    dStage2AnalysisTiming <- sort( vPFSEventTimes )[ nTargetNumPFSEvents ]

    # Preparing data for analysis
    # PFSEvent: 1 = event, 0 = censored
    # PFSObservedTime: time for PFS analysis (time relative to the patient)
    # Note that as we have full data, we'll have to filter out patients that arrived after the Stage 2 analysis, then calculate PFS parameters:
    dfSimDataStage2 <- dfSimDataStage2[ dfSimDataStage2$ArrivalTime <= dStage2AnalysisTiming, ]

    dfSimDataStage2$PFSEvent <- ifelse( dfSimDataStage2$TimeOfPFSEvent <= dStage2AnalysisTiming &
        dfSimDataStage2$TimeOfPFSEvent <= dfSimDataStage2$TimeOfDropout,
    1, 0
    )

    dfSimDataStage2$PFSObservedTime <- pmin(
        dfSimDataStage2$TimeOfPFSEvent,
        dfSimDataStage2$TimeOfDropout,
        dStage2AnalysisTiming
    ) - dfSimDataStage2$ArrivalTime

    nStage2NumPatients <- nrow( dfSimDataStage2 )

    # Order the data by observed time
    dfSimDataStage2 <- dfSimDataStage2[ order( dfSimDataStage2$PFSObservedTime ), ]

    # Compute Observed HR
    coxModel <- survival::coxph( survival::Surv( PFSObservedTime, PFSEvent ) ~ Trt, data = dfSimDataStage2 )
    dTrueHR <- exp( coxModel$coefficients )

    dfSimDataStage2$EventOnTreatment <- ifelse( dfSimDataStage2$Trt == 1, dfSimDataStage2$PFSEvent, 0 )
    dfSimDataStage2$EventOnControl <- ifelse( dfSimDataStage2$Trt == 0, dfSimDataStage2$PFSEvent, 0 )

    nSubjectsAtRiskTreatment <- nrow( dfSimDataStage2[ dfSimDataStage2$Trt == 1, ] )
    nSubjectsAtRiskControl <- nrow( dfSimDataStage2[ dfSimDataStage2$Trt == 0, ] )

    # Initialize intermediate quantities required for test statistic computation
    dNum <- 0
    dDen <- 0

    # Iterate over subjects to calculate dNum and dDen required for test statistic computation
    for ( nSubject in seq_len( nrow( dfSimDataStage2 ) ) )
    { # Update the count of subjects at risk for each arm for non event times
        if ( dfSimDataStage2$PFSEvent[ nSubject ] == 0 ) {
            if ( dfSimDataStage2$Trt[ nSubject ] == 1 ) {
                nSubjectsAtRiskTreatment <- nSubjectsAtRiskTreatment - 1
            }
            if ( dfSimDataStage2$Trt[ nSubject ] == 0 ) {
                nSubjectsAtRiskControl <- nSubjectsAtRiskControl - 1
            }
        }
        # For subjects with events, compute dNum and dDen
        if ( dfSimDataStage2$PFSEvent[ nSubject ] == 1 ) {
            nEventsOnTreatment <- dfSimDataStage2$EventOnTreatment[ nSubject ]
            nEventsOnControl <- dfSimDataStage2$EventOnControl[ nSubject ]
            nEvents <- nEventsOnTreatment + nEventsOnControl
            nSubjectsAtRisk <- nSubjectsAtRiskTreatment + nSubjectsAtRiskControl

            # Equation Q.242 in East Horizon Manual
            dNum <- dNum + nEventsOnTreatment - nSubjectsAtRiskTreatment * nEvents / nSubjectsAtRisk

            # Generate dDen based on number of subjects at risk
            if ( nSubjectsAtRisk != 1 ) {
                # Equation Q.243 in East Horizon Manual
                dDen <- dDen + nSubjectsAtRiskTreatment * nSubjectsAtRiskControl * ( nSubjectsAtRisk - nEvents ) * nEvents / ( ( nSubjectsAtRisk - 1 ) * nSubjectsAtRisk^2 )
            }
            # Update the count of subjects at risk before the next iteration
            nSubjectsAtRiskTreatment <- nSubjectsAtRiskTreatment - nEventsOnTreatment
            nSubjectsAtRiskControl <- nSubjectsAtRiskControl - nEventsOnControl
        }
    }

    # Check that dDen is not zero
    if ( dDen == 0 ) {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -6 ) )
    }

    # Compute the log-rank test statistic
    dTS <- dNum / sqrt( dDen )

    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
        bIAEfficacyCondition = dTS < dCriticalPoint,
        bFAEfficacyCondition = dTS < dCriticalPoint
    )

    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    # Calculate the number of patients that were recruited (and their PFS data was used for analysis) at Stage 2
    dfStage2PatientPFSData <- dfSimDataStage2[ dfSimDataStage2$Stage == 2, ]

    vStage2RecruitedPatientsPerArm <- as.vector( table( dfStage2PatientPFSData$TreatmentID ) )

    nCompleters <- nrow( dfSimDataStage1 ) + nrow( dfStage2PatientPFSData )

    lRet <- ReturnResult(
        nTrtArms,
        nErrorCode,
        nBestArm,
        dCriticalPoint,
        dTS,
        nDecision,
        nStage1NumCompleters,
        nStage2NumPatients,
        dStage2AnalysisTiming,
        nCompleters,
        dTrueHR,
        vStage1RecruitedPatientsPerArm,
        vStage2RecruitedPatientsPerArm
    )

    return( lRet )
}

##################
ReturnResult <- function( nTrtArms,
                         nErrorCode,
                         nBestArm = NA_integer_,
                         dCriticalPoint = NA_real_,
                         dTS = NA_real_,
                         nDecision = NA_integer_,
                         nStage1NumCompleters = NA_integer_,
                         nStage2NumPatients = NA_integer_,
                         dStage2AnalysisTiming = NA_real_,
                         nCompleters = NA_integer_,
                         dTrueHR = NA_real_,
                         vStage1RecruitedPatientsPerArm = rep( NA_integer_, nTrtArms + 1 ),
                         vStage2RecruitedPatientsPerArm = rep( NA_integer_, 2 ) ) {
    vTestStat <- rep( 0, nTrtArms )
    vTestStat[ nBestArm ] <- dTS

    vDecision <- rep( 0, nTrtArms )
    vDecision[ nBestArm ] <- nDecision

    lRet <- list(
        TestStat = as.double( vTestStat ),
        Decision = as.integer( vDecision ),
        ErrorCode = as.integer( nErrorCode )
    )

    for ( i in 1:nTrtArms ) {
        lRet[[ paste0( "NumPatientInStage_", i ) ]] <- as.integer( ifelse( i == 1, nStage1NumCompleters,
            ifelse( i == 2, nStage2NumPatients, 0 )
        ) )
        lRet[[ paste0( "NumCompleters_", i ) ]] <- as.integer( ifelse( i == nBestArm, nCompleters, 0 ) )
        lRet[[ paste0( "ChosenArm_", i ) ]] <- as.integer( ifelse( i == nBestArm, nBestArm, 0 ) )
        lRet[[ paste0( "Stage2AnalysisTiming_", i ) ]] <- as.double( ifelse( i == nBestArm, dStage2AnalysisTiming, 0 ) )
        lRet[[ paste0( "CriticalPoint_", i ) ]] <- as.double( ifelse( i == nBestArm, dCriticalPoint, 0 ) )
        lRet[[ paste0( "HazardRatio_", i ) ]] <- as.double( ifelse( i == nBestArm, dTrueHR, 0 ) )
        lRet[[ paste0( "Control_Stage_", i, "_Patients" ) ]] <- as.double( ifelse( i == 1, vStage1RecruitedPatientsPerArm[ 1 ],
            ifelse( i == 2, vStage2RecruitedPatientsPerArm[ 1 ], 0 )
        ) )
        lRet[[ paste0( "Stage1Patients_Arm_", i ) ]] <- vStage1RecruitedPatientsPerArm[ i + 1 ]
        lRet[[ paste0( "Stage2Patients_Arm_", i ) ]] <- as.double( ifelse( i == nBestArm, vStage2RecruitedPatientsPerArm[ 2 ], 0 ) )
    }

    return( lRet )
}
