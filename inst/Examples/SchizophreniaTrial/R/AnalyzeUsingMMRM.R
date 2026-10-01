######################################################################################################################## .
#' @name AnalyzeUsingMMRM
#' @title Perform MMRM analysis
#' @description This function fits a Mixed Model for Repeated Measures (MMRM) to simulated patient data, and
#'   returns the treatment effect estimate, p-value, and decision outcome.
#' @author Jacob Wathen
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{Response1, ..., ResponseNumVisit}{Numeric response vectors, one per visit, with one element per subject.
#'     Replace NumVisit by the actual number of visits.}
#'   \item{CensorInd1, ..., CensorIndNumVisit}{Integer censor-indicator vectors, one per visit: 0 =
#'     dropout/non-completer; 1 = completer.}
#'   \item{DropOutTime}{Numeric vector of generated dropout times measured from each subject's enrollment, with one
#'     element per subject. Inf indicates no dropout.}
#'   \item{DropoutVisitID}{Integer vector of 1-based visit IDs after which subjects drop out, with one element per
#'     subject.}
#'   \item{ArrTimeVisit[VisitID]}{Numeric vector of visit times measured from each subject's enrollment,
#'     with one element per subject. Replace VisitID by the actual visit number. Add ArrivalTime to obtain
#'     calendar visit times.}
#' }
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
#'   \item{MuC}{Numeric. Design mean for the control arm. East Horizon Explore: Not available.}
#'   \item{Sigma}{Numeric. Design standard deviation specified in simulations. East Horizon Explore: Not available.
#'     East Horizon Design: Only available for `Test = Difference of Means` (Continuous) and `Test Stat Type = 3
#'     (Z-test)`.}
#'   \item{NumVisit}{Integer number of visits. East Horizon Explore: Only available for `Endpoint Type =
#'     Continuous with Repeated Measures`. East Horizon Design: Not available.}
#'   \item{VisitTime}{Numeric vector of visit times measured from enrollment, of length NumVisit and
#'     ordered by visit. East Horizon Explore: Only available for `Endpoint Type = Continuous with
#'     Repeated Measures`. East Horizon Design: Not available.}
#'   \item{VisitStatus}{Vector of Integer. Vector of length `NumVisit`, indicating the visit selection status for
#'     each visit: – `0`: Visit has not been selected for analysis. – `1`: Visit has been selected for analysis.
#'     East Horizon Explore: Only available for `Endpoint Type = Continuous with Repeated Measures`. East Horizon
#'     Design: Not available.}
#'   \item{PrimContrastCoeff}{Vector of Numeric. Vector of length `NumVisit`, indicating the primary contrast
#'     coefficient for each visit. East Horizon Explore: Only available for `Endpoint Type = Continuous with
#'     Repeated Measures`. East Horizon Design: Not available.}
#'   \item{SecContrastCoeff}{Vector of Numeric. Vector of length `NumVisit`, indicating the secondary contrast
#'     coefficient for each visit. East Horizon Explore: Only available for `Endpoint Type = Continuous with
#'     Repeated Measures`. Set to `NULL` for `Statistical Design = Fixed Sample`. East Horizon Design: Not
#'     available.}
#'   \item{DropImp}{Integer. Dropout imputation method: – `0`: None. – `1`: Last observation carried forward
#'     (LOCF). East Horizon Explore: Only available for `Endpoint Type = Continuous with Repeated Measures`. East
#'     Horizon Design: Not available.}
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
#'   \item{CumCompleters}{Vector of Integer. Vector of length `LookInfo$NumLooks`, containing the cumulative number
#'     of completers for each look. East Horizon Explore: Not available for `Endpoint Type = Time-to-Event` and for
#'     Vaccine Efficacy (`Endpoint Type = Binary` with Lower Value and `Test = 1 - Ratio of Proportions or 1 -
#'     Ratio of Poisson Rates`). East Horizon Design: Not available for `Time-to-Event` tests.}
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
#'   \item{InterimVisit}{Integer. Index of the visit which is driving the interims (starts at 1). East Horizon
#'     Explore: Only available for `Endpoint Type = Continuous with Repeated Measures`. East Horizon Design: Not
#'     available.}
#'   \item{FutContrast}{Integer. The contrast based on which futility boundaries are being computed: – `0`: Primary
#'     contrast. – `1`: Secondary contrast. East Horizon Explore: Only available for `Endpoint Type = Continuous
#'     with Repeated Measures`. East Horizon Design: Not available.}
#'   \item{IncludePipeline}{Integer. Indicates whether to include pipeline subjects in the interim: – `0`: Do not
#'     include. – `1`: Include. East Horizon Explore: Only available for `Endpoint Type = Continuous with Repeated
#'     Measures`. East Horizon Design: Not available.}
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
#'   \item{PrimDelta}{Estimated treatment effect for the primary contrast. Required for Delta-scale futility using
#'     the primary contrast.}
#'   \item{SecDelta}{Estimated treatment effect for the secondary contrast. Required for Delta-scale futility using
#'     the secondary contrast.}
#'   \item{AnalysisTime}{Optional numeric calendar time of the analysis: the look time at an interim analysis and
#'     the study duration at the final analysis. Compute and return this value in the R function.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' Example-specific additional output elements:
#' \describe{
#'   \item{p.value}{p-value for the analysis}
#' }
#' @details For ordinary analysis designs, return either Decision to apply custom stopping logic or TestStat to let
#'   the engine apply its boundaries. Delta, event/completer counts, and standard errors may also be required for
#'   Delta-scale or conditional-power futility. Sample size re-estimation designs require a decision and the
#'   re-estimated total event/completer count.
#'
#' If only one post-baseline visit has been observed, the repeated-measures model reduces to a baseline-adjusted
#'   ANCOVA at that visit. Future visit responses are excluded from interim analyses.
#'
#' Example-specific output usage: PrimDelta: Estimated treatment effect from the MMRM model at the final visit.
######################################################################################################################## .

AnalyzeUsingMMRM <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    # Initialize outputs
    nErrorCode <- 0
    nDecision <- 0
    dPrimDelta <- NA_real_

    # Step 1: Setup LooksInfo ####
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfPatsForInterim <- LookInfo$CumCompleters[ nLookIndex ]
        nAnalysisVisit <- LookInfo$InterimVisit
    } else {
        nLookIndex <- 1
        nQtyOfLooks <- 1
        nQtyOfPatsForInterim <- nrow( SimData )
    }

    # Step 2: Create analysis dataset ####
    dfNoBaselineAnalysisData <- CreateAnalysisDataset( SimData, LookInfo )

    # Step 3: Fit the MMRM with default CS + varIdent ####
    lLMEControls <- nlme::lmeControl(
        opt = "optim",
        optimMethod = "BFGS",
        optimCtrl = list( maxit = 500 ),
        nlminb = list( iter.max = 100, eval.max = 200 )
    )

    # Make last visit the reference level
    strLastVisit <- utils::tail( levels( dfNoBaselineAnalysisData$Visit ), 1 )

    dfNoBaselineAnalysisData$Visit <- stats::relevel( dfNoBaselineAnalysisData$Visit, ref = strLastVisit )

    nObservedVisits <- length( unique( dfNoBaselineAnalysisData$Visit ) )
    cMMRMModel <- tryCatch(
        {
            if ( nObservedVisits == 1 ) {
                # With one follow-up per subject, the repeated-measures model reduces to ANCOVA.
                nlme::gls( Response ~ Baseline + TreatmentID,
                    data = dfNoBaselineAnalysisData, method = "REML", na.action = stats::na.omit
                )
            } else {
                nlme::lme( Response ~ Baseline + TreatmentID * Visit,
                    random      = ~ 1 | Id,
                    correlation = nlme::corCompSymm( form = ~ 1 | Id ),
                    weights     = nlme::varIdent( form = ~ 1 | Visit ),
                    data        = dfNoBaselineAnalysisData,
                    method      = "REML",
                    na.action   = stats::na.omit,
                    control     = lLMEControls
                )
            }
        },
        error = function( e ) {
            return( NULL )
        }
    )

    # Step 3b: Extract the treatment × last‐visit effect ####
    if ( !is.null( cMMRMModel ) ) {
        mCoefficients <- summary( cMMRMModel )$tTable
        if ( inherits( cMMRMModel, "gls" ) ) {
            mCoefficients <- cbind( mCoefficients, DF = cMMRMModel$dims$N - cMMRMModel$dims$p )
        }
        strTrtRow <- grep( "^TreatmentID", rownames( mCoefficients ), value = TRUE )[ 1 ]

        if ( strTrtRow %in% rownames( mCoefficients ) ) {
            dPrimDelta <- mCoefficients[ strTrtRow, "Value" ]
            dStdError <- mCoefficients[ strTrtRow, "Std.Error" ]
            nDOF <- mCoefficients[ strTrtRow, "DF" ]
            dPValue <- stats::pt( mCoefficients[ strTrtRow, "t-value" ], nDOF,
                lower.tail = DesignParam$TailType == 0
            )
        } else {
            warning( "Treatment coefficient not found: ", strTrtRow )
            nErrorCode <- 1
            dPValue <- 1.0
        }
    } else {
        dPValue <- 1.0
        nErrorCode <- 1
    }

    # Step 4: Obtain group‐sequential alpha ####
    if ( !is.null( LookInfo ) ) {
        vInformationRates <- LookInfo$InfoFrac
        if ( is.null( vInformationRates ) ) {
            vInformationRates <- NA_real_
        }
        cGroupSequentialDesign <- rpact::getDesignGroupSequential(
            kMax = nQtyOfLooks,
            informationRates = vInformationRates,
            alpha = DesignParam$Alpha,
            sided = 1,
            typeOfDesign = "OF"
        )
        dAlpha <- cGroupSequentialDesign$stageLevels[ nLookIndex ]
    } else {
        dAlpha <- DesignParam$Alpha
    }

    # Step 5: Decision rules ####

    if ( dPValue <= dAlpha ) {
        if ( nLookIndex == nQtyOfLooks ) {
            # FA Efficacy condition
            bIAEfficacyCondition <- FALSE
            bFAEfficacyCondition <- TRUE
        } else {
            # IA Efficacy condition
            bIAEfficacyCondition <- TRUE
            bFAEfficacyCondition <- FALSE
        }
        # Efficacy decision
        strDecision <- CyneRgy::GetDecisionString(
            LookInfo = LookInfo,
            nLookIndex = nLookIndex,
            nQtyOfLooks = nQtyOfLooks,
            bIAEfficacyCondition = bIAEfficacyCondition,
            bFAEfficacyCondition = bFAEfficacyCondition
        )

        nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    } else {
        strDecision <- CyneRgy::GetDecisionString(
            LookInfo = LookInfo,
            nLookIndex = nLookIndex,
            nQtyOfLooks = nQtyOfLooks
        )

        nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    }

    # Step 6: Return analysis results ####
    lRet <- list(
        Decision = as.integer( nDecision ),
        PrimDelta = as.double( dPrimDelta ),
        p.value = as.double( dPValue ),
        ErrorCode = as.integer( nErrorCode )
    )

    return( lRet )
}
# Create a dataset for analysis ####

CreateAnalysisDataset <- function( SimData, LookInfo ) {
    # Step 1: Setup LooksInfo ####
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfPatsForInterim <- LookInfo$CumCompleters[ nLookIndex ]
        nAnalysisVisit <- LookInfo$InterimVisit
    } else {
        nLookIndex <- 1
        nQtyOfLooks <- 1
        nQtyOfPatsForInterim <- nrow( SimData )
    }

    # Step 2: Reshape wide → long in one shot ####
    dfLongData <- SimData |>
        dplyr::mutate( Id = dplyr::row_number( ) ) |>
        tidyr::pivot_longer(
            cols = tidyselect::matches( "^(Response|ArrTimeVisit)\\d+$" ),
            names_to = c( ".value", "Visit" ),
            names_pattern = "(Response|ArrTimeVisit)(\\d+)"
        ) |>
        dplyr::mutate(
            Visit = as.integer( Visit ),
            CalendarVisitTime = ArrivalTime + ArrTimeVisit
        ) |>
        dplyr::select( Id, TreatmentID, Visit, Response, CalendarVisitTime ) |>
        dplyr::arrange( Visit, CalendarVisitTime )

    # Step 3: Interim‐look filtering using dplyr ####

    if ( !is.null( LookInfo ) ) {
        # 3a) compute cutoff time
        dAnalysisTime <- dfLongData |>
            dplyr::filter( Visit == nAnalysisVisit ) |>
            dplyr::slice( nQtyOfPatsForInterim ) |>
            dplyr::pull( CalendarVisitTime )

        # 3b) pick subjects
        if ( LookInfo$IncludePipeline == 0 ) {
            vSubjectsForAnalysis <- dfLongData |>
                dplyr::filter(
                    Visit == nAnalysisVisit,
                    CalendarVisitTime <= dAnalysisTime
                ) |>
                dplyr::distinct( Id ) |>
                dplyr::pull( Id )
        } else {
            vSubjectsForAnalysis <- dfLongData |>
                dplyr::filter( CalendarVisitTime <= dAnalysisTime ) |>
                dplyr::distinct( Id ) |>
                dplyr::pull( Id )
        }

        dfAnalysisData <- dfLongData |>
            dplyr::filter( Id %in% vSubjectsForAnalysis )
        if ( nLookIndex < nQtyOfLooks ) {
            dfAnalysisData <- dfAnalysisData |>
                dplyr::filter( CalendarVisitTime <= dAnalysisTime )
        }
    } else {
        dfAnalysisData <- dfLongData
    }

    # Step 4: Prepare for MMRM ####
    dfAnalysisData <- dfAnalysisData |>
        dplyr::mutate(
            Visit = factor( Visit ),
            TreatmentID = factor( TreatmentID ),
            Id = factor( Id )
        )

    # Step 5: Create a dataset ####
    # The dataset removes the baseline visit from the long form and adds the baseline response as a new column
    dfNoBaselineAnalysisData <- dplyr::filter( dfAnalysisData, Visit != 1 )
    dfBaselineAnalysisData <- dplyr::filter( dfAnalysisData, Visit == 1 ) |>
        dplyr::select( Id, Baseline = Response )
    dfNoBaselineAnalysisData <- dplyr::left_join( dfNoBaselineAnalysisData, dfBaselineAnalysisData, by = "Id" )

    return( dfNoBaselineAnalysisData )
}
