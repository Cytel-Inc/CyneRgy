######################################################################################################################## .
#' @name Analyze.RepeatedMeasures
#'
#' @title Analyze repeated-measures subject responses
#'
#' @description Analyze repeated-measures subject responses. Use the documented inputs and outputs to integrate
#'   this function with the simulation workflow.
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
#'   \item{Response1, ..., ResponseNumVisit}{Numeric response vectors, one per visit, with one element per subject.
#'     Replace NumVisit by the actual number of visits.}
#'   \item{CensorInd1, ..., CensorIndNumVisit}{Integer censor-indicator vectors, one per visit: 0 =
#'     dropout/non-completer; 1 = completer.}
#'   \item{DropOutTime}{Numeric vector of generated dropout times measured from each subject's enrollment, with one
#'     element per subject. Inf indicates no dropout.}
#'   \item{DropoutVisitID}{Integer vector of 1-based visit IDs after which subjects drop out, with one element per
#'     subject.}
#'   \item{ArrTimeVisit[VisitID]}{Optional custom numeric vector of subject arrival times on the calendar scale for
#'     visit VisitID. Replace VisitID by the actual visit number.}
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
#'   \item{MuC}{Numeric. Design mean for the control arm. East Horizon Explore: Not available.}
#'   \item{Sigma}{Numeric. Design standard deviation specified in simulations. East Horizon Explore: Not available.
#'     East Horizon Design: Only available for `Test = Difference of Means` (Continuous) and `Test Stat Type = 3
#'     (Z-test)`.}
#'   \item{NumVisit}{Integer. Number of visits. East Horizon Explore: Only available for `Endpoint Type =
#'     Continuous with Repeated Measures`. East Horizon Design: Not available.}
#'   \item{VisitTime}{Vector of Numeric. Vector of length `NumVisit`, indicating the time for each visit. East
#'     Horizon Explore: Only available for `Endpoint Type = Continuous with Repeated Measures`. East Horizon
#'     Design: Not available.}
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
#' @details For ordinary analysis designs, return either Decision to apply custom stopping logic or TestStat to let
#'   the engine apply its boundaries. Delta, event/completer counts, and standard errors may also be required for
#'   Delta-scale or conditional-power futility. Sample size re-estimation designs require a decision and the
#'   re-estimated total event/completer count. This example may use only a subset of the documented design fields.
######################################################################################################################## .

Analyze.RepeatedMeasures <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    # This example is for the case of no dropouts only
    nErrorCode <- 0
    nDecision <- 0
    dPrimDelta <- 0
    dSecDelta <- 0

    # Step 1: Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfPatsForInterim <- LookInfo$CumCompleters[ nLookIndex ]
        nAnalysisVisit <- LookInfo$InterimVisit
        nRejType <- LookInfo$RejType
        nTailType <- DesignParam$TailType
    } else {
        nLookIndex <- 1
        nQtyOfLooks <- 1
        nQtyOfPatsForInterim <- nrow( SimData )
        nTailType <- DesignParam$TailType
    }

    dfWideData <- data.frame( id = 1:DesignParam$SampleSize, TreatmentID = SimData$TreatmentID )
    vResponseColumns <- c( )
    vVisitTimesColumns <- c( )
    for ( i in 1:DesignParam$NumVisit ) {
        dfWideData[[ paste0( "Response", i ) ]] <- SimData[ , paste0( "Response", i ) ]
        vResponseColumns <- c( vResponseColumns, paste0( "Response", i ) )
        dfWideData[[ paste0( "CalendarVisitTime", i ) ]] <- SimData[ , "ArrivalTime" ] + SimData[ , paste0( "ArrTimeVisit", i ) ]
        vVisitTimesColumns <- c( vVisitTimesColumns, paste0( "CalendarVisitTime", i ) )
    }

    dfLongData <- stats::reshape( dfWideData,
        varying = c( vResponseColumns, vVisitTimesColumns ),
        direction = "long", sep = "", idvar = "id", timevar = "Visit"
    )
    dfLongData <- dfLongData[ order( dfLongData$Visit, dfLongData$CalendarVisitTime ), ]

    if ( !is.null( LookInfo ) ) {
        dAnalysisTime <- dfLongData[ dfLongData[[ "Visit" ]] == nAnalysisVisit, ][ nQtyOfPatsForInterim, "CalendarVisitTime" ]

        if ( LookInfo$IncludePipeline == 0 ) {
            vSubjectsForAnalysis <- unique( dfLongData[ dfLongData[[ "Visit" ]] == nAnalysisVisit & dfLongData[[ "CalendarVisitTime" ]] <= dAnalysisTime, "id" ] )
        } else {
            vSubjectsForAnalysis <- unique( dfLongData[ dfLongData[[ "CalendarVisitTime" ]] <= dAnalysisTime, "id" ] )
        }

        dfAnalysisData <- dfLongData[ dfLongData[[ "id" ]] %in% vSubjectsForAnalysis, ]
    } else {
        dfAnalysisData <- dfLongData
    }

    mmrm <- nlme::gls( Response ~ TreatmentID,
        na.action = stats::na.omit, data = dfAnalysisData,
        correlation = nlme::corSymm( form = ~ Visit | id ),
        weights = nlme::varIdent( form = ~ 1 | Visit )
    )

    dpValue <- summary( mmrm )$tTable[ "TreatmentID", "p-value" ]

    # Get group sequential boundaries
    if ( !is.null( LookInfo ) ) {
        vGroupSequentialBoundaries <- rpact::getDesignGroupSequential( kMax = nQtyOfLooks, alpha = DesignParam$Alpha, sided = 1, typeOfDesign = "OF" )
        dAlpha <- vGroupSequentialBoundaries$alphaSpent[ nLookIndex ]
    } else {
        dAlpha <- DesignParam$Alpha
    }

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
        bIAEfficacyCondition = dpValue <= dAlpha,
        bFAEfficacyCondition = dpValue <= dAlpha
    )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    return( list( Decision = as.integer( nDecision ), PrimDelta = as.double( dPrimDelta ), SecDelta = as.double( dSecDelta ), ErrorCode = as.integer( nErrorCode ) ) )
}
