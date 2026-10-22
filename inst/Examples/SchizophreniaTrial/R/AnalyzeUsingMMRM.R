######################################################################################################################## .
#' @name AnalyzeUsingMMRM
#' @title Perform MMRM analysis
#' @description This function fits a Mixed Model for Repeated Measures (MMRM) to simulated patient data, and
#'   returns the treatment effect estimate, p-value, and decision outcome.
#' @author Jacob Wathen
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$TreatmentID`. Columns include the fields below when applicable,
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
#'   \item{DropoutVisitID}{Integer vector of 1-based visit IDs after which subjects drop out, with one element per
#'     subject.}
#'   \item{ArrTimeVisit[VisitID]}{Numeric vector of visit times measured from each subject's enrollment,
#'     with one element per subject. Replace VisitID by the actual visit number. Add ArrivalTime to obtain
#'     calendar visit times.}
#' }
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$TailType`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{TailType}{Integer. Nature of critical region: - `0`: Left-tailed. - `1`: Right-tailed. East Horizon
#'     Design: Only available if `Test Type = One-sided`.}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumLooks}{Integer total number of analysis looks.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{InfoFrac}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the information fraction
#'     for each look.}
#'   \item{CumCompleters}{Integer vector of cumulative completer counts, one per look.}
#'   \item{RejType}{Integer one-sided rejection type supported by the decision helpers: 0 = upper efficacy;
#'     1 = upper futility; 2 = lower efficacy; 3 = lower futility; 4 = upper efficacy and lower futility;
#'     5 = lower efficacy and upper futility.}
#'   \item{InterimVisit}{Integer. Index of the visit which is driving the interims (starts at 1). East Horizon
#'     Explore: Only available for `Endpoint Type = Continuous with Repeated Measures`. East Horizon Design: Not
#'     available.}
#'   \item{IncludePipeline}{Integer. Indicates whether to include pipeline subjects in the interim: - `0`: Do not
#'     include. - `1`: Include. East Horizon Explore: Only available for `Endpoint Type = Continuous with Repeated
#'     Measures`. East Horizon Design: Not available.}
#' }
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore).}
#'   \item{PrimDelta}{Estimated treatment effect for the primary contrast. Required for Delta-scale futility using
#'     the primary contrast.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' Example-specific additional output elements:
#' \describe{
#'   \item{p.value}{p-value for the analysis}
#' }
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

    cMMRMModel <- tryCatch(
        {
            nlme::lme( Response ~ Baseline + TreatmentID * Visit,
                random      = ~ 1 | Id,
                correlation = nlme::corCompSymm( form = ~ 1 | Id ),
                weights     = nlme::varIdent( form = ~ 1 | Visit ),
                data        = dfNoBaselineAnalysisData,
                method      = "REML",
                na.action   = stats::na.omit,
                control     = lLMEControls
            )
        },
        error = function( e ) {
            return( NULL )
        }
    )

    # Step 3b: Extract the treatment × last‐visit effect ####
    if ( !is.null( cMMRMModel ) ) {
        mCoefficients <- summary( cMMRMModel )$tTable
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
