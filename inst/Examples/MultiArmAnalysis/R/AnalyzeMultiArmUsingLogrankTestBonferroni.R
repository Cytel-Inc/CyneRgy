######################################################################################################################## .
#' @name AnalyzeMultiArmUsingLogrankTestBonferroni
#' @title Analyze multi-arm time-to-event outcomes using Bonferroni-adjusted log-rank tests.
#' @description Analyze multi-arm time-to-event outcomes using Bonferroni-adjusted log-rank tests. Use the
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
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject.}
#' }
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$TailType`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{TailType}{Integer. Nature of critical region: - `0`: Left-tailed. - `1`: Right-tailed.}
#'   \item{MaxEvents}{Integer maximum number of events in the trial.}
#'   \item{NumTreatments}{Integer number of experimental treatment arms, excluding control.}
#'   \item{IsArmPresent}{Vector of Integer. Vector of length `DesignParam$NumTreatments` (number of arms - 1),
#'     indicating whether each arm is still in the trial or was dropped in the interim: - `0`: Dropped in the
#'     interim. - `1`: Still present. East Horizon Explore: Fixed to `1` for the first look and for `Statistical
#'     Design = Fixed Sample`. East Horizon Design: Fixed to `1` for the first look and for `Statistical Design =
#'     Fixed Sample`.}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumLooks}{Integer total number of analysis looks.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{InfoFrac}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the information fraction
#'     for each look.}
#'   \item{RejType}{Integer one-sided rejection type supported by the decision helpers: 0 = upper efficacy;
#'     1 = upper futility; 2 = lower efficacy; 3 = lower futility; 4 = upper efficacy and lower futility;
#'     5 = lower efficacy and upper futility.}
#'   \item{EffBdryScale}{Integer. Efficacy boundary scale. East Horizon Explore: Not available for `Study Objective
#'     = Dose Finding`. Possible values: - `0`: Z scale. East Horizon Design: Possible values: `1`: Adjusted
#'     p-value scale.}
#'   \item{EffBdry}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the efficacy boundary
#'     values for each look. East Horizon Explore: Not available for `Study Objective = Dose Finding`.}
#' }
#' @param OutList Optional named list used to pass outputs between analysis looks. Return it at one look to receive
#'   the same list as input at the next look; the input is NULL at the first look. Access elements by name.
#'   Available for designs that support passing state between looks.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore). Return one value per experimental arm in treatment-ID order.}
#'   \item{HR}{Numeric vector of estimated treatment-to-control hazard ratios, one per experimental arm in
#'     treatment-ID order.}
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
#' @details
#' ## CyneRgy Decision Helpers
#'
#' `CyneRgy::GetDecisionString()` determines the decision label, and
#' `CyneRgy::GetDecision()` converts it to the integer decision returned to East Horizon.
#' When using these helpers, include the following fields in sample/test data as well as simulation inputs:
#'
#' - `DesignParam$TailType`: test direction; `0` = left-tailed, `1` = right-tailed.
#' - For a group sequential design, `LookInfo$NumLooks` (total number of looks),
#'   `LookInfo$CurrLookIndex` (current look, starting at 1), and `LookInfo$RejType`
#'   (enabled stopping boundaries) are required.
#' - For a fixed-sample design, pass `LookInfo = NULL` explicitly to both helpers,
#'   and pass `nLookIndex = 1` and `nQtyOfLooks = 1` to `GetDecisionString()`.
#'
#' Supported one-sided `LookInfo$RejType` values:
#'
#' - `0`: upper efficacy only.
#' - `1`: upper futility only.
#' - `2`: lower efficacy only.
#' - `3`: lower futility only.
#' - `4`: upper efficacy and lower futility.
#' - `5`: lower efficacy and upper futility.
#'
#' The helpers do not support `RejType` values `6` (two-sided efficacy only),
#' `7` (two-sided futility only), `8` (two-sided efficacy and futility), or `9` (equivalence).
#' The helper requirements above apply in addition to the inputs used by the analysis itself.
######################################################################################################################## .

AnalyzeMultiArmUsingLogrankTestBonferroni <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL, OutList = NULL ) {
    # Retrieve necessary information from the objects East Horizon sent
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfEvents <- LookInfo$InfoFrac[ nLookIndex ] * DesignParam$MaxEvents
        dEffBoundary <- LookInfo$EffBdry[ nLookIndex ]

        # A p-value-scale boundary is already on the comparison scale.
        if ( !is.null( LookInfo$EffBdryScale ) && LookInfo$EffBdryScale == 1 ) {
            dBoundaryPScale <- dEffBoundary
        } else if ( DesignParam$TailType == 1 ) {
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

    vEventTimes <- SimData$TimeOfEvent
    dTimeOfAnalysis <- vEventTimes[ nQtyOfEvents ]

    SimData <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ]
    SimData$Event <- as.integer( SimData$TimeOfEvent <= dTimeOfAnalysis )
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
            dfSimDataTrt <- SimData[ SimData$TreatmentID %in% c( 0, nTrtID ), ]

            # Compute Observed HR
            cCoxModel <- survival::coxph(
                survival::Surv( ObservedTime, Event ) ~ factor( TreatmentID ),
                data = dfSimDataTrt
            )

            # Compute the test statistic using survival package
            cLogrankTest <- survival::survdiff(
                survival::Surv( ObservedTime, Event ) ~ TreatmentID,
                data = dfSimDataTrt
            )

            vHRRatio[ nTrtID ] <- as.numeric( exp( cCoxModel$coefficients ) )
            dZValue <- sqrt( cLogrankTest$chisq ) * sign( cLogrankTest$obs[ 2 ] - cLogrankTest$exp[ 2 ] )
            vPValues[ nTrtID ] <- stats::pnorm( dZValue, lower.tail = DesignParam$TailType == 0 )
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
