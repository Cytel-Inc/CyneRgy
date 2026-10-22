######################################################################################################################## .
#' @name AnalyzeSurvivalDataUsingCoxPH
#' @title Analyze time-to-event subject outcomes
#' @description Analyze time-to-event subject outcomes. Use the documented inputs and outputs to integrate this
#'   function with the simulation workflow.
#' @author J. Kyle Wathen, Laurent Spiess, Gabriel Potvin
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
#'
#' Additional fields returned by SimulatePatientSurvivalAssuranceUsingPh2Prior:
#' \describe{
#'   \item{TrueHR}{Numeric vector of sampled treatment-to-control hazard ratios, with one element per subject.
#'     The same hazard ratio is repeated for all subjects in a simulation.}
#' }
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$TailType`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{TailType}{Integer. Nature of critical region: - `0`: Left-tailed. - `1`: Right-tailed. East Horizon
#'     Design: Only available if `Test Type = One-sided`.}
#'   \item{MaxEvents}{Integer maximum number of events in the trial.}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumLooks}{Integer total number of analysis looks.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{CumEvents}{Cumulative event-count vector indexed by LookInfo$CurrLookIndex in this example.}
#'   \item{RejType}{Integer one-sided rejection type supported by the decision helpers: 0 = upper efficacy;
#'     1 = upper futility; 2 = lower efficacy; 3 = lower futility; 4 = upper efficacy and lower futility;
#'     5 = lower efficacy and upper futility.}
#'   \item{EffBdryScale}{Integer. Efficacy boundary scale. East Horizon Explore: Possible values: - `0`: Z scale.
#'     East Horizon Design: Possible values: - `0`: Z scale. - `1`: p-value scale.}
#'   \item{EffBdry}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the efficacy boundary
#'     values (for one-sided tests) for each look. East Horizon Explore: Set to `NA` for `Endpoint Type =
#'     Continuous with Repeated Measures`. East Horizon Design: Only available if `Test Type = One-sided`.}
#' }
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' \describe{
#'   \item{bReturnLogTrueHazard}{Logical flag indicating whether to return the natural logarithm of the true hazard
#'     ratio in the custom HazardRatio and TrueHR outputs. Default: FALSE.}
#'   \item{bReturnNAForNoGoTrials}{Logical flag indicating whether to return NA for the custom true-hazard outputs
#'     when the trial does not meet the efficacy criterion. Default: FALSE.}
#' }
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore).}
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details This example implements a one-sided Cox test. The p-value follows DesignParam$TailType and is
#'   compared with Alpha for a fixed-sample design or the current efficacy boundary for a sequential design.
#'
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

AnalyzeSurvivalDataUsingCoxPH <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    nErrorCode <- 0
    nLookIndex <- 1

    if ( !is.null( LookInfo ) ) {
        # Look info was provided so use it
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfEvents <- LookInfo$CumEvents[ nLookIndex ]
    } else {
        nQtyOfEvents <- DesignParam$MaxEvents
    }

    if ( is.null( UserParam ) ) {
        UserParam <- list( bReturnLogTrueHazard = FALSE, bReturnNAForNoGoTrials = FALSE )
    }

    SimData$TimeOfEvent <- SimData$ArrivalTime + SimData$SurvivalTime # This is the calendar time in the trial that the patients event is observed

    # Compute the time of analysis
    SimData <- SimData[ order( SimData$TimeOfEvent ), ]
    vEventTimes <- SimData$TimeOfEvent
    dTimeOfAnalysis <- vEventTimes[ nQtyOfEvents ]

    # Add the Observed Time variable
    SimData <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ] # Exclude any patients that were not enrolled by the time of the analysis
    SimData$Event <- as.integer( SimData$TimeOfEvent <= dTimeOfAnalysis ) # If the event is observed after the analysis it is not observed, eg censored
    SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )

    # Fit a cox model
    cCoxModel <- survival::coxph( survival::Surv( ObservedTime, Event ) ~ as.factor( TreatmentID ), data = SimData )
    dPValue <- summary( cCoxModel )$coefficients[ , "Pr(>|z|)" ]
    dZVal <- summary( cCoxModel )$coefficients[ , "z" ]
    dPValue <- stats::pnorm( dZVal, lower.tail = DesignParam$TailType == 0 )
    dPValueCutoff <- DesignParam$Alpha
    if ( !is.null( LookInfo ) ) {
        if ( !is.null( LookInfo$EffBdryScale ) && LookInfo$EffBdryScale == 1 ) {
            dPValueCutoff <- LookInfo$EffBdry[ nLookIndex ]
        } else {
            dPValueCutoff <- stats::pnorm( LookInfo$EffBdry[ nLookIndex ],
                lower.tail = DesignParam$TailType == 0 )
        }
    }
    nQtyOfLooks <- 1
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
    }
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
        bIAEfficacyCondition = dPValue <= dPValueCutoff,
        bFAEfficacyCondition = dPValue <= dPValueCutoff
    )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    #
    # Decision Code
    # 0  No Boundary Crossed
    # 1  Lower Efficacy Boundary Crossed
    # 2  Upper Efficacy Boundary Crossed
    # 3  Futility Boundary Crossed
    # 4  Equivalence Boundary Crossed
    #

    dTrueHR <- as.double( SimData$TrueHR[ 1 ] )

    if ( isTRUE( as.logical( UserParam$bReturnLogTrueHazard ) ) ) {
        dTrueHR <- log( dTrueHR )
    }

    if ( isTRUE( as.logical( UserParam$bReturnNAForNoGoTrials ) ) && !nDecision %in% c( 1, 2 ) ) {
        dTrueHR <- NA
    }
    lRet <- list(
        TestStat = as.double( dZVal ),
        Decision = as.integer( nDecision ),
        ErrorCode = as.integer( nErrorCode ),
        dPValue = as.double( dPValue ),
        HazardRatio = as.double( dTrueHR ),
        TrueHR = as.double( dTrueHR )
    )

    return( lRet )
}
