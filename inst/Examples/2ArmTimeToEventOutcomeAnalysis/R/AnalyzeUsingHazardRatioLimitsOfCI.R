######################################################################################################################## .
#' @name AnalyzeUsingHazardRatioLimitsOfCI
#' @title Analyze using a simplified limits of confidence interval design
#' @description In this simplified example of upper and lower confidence boundary designs, if it is likely that the
#'   HR (Hazard Ratio) is below the Minimum Acceptable Value (MAV) then a Go decision is made. If a Go decision is
#'   not made, then if it is unlikely that the Hazard ratio is below the Target Value (TV) a No Go decision is
#'   made. In this example, the coxph() from survival package in R is utilized to analyze the data and compute
#'   estimate of log HR and Std error of log HR. The team would like to make a Go decision if there is at least a
#'   90\% chance that HR is below the MAV. If a Go decision is not made, then a No Go decision is made if
#'   there is less than a 10\% chance the HR is less than the TV.
#'
#' Specifically, if user provides upper and lower limit in Hazard ratio scale then, 1. For Hazard Ratio if UL <
#'   UserParam$dMAV --> Go if LL > UserParam$dTV --> No Go Otherwise, continue to the next analysis.
#'
#' 2. For log Hazard Ratio if UL < log( UserParam$dMAV ) --> Go if LL > log( UserParam$dTV ) --> No Go Otherwise, continue to the
#'   next analysis. Note - HR and log HR are monotonically related. In coxph() function, we get the analysis for
#'   log HR and hence we make the use of 2) in decision making.
#'
#' At the Final Analysis: If UL < UserParam$dMAV then a Go decision is made, otherwise, a No Go decision is made.
#' @author Anoop Singh Rawat, Shubham Lahoti, and Gabriel Potvin
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
#'   \item{TailType}{Integer. Nature of critical region: - `0`: Left-tailed. - `1`: Right-tailed. East Horizon
#'     Design: Only available if `Test Type = One-sided`.}
#'   \item{CriticalPoint}{Numeric. Critical value (for one-sided tests). East Horizon Explore: Only available if
#'     `Statistical Design = Fixed Sample`. East Horizon Design: Only available if `Test Type = One-sided` and
#'     `Statistical Design = Fixed Sample`.}
#'   \item{MaxEvents}{Integer maximum number of events in the trial.}
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
#'   \item{EffBdryLower}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the lower efficacy
#'     boundary values (for two-sided tests) for each look. East Horizon Explore: Only available if `Tail Type =
#'     Left-tailed`. Two-sided tests do not exist, so this variable is not useful: use EffBdry instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Left-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
#' }
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' UserParam must be supplied, the list must contain the following named elements:
#' \describe{
#'   \item{UserParam$dMAV}{Positive numeric Minimum Acceptable Value (MAV) for the hazard ratio. A Go
#'     decision is made when the upper confidence limit is below this value.}
#'   \item{UserParam$dTV}{Positive numeric Target Value (TV) for the hazard ratio. A No-Go decision is
#'     made when the lower confidence limit is above this value.}
#'   \item{UserParam$dConfLevel}{Numeric confidence level in (0, 1) used to construct the confidence
#'     interval.}
#' }
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore).}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' Example-specific additional output elements:
#' \describe{
#'   \item{HazardRatio}{Estimated treatment-to-control hazard ratio, retained as a custom output for reporting.}
#' }
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

AnalyzeUsingHazardRatioLimitsOfCI <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    if ( is.null( UserParam ) ) {
        return( list( Decision = 0L, ErrorCode = -1L ) )
    }

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
    vEventTimes <- SimData$TimeOfEvent
    dTimeOfAnalysis <- vEventTimes[ nQtyOfEvents ]

    # Add the Observed Time variable
    SimData <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ] # Exclude any patients that were not enrolled by the time of the analysis
    SimData$Event <- as.integer( SimData$TimeOfEvent <= dTimeOfAnalysis ) # If the event is observed after the analysis it is not observed, eg censored
    SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )

    # Order the data by observed time for the remainder of the computations
    SimData <- SimData[ order( SimData$ObservedTime ), ]

    # Compute Observed HR
    cCoxModel <- survival::coxph( survival::Surv( ObservedTime, Event ) ~ TreatmentID, data = SimData )

    # find the value of observed Log(HR) and SE(log(HR))
    dLogHR <- summary( cCoxModel )$coefficients[ 1 ]
    dStdError <- summary( cCoxModel )$coefficients[ 3 ]

    # Log HR follows Normal distribution with mean = observed log HR on line no 83 and Std error given on line no 84
    # Critical value for Z test is given as,
    dAlpha <- 1 - UserParam$dConfLevel
    dZalpha <- stats::qnorm( 1 - dAlpha / 2 )

    # Confidence Interval for log HR is given as,

    dLowerLimitCI <- dLogHR - dZalpha * dStdError
    dUpperLimitCI <- dLogHR + dZalpha * dStdError

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
        bIAEfficacyCondition = dUpperLimitCI < log( UserParam$dMAV ),
        bIAFutilityCondition = dLowerLimitCI > log( UserParam$dTV ),
        bFAEfficacyCondition = dUpperLimitCI < log( UserParam$dMAV )
    )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    nErrorCode <- 0

    return( list(
        HazardRatio = as.double( exp( dLogHR ) ),
        ErrorCode = as.integer( nErrorCode ),
        Decision = as.integer( nDecision )
    ) )
}