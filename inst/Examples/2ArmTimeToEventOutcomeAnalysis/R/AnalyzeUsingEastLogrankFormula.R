######################################################################################################################## .
#' @name AnalyzeUsingEastLogrankFormula
#' @title Compute the statistic using formulas Q.242 and Q.243 in the East manual.
#' @description Use the formulas Q.242 and Q.243 in the East manual to compute the statistic. The purpose of this
#'   example is to demonstrate how the analysis and decision making can be modified in a simple approach. The test
#'   statistic is compared to the lower boundary computed and sent by East Horizon as an input. This example does NOT
#'   include a futility rule.
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
#'
#' Example-specific additional output elements:
#' \describe{
#'   \item{HazardRatio}{Estimated treatment-to-control hazard ratio, retained as a custom output for reporting.}
#' }
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
    dTrueHR <- exp( cCoxModel$coefficients )

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
            # Equation Q.242 in East Manual
            dNum <- dNum + nEventsOnTreatment - nSubjectsAtRiskTreatment * nEvents / nSubjectsAtRisk
            # Generate dDen based on number of subjects at risk
            if ( nSubjectsAtRisk != 1 ) { # Equation Q.243 in East Manual
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
