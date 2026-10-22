######################################################################################################################## .
#' @name AnalyzePFSAndOS
#' @title Analyze Progression-Free Survival (PFS) and Overall Survival (OS) Data Using Probability of Success (PoS)
#' @description Analyze progression-free survival (PFS) and overall survival (OS) in a two-arm survival design. Use
#'   the PFS efficacy boundary together with the user-specified OS hazard ratio cutoff to assess probability of
#'   success at interim and final analyses.
#' @author Gabriel Potvin, Valeria A. G. Mazzanti, J. Kyle Wathen
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
#' Additional fields returned by Simulate2EndpointTTEWithMultiState:
#' \describe{
#'   \item{OS}{Numeric vector of subject overall-survival times measured from enrollment, with one element
#'     per subject.}
#' }
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{TailType}{Integer. Nature of critical region: – `0`: Left-tailed. – `1`: Right-tailed. East Horizon
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
#' Relevant elements include:
#'                  \describe{
#'   \item{HazardRatioCutoffIA}{OS hazard ratio threshold for interim analysis.}
#'   \item{HazardRatioCutoffFA}{OS hazard ratio threshold for final analysis.}
#'                  }
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
#'   \item{nPFSEfficacy}{Indicator for PFS efficacy decision:
#'                              \describe{
#'                                \item{0}{No PFS efficacy decision.}
#'                                \item{1}{PFS efficacy decision made.}
#'                              }}
#'   \item{nOSEfficacy}{Indicator for OS efficacy decision:
#'                              \describe{
#'                                \item{0}{No OS efficacy decision.}
#'                                \item{1}{OS efficacy decision made.}
#'                              }}
#'   \item{dPValuePFS}{P-value for progression-free survival endpoint.}
#'   \item{dZValPFS}{Z-value for progression-free survival endpoint.}
#'   \item{dPValueOS}{P-value for overall survival endpoint.}
#'   \item{dHazardRatioPFS}{Hazard ratio for progression-free survival endpoint.}
#'   \item{HazardRatio}{Custom numeric hazard ratio for progression-free survival, equal to dHazardRatioPFS.}
#'   \item{dHazardRatioOS}{Hazard ratio for overall survival endpoint.}
#'   \item{dEffBdry}{Efficacy boundary value for the current look.}
#'   \item{HazardRatioCutoffIA}{OS hazard ratio threshold for interim analysis.}
#'   \item{HazardRatioCutoffFA}{OS hazard ratio threshold for final analysis.}
#' }
#'
######################################################################################################################## .

AnalyzePFSAndOS <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        vCumEvents <- LookInfo$InfoFrac * DesignParam$MaxEvents
        nQtyOfEvents <- vCumEvents[ nLookIndex ]
        dEffBdry <- LookInfo$EffBdryLower[ nLookIndex ]
    } else {
        nQtyOfLooks <- 1
        nLookIndex <- 1
        nQtyOfEvents <- DesignParam$MaxEvents
        dEffBdry <- DesignParam$CriticalPoint
    }

    # Build the dataset
    SimData$TimeOfPFSEvent <- SimData$ArrivalTime + SimData$SurvivalTime
    SimData$TimeOfOSEvent <- SimData$ArrivalTime + SimData$OS
    SimData <- SimData[ order( SimData$TimeOfPFSEvent ), ]
    vEventTimes <- SimData$TimeOfPFSEvent
    dTimeOfAnalysis <- vEventTimes[ nQtyOfEvents ]
    SimData <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ]

    # Set the PFS event and observed times
    SimData$Event <- as.integer( SimData$TimeOfPFSEvent <= dTimeOfAnalysis )
    SimData$ObservedTime <- pmin( SimData$TimeOfPFSEvent, dTimeOfAnalysis ) - SimData$ArrivalTime

    # Set the OS event and observed times
    SimData$OSEvent <- as.integer( SimData$TimeOfOSEvent <= dTimeOfAnalysis )
    SimData$ObservedTimeOS <- pmin( SimData$TimeOfOSEvent, dTimeOfAnalysis ) - SimData$ArrivalTime

    # Analyze the PFS data
    cCoxModel <- survival::coxph( survival::Surv( ObservedTime, Event ) ~ as.factor( TreatmentID ), data = SimData )
    dPValuePFS <- summary( cCoxModel )$coefficients[ , "Pr(>|z|)" ]
    dZValPFS <- summary( cCoxModel )$coefficients[ , "z" ]
    dHazardRatioPFS <- exp( stats::coef( cCoxModel ) )

    # Analyze the OS data
    cOSCoxModel <- survival::coxph( survival::Surv( ObservedTimeOS, OSEvent ) ~ as.factor( TreatmentID ), data = SimData )
    dPValueOS <- summary( cOSCoxModel )$coefficients[ , "Pr(>|z|)" ]
    dHazardRatioOS <- exp( stats::coef( cOSCoxModel ) )
    dZValOS <- summary( cOSCoxModel )$coefficients[ , "z" ]

    nPFSEfficacy <- 0 # 0 if NOT an efficacy decision for PFS, 1 if efficacy decision for PFS
    nOSEfficacy <- 0 # 0 if NOT an efficacy decision for OS, 1 if efficacy decision for OS
    if ( nLookIndex < nQtyOfLooks ) { # Interim Analysis
        if ( dZValPFS <= dEffBdry ) {
            nPFSEfficacy <- 1
        }
        if ( dHazardRatioOS < UserParam$HazardRatioCutoffIA ) {
            nOSEfficacy <- 1
        }

        if ( dZValPFS <= dEffBdry && dHazardRatioOS < UserParam$HazardRatioCutoffIA ) {
            strDecision <- "Efficacy"
        } else {
            strDecision <- "Continue"
        }
    } else { # Final Analysis

        if ( dZValPFS <= dEffBdry ) {
            nPFSEfficacy <- 1
        }
        if ( dHazardRatioOS < UserParam$HazardRatioCutoffFA ) {
            nOSEfficacy <- 1
        }

        if ( dZValPFS <= dEffBdry && dHazardRatioOS < UserParam$HazardRatioCutoffFA ) {
            strDecision <- "Efficacy"
        } else {
            strDecision <- "Futility"
        }
    }

    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    lRet <- list(
        TestStat = as.double( dZValPFS ),
        Decision = as.integer( nDecision ),
        nPFSEfficacy = as.integer( nPFSEfficacy ),
        nOSEfficacy = as.integer( nOSEfficacy ),
        dPValuePFS = as.double( dPValuePFS ),
        dZValPFS = as.double( dZValPFS ),
        dPValueOS = as.double( dPValueOS ),
        dHazardRatioPFS = as.double( dHazardRatioPFS ),
        dHazardRatioOS = as.double( dHazardRatioOS ),
        dEffBdry = as.double( dEffBdry ),
        HazardRatioCutoffIA = as.double( UserParam$HazardRatioCutoffIA ),
        HazardRatioCutoffFA = as.double( UserParam$HazardRatioCutoffFA ),
        ErrorCode = as.integer( 0 ),
        HazardRatio = as.double( dHazardRatioPFS )
    )

    return( lRet )
}
