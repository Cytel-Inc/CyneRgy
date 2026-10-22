######################################################################################################################## .
#' @name AnalyzeTTEWithConditionalPowerFutility
#' @title Time-To-Event Weighted Conditional Power Futility Analysis
#' @description This function performs a time-to-event (TTE) analysis with conditional power and futility
#'   boundaries, extending East Horizon: Design functionality via a custom R script for the Analysis integration
#'   point. The analysis is based on the logrank test and supports three modes of computing conditional power:
#'   using a target hazard ratio, using the estimated hazard ratio, or using a weighted combination of the two.
#' @author Gabriel Potvin, Valeria A. G. Mazzanti, Sheetal Solanki
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
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{TailType}{Integer. Nature of critical region: – `0`: Left-tailed. – `1`: Right-tailed. East Horizon
#'     Design: Only available if `Test Type = One-sided`.}
#'   \item{AllocInfo}{Vector of Numeric. Vector of length equal to the number of experimental arms (number of arms -
#'     1), containing the ratios of the experimental group sample sizes to the control group sample size.}
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
#'   \item{nComputationOption}{Specifies method for conditional power:
#'       1 = Target hazard ratio (using UserParam$TargetHazardRatio),
#'       2 = Estimated hazard ratio,
#'       3 = Weighted hazard ratio (must supply UserParam$WeightEstimatedHR, UserParam$WeightTargetHR and
#'         UserParam$TargetHazardRatio).}
#'   \item{FutilityThreshold}{Threshold below which futility is declared.}
#'   \item{TargetHazardRatio}{User-specified hazard ratio (used in options 1 and 3).}
#'   \item{WeightEstimatedHR}{Weight assigned to the estimated hazard ratio (used in option 3).}
#'   \item{WeightTargetHR}{Weight assigned to the target hazard ratio (used in option 3).}
#' }
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore).}
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale.}
#'   \item{HR}{Estimated treatment-to-control hazard ratio.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' Example-specific additional output elements:
#' \describe{
#'   \item{dConditionalPower}{Numeric. The conditional power at the current analysis (–1 if not computed).}
#' }
#' @details This example implements a one-sided left-tailed efficacy test on the Z scale and checks conditional
#'   power futility at interim looks. Conditional power targets the final efficacy boundary supplied by East
#'   Horizon, or the nominal Alpha boundary when no explicit boundary is supplied.
#'
######################################################################################################################## .

AnalyzeTTEWithConditionalPowerFutility <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    nErrorCode <- 0
    nDecision <- 0
    dTestStatistic <- 0
    nLookIndex <- 1
    dEstimatedHR <- 1

    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        vCumEvents <- LookInfo$InfoFrac * DesignParam$MaxEvents
        nQtyOfEvents <- vCumEvents[ nLookIndex ]
    } else {
        nQtyOfLooks <- 1
        nLookIndex <- 1
        nQtyOfEvents <- DesignParam$MaxEvents
    }

    # Prepare event and censoring times from simulated data
    SimData$TimeOfEvent <- SimData$ArrivalTime + SimData$SurvivalTime
    SimData <- SimData[ order( SimData$TimeOfEvent ), ]
    vEventTimes <- SimData$TimeOfEvent
    dTimeOfAnalysis <- vEventTimes[ nQtyOfEvents ]
    SimData <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ]
    SimData$Event <- as.integer( SimData$TimeOfEvent <= dTimeOfAnalysis )
    SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )

    # Perform Logrank test
    cLogrankTest <- survival::survdiff( survival::Surv( ObservedTime, Event ) ~ TreatmentID, data = SimData )
    dTestStatistic <- sqrt( cLogrankTest$chisq ) * sign( cLogrankTest$obs[ 2 ] - cLogrankTest$exp[ 2 ] )
    dPValue <- 1 - stats::pchisq( cLogrankTest$chisq, df = 1 )
    dAllocationFraction <- DesignParam$AllocInfo / ( 1 + DesignParam$AllocInfo )
    dSEHR <- 1 / sqrt( nQtyOfEvents * dAllocationFraction * ( 1 - dAllocationFraction ) )
    dEstimatedHR <- exp( dTestStatistic * dSEHR )

    # Retrieve user-defined parameters
    dTargetHR <- UserParam$TargetHazardRatio
    dFutilityThreshold <- UserParam$FutilityThreshold



    # Conditional power targets the final left-tail efficacy boundary.
    dEffBdry <- stats::qnorm( DesignParam$Alpha )
    if ( !is.null( LookInfo ) && !is.null( LookInfo$EffBdry ) ) {
        dEffBdry <- LookInfo$EffBdry[ nQtyOfLooks ]
    } else if ( !is.null( DesignParam$CriticalPoint ) ) {
        dEffBdry <- DesignParam$CriticalPoint
    }

    nDecision <- 0 # Set a default value
    if ( nLookIndex < nQtyOfLooks ) {
        if ( UserParam$nComputationOption == 1 ) {
            # Option 1: Compute CP using HR* = UserParam$TargetHazardRatio

            dAllocationProportion <- DesignParam$AllocInfo / ( 1 + DesignParam$AllocInfo )
            dSEHR <- 1 / sqrt( vCumEvents[ nLookIndex ] * dAllocationProportion * ( 1 - dAllocationProportion ) )
            dConditionalPower <- stats::pnorm(
                dEffBdry * sqrt( 1 + vCumEvents[ nLookIndex ] / ( DesignParam$MaxEvents - vCumEvents[ nLookIndex ] ) ) -
                    dTestStatistic * sqrt( vCumEvents[ nLookIndex ] / ( DesignParam$MaxEvents - vCumEvents[ nLookIndex ] ) ) -
                    log( dTargetHR ) * sqrt( dAllocationProportion * ( 1 - dAllocationProportion ) ) * sqrt( DesignParam$MaxEvents - vCumEvents[ nLookIndex ] )
            )
        } else if ( UserParam$nComputationOption == 2 ) {
            # Option 2: Compute CP using HR* = observed HR

            dAllocationProportion <- DesignParam$AllocInfo / ( 1 + DesignParam$AllocInfo )
            dSEHR <- 1 / sqrt( vCumEvents[ nLookIndex ] * dAllocationProportion * ( 1 - dAllocationProportion ) )
            dEstimatedHR <- exp( dTestStatistic * dSEHR )
            dConditionalPower <- stats::pnorm(
                dEffBdry * sqrt( 1 + vCumEvents[ nLookIndex ] / ( DesignParam$MaxEvents - vCumEvents[ nLookIndex ] ) ) -
                    dTestStatistic * sqrt( vCumEvents[ nLookIndex ] / ( DesignParam$MaxEvents - vCumEvents[ nLookIndex ] ) ) -
                    log( dEstimatedHR ) * sqrt( dAllocationProportion * ( 1 - dAllocationProportion ) ) * sqrt( DesignParam$MaxEvents - vCumEvents[ nLookIndex ] )
            )
        } else if ( UserParam$nComputationOption == 3 ) {
            dWeightEstimated <- UserParam$WeightEstimatedHR
            dWeightTarget <- UserParam$WeightTargetHR

            # Option 3: Compute CP using HR* = weighted combination

            dAllocationProportion <- DesignParam$AllocInfo / ( 1 + DesignParam$AllocInfo )
            dSEHR <- 1 / sqrt( vCumEvents[ nLookIndex ] * dAllocationProportion * ( 1 - dAllocationProportion ) )
            dEstimatedHR <- exp( dTestStatistic * dSEHR )
            dWeightedHR <- ( dWeightEstimated * dEstimatedHR ) + ( dWeightTarget * dTargetHR )
            dConditionalPower <- stats::pnorm(
                dEffBdry * sqrt( 1 + vCumEvents[ nLookIndex ] / ( DesignParam$MaxEvents - vCumEvents[ nLookIndex ] ) ) -
                    dTestStatistic * sqrt( vCumEvents[ nLookIndex ] / ( DesignParam$MaxEvents - vCumEvents[ nLookIndex ] ) ) -
                    log( dWeightedHR ) * sqrt( dAllocationProportion * ( 1 - dAllocationProportion ) ) * sqrt( DesignParam$MaxEvents - vCumEvents[ nLookIndex ] )
            )
        } else {
            stop( "Invalid computation option specified in UserParam$nComputationOption" )
        }



        # Make futility decision based on conditional power
        if ( dConditionalPower < dFutilityThreshold ) {
            nDecision <- 3 # Futility boundary crossed
        }
    } else if ( nLookIndex == nQtyOfLooks ) {
        # Check efficacy
        dConditionalPower <- -1
        if ( dTestStatistic <= dEffBdry ) {
            # Efficacy boundary crossed
            strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                bIAEfficacyCondition = FALSE, bIAFutilityCondition = FALSE,
                bFAEfficacyCondition = TRUE, bFAFutilityCondition = FALSE
            )
        } else {
            # Final analysis: if efficacy not achieved, declare futility
            strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                bIAEfficacyCondition = FALSE, bIAFutilityCondition = FALSE,
                bFAEfficacyCondition = FALSE, bFAFutilityCondition = TRUE
            )
        }
        nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    }

    # Return test statistic, hazard ratio, conditional power, and decision
    lRet <- list(
        TestStat = as.double( dTestStatistic ),
        HR = as.double( dEstimatedHR ),
        Decision = as.integer( nDecision ),
        ErrorCode = as.integer( nErrorCode ),
        dConditionalPower = as.double( dConditionalPower )
    )
    return( lRet )
}
