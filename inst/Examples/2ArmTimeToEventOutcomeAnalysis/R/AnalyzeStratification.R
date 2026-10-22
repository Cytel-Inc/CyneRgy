######################################################################################################################## .
#' @name AnalyzeStratification
#' @title Analyze time-to-event subject outcomes
#' @description Computes a stratified log-rank test, hazard ratio, analysis time and decision at a given interim
#'   analysis.
#'
#' \enumerate{
#'   \item Prepares observed data up to the interim analysis time
#'   \item Computes the test statistic
#'   \item Computes the HR
#'   \item Generates a decision at the current look (efficacy, continue, or futility at final look)
#' }
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
#'
#' Subject-level stratification factor columns are named by DesignParam$TestStratFactors (Explore) or
#'   names(DesignParam$StratFactors) (Design), with each row containing the corresponding subject's factor levels.
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$TailType`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{TailType}{Integer. Nature of critical region: – `0`: Left-tailed. – `1`: Right-tailed. East Horizon
#'     Design: Only available if `Test Type = One-sided`.}
#'   \item{CriticalPoint}{Numeric. Critical value (for one-sided tests). East Horizon Explore: Only available if
#'     `Statistical Design = Fixed Sample`. East Horizon Design: Only available if `Test Type = One-sided` and
#'     `Statistical Design = Fixed Sample`.}
#'   \item{MaxEvents}{Integer maximum number of events in the trial.}
#'   \item{StratFactors}{Named List. Named list of length `NumStratFactors`, indicating stratification factors
#'     details. For example, `StratFactors["Factor1"]` is an array of level names for Factor 1. East Horizon
#'     Explore: Only available for `Endpoint Type = Time-to-Event` with `Stratification` turned on. East Horizon
#'     Design: Only available for `Time-to-Event` tests with `Stratification` turned on.}
#'   \item{TestStratFactors}{Array of Character. Array of factor names included in the analysis. Length is between
#'     1 and `NumStratFactors`. East Horizon Explore: Only available for `Endpoint Type = Time-to-Event` with
#'     `Stratification` turned on. East Horizon Design: Not available.}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumLooks}{Integer total number of analysis looks.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{CumEvents}{Integer cumulative number of events at the current look. These examples also accept a vector of
#'     per-look cumulative event counts, indexed by LookInfo$CurrLookIndex. East Horizon Explore: Only available for
#'     `Endpoint Type = Time-to-Event` and for Vaccine Efficacy (`Endpoint Type = Binary` with Lower Value and `Test =
#'     1 - Ratio of Proportions or 1 - Ratio of Poisson Rates`). East Horizon Design: Only available for `Time-to-
#'     Event` tests.}
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
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore).}
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale.}
#'   \item{HR}{Estimated treatment-to-control hazard ratio.}
#'   \item{AnalysisTime}{Optional numeric calendar time of the analysis: the look time at an interim analysis and
#'     the study duration at the final analysis. Compute and return this value in the R function.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details This example applies one-sided efficacy boundaries on the Z scale. Extend its boundary logic
#'   before using p-value-scale boundaries or two-sided designs.
#'
#'
#' SimData also contains the subject-level stratification-factor columns named in DesignParam$StratFactors.
#'   This function returns scalar results. Use AnalyzeSubpopulation for subpopulation-specific analyses.
######################################################################################################################## .

AnalyzeStratification <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    nErrorCode <- 0
    nDecision <- 0
    dTestStatistic <- 0
    dTimeOfAnalysis <- 0

    # Determine number of events for analysis
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfEvents <- LookInfo$CumEvents
        if ( length( nQtyOfEvents ) > 1 ) {
            nQtyOfEvents <- nQtyOfEvents[ nLookIndex ]
        }
    } else {
        nQtyOfLooks <- 1
        nLookIndex <- 1
        nQtyOfEvents <- DesignParam$MaxEvents
    }

    # Prepare analysis dataset
    SimData$TimeOfEvent <- SimData$ArrivalTime + SimData$SurvivalTime
    SimData <- SimData[ order( SimData$TimeOfEvent ), ]
    vEventTimes <- SimData$TimeOfEvent
    dTimeOfAnalysis <- vEventTimes[ nQtyOfEvents ]
    SimData <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ]
    SimData$Event <- as.integer( SimData$TimeOfEvent <= dTimeOfAnalysis )
    SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )

    vObservedTime <- SimData$ObservedTime
    vEvent <- SimData$Event
    vTreatmentID <- SimData$TreatmentID

    # Determine which stratification factors to use
    if ( !all( is.na( DesignParam$TestStratFactors ) ) ) {
        vStratFactors <- DesignParam$TestStratFactors
    } else {
        # For Design, as TestStratFactors is NA
        vStratFactors <- names( DesignParam$StratFactors )
    }

    # Convert each stratification column to a factor
    for ( strFactor in vStratFactors ) {
        SimData[[ strFactor ]] <- factor( SimData[[ strFactor ]],
            levels = unique( SimData[[ strFactor ]] )
        )
    }

    # Construct the formula for strata dynamically
    fStrataFormula <- stats::as.formula(
        paste0(
            "survival::Surv(vObservedTime, vEvent) ~ vTreatmentID + ",
            paste0( "survival::strata(`", vStratFactors, "`)", collapse = " + " )
        )
    )

    # Perform stratified log-rank test
    cSurvDiff <- survival::survdiff( fStrataFormula, data = SimData )
    dTestStatistic <- sqrt( cSurvDiff$chisq )

    # Compute Hazard Ratio (HR)
    cCoxFit <- survival::coxph( fStrataFormula, data = SimData )
    dHR <- exp( stats::coef( cCoxFit ) )

    dTestStatistic <- ifelse( unname( dHR ) < 1, dTestStatistic * -1, dTestStatistic )

    # Decision logic based on boundaries
    if ( !is.na( dTestStatistic ) ) {
        dEffBdry <- DesignParam$CriticalPoint
        if ( !is.null( LookInfo ) ) {
            dEffBdry <- LookInfo$EffBdry[ nLookIndex ]
        }

        bEfficacyCondition <- FALSE
        if ( !is.null( dEffBdry ) && !is.na( dEffBdry ) ) {
            if ( DesignParam$TailType == 0 ) {
                bEfficacyCondition <- dTestStatistic < dEffBdry
            } else {
                bEfficacyCondition <- dTestStatistic > dEffBdry
            }
        }
        strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
            bIAEfficacyCondition = bEfficacyCondition,
            bFAEfficacyCondition = bEfficacyCondition
        )
        nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    }

    lRet <- list(
        TestStat = as.double( dTestStatistic ),
        AnalysisTime = as.double( dTimeOfAnalysis ),
        HR = as.double( dHR ),
        Decision = as.integer( nDecision ),
        ErrorCode = as.integer( nErrorCode )
    )
    return( lRet )
}
