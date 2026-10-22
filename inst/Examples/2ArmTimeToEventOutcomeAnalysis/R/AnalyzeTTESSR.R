######################################################################################################################## .
#' @name AnalyzeTTESSR
#' @title Analyze time-to-event subject outcomes
#' @description Implements time-to-event (TTE) analysis with conditional power–based event re-estimation (SSR).
#' The function:
#' \enumerate{
#'   \item Prepares observed data up to the current analysis time
#'   \item Computes the log-rank–style test statistic and hazard ratio (HR)
#'   \item Computes conditional power using the design boundary
#'   \item Determines re-estimated events using a continuous or step-function SSR rule
#'   \item Generates a decision at the current look (efficacy, continue, or futility at final look)
#' }
#'
#' This function assumes proportional hazards and uses the Cox model for hazard ratio estimation.
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
#'   \item{EffBdry}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the efficacy boundary
#'     values (for one-sided tests) for each look. East Horizon Explore: Set to `NA` for `Endpoint Type =
#'     Continuous with Repeated Measures`. East Horizon Design: Only available if `Test Type = One-sided`.}
#' }
#' @param AdaptInfo Named list of sample size re-estimation parameters for this two-arm time-to-event design. Access elements by name, for example `AdaptInfo$SSRFuncScale`. Available only for
#'   designs with sample size re-estimation.
#' \describe{
#'   \item{PromZoneMin}{Numeric. Minimum threshold for promising zone.}
#'   \item{PromZoneMax}{Numeric. Maximum threshold for promising zone.}
#'   \item{SSRFuncScale}{Numeric. Sample size re-estimation function scale for promising zone: - `0`: Continuous. -
#'     `1`: Step. - `2`: User-specified R. `SSRFuncScale = 1 (step)` is available only in East Horizon Explore.}
#'   \item{MaxSSMultInp}{Named List. Maximum sample size multiplier inputs. If `SSRFuncScale = 0 (continuous)`, it
#'     is a Named List of Numeric: - `MaxSSMultInp["From"] = PromZoneMin`. - `MaxSSMultInp["To"] = PromZoneMax`. -
#'     `MaxSSMultInp["MaxSSMult"]` is a numeric specified by the user. If `SSRFuncScale = 1 (step)`, it is a Named
#'     List of Array of Numeric: - `MaxSSMultInp["From"]` is an array of size `NumSteps` containing the lower
#'     threshold for each step. - `MaxSSMultInp["To"]` is an array of size `NumSteps` containing the upper
#'     threshold for each step. - `MaxSSMultInp["MaxSSMult"]` is an array of size `NumSteps` containing the maximum
#'     sample size multiplier for each step.}
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
#'   \item{ReEstEvents}{Required integer re-estimated total event count for the sample size re-estimation design.}
#' }
#' @details This example applies one-sided efficacy boundaries on the Z scale. Extend its boundary logic
#'   before using p-value-scale boundaries or two-sided designs.
#'
######################################################################################################################## .

AnalyzeTTESSR <- function( SimData, DesignParam, LookInfo = NULL, AdaptInfo = NULL, UserParam = NULL ) {
    nErrorCode <- 0
    nDecision <- 0
    dTestStatistic <- 0
    dAnalysisTime <- 0

    ###########################################################
    ## Step 1 — Data Preparation and Analysis Time Computation
    ###########################################################

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

    SimData$TimeOfEvent <- SimData$ArrivalTime + SimData$SurvivalTime
    SimData <- SimData[ order( SimData$TimeOfEvent ), ]
    vEventTimes <- SimData$TimeOfEvent
    dAnalysisTime <- vEventTimes[ nQtyOfEvents ]

    SimData <- SimData[ SimData$ArrivalTime <= dAnalysisTime, ]
    SimData$Event <- as.integer( SimData$TimeOfEvent <= dAnalysisTime )
    SimData$ObservedTime <- ifelse(
        SimData$TimeOfEvent > dAnalysisTime,
        dAnalysisTime - SimData$ArrivalTime,
        SimData$TimeOfEvent - SimData$ArrivalTime
    )

    SimData <- SimData[ order( SimData$ObservedTime ), ]
    dfSimDataCurrLook <- subset( SimData, SimData$ArrivalTime <= dAnalysisTime + 1e-4 )

    ###########################################################
    ## Step 2 — Test Statistic And HR Computation
    ###########################################################

    cLogrank <- survival::survdiff(
        survival::Surv( ObservedTime, Event ) ~ TreatmentID,
        data = dfSimDataCurrLook
    )
    cCoxModel <- survival::coxph(
        survival::Surv( ObservedTime, Event ) ~ TreatmentID,
        data = dfSimDataCurrLook
    )
    dHR <- exp( stats::coef( cCoxModel ) )
    dTestStatistic <- sqrt( cLogrank$chisq )
    if ( dHR < 1 ) {
        dTestStatistic <- -dTestStatistic
    }

    ###########################################################
    ## Step 3 — Conditional Power Computation
    ###########################################################
    dOrigCp <- NA
    dZCrit <- DesignParam$CriticalPoint
    dTau <- 1
    nReEstEvents <- DesignParam$MaxEvents

    if ( !is.na( dTestStatistic ) ) {
        if ( !is.null( LookInfo ) && !is.null( LookInfo$EffBdry ) ) {
            dZCrit <- LookInfo$EffBdry[ nQtyOfLooks ]
        }
        if ( !is.null( LookInfo ) ) {
            dTau <- LookInfo$InfoFrac[ nLookIndex ]
        }
        dTailSign <- 1
        if ( DesignParam$TailType == 0 ) {
            dTailSign <- -1
        }
        dOrigCp <- 1 - stats::pnorm( ( dTailSign * ( dZCrit - dTestStatistic * sqrt( dTau ) ) ) / sqrt( 1 - dTau + 1e-12 ) )
    }

    ###########################################################
    ## Step 4 — Re-estimated Events Computation
    ###########################################################
    if ( !is.null( AdaptInfo ) && AdaptInfo$SSRFuncScale == 0 ) {
        if ( is.na( dOrigCp ) ) {
            nReEstEvents <- DesignParam$MaxEvents
        } else if ( dOrigCp > AdaptInfo$PromZoneMin && dOrigCp < AdaptInfo$PromZoneMax ) {
            nReEstEvents <- DesignParam$MaxEvents * AdaptInfo$MaxSSMultInp$MaxSSMult
        } else {
            nReEstEvents <- DesignParam$MaxEvents
        }
    } else if ( !is.null( AdaptInfo ) && AdaptInfo$SSRFuncScale == 1 ) {
        if ( is.na( dOrigCp ) ) {
            nReEstEvents <- DesignParam$MaxEvents
        } else {
            vStepLowerBound <- AdaptInfo$MaxSSMultInp$From
            vStepUpperBound <- AdaptInfo$MaxSSMultInp$To
            vStepMultiplier <- AdaptInfo$MaxSSMultInp$MaxSSMult
            nIdx <- which( dOrigCp > vStepLowerBound & dOrigCp <= vStepUpperBound )
            if ( length( nIdx ) == 0 ) {
                nReEstEvents <- DesignParam$MaxEvents
            } else {
                nReEstEvents <- DesignParam$MaxEvents * vStepMultiplier[ nIdx ]
            }
        }
    }

    ###########################################################
    ## Step 5 — Decision Computation
    ###########################################################
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

    ###########################################################
    ## Step 6 — Return Output
    ###########################################################
    return( list(
        Decision       = as.integer( nDecision ),
        TestStat       = as.double( dTestStatistic ),
        ReEstEvents    = as.integer( nReEstEvents ),
        HR             = as.double( dHR ),
        AnalysisTime   = as.double( dAnalysisTime ),
        ErrorCode      = as.integer( nErrorCode )
    ) )
}
