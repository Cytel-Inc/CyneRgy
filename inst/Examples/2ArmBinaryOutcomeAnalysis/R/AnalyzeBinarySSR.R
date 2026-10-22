######################################################################################################################## .
#' @name AnalyzeBinarySSR
#' @title Analyze binary subject responses
#' @description Implements binary-outcome analysis with conditional power–based sample size re-estimation (SSR).
#' The function:
#' \enumerate{
#'   \item Prepares observed data up to the interim analysis time
#'   \item Computes the standardized test statistic
#'   \item Computes conditional power using the design boundary
#'   \item Determines re-estimated completers using a continuous or step-function SSR rule
#'   \item Generates a decision at the current look (efficacy, continue, or futility at final look)
#' }
#' @author J. Kyle Wathen and Gabriel Potvin
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
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
#'   \item{MaxCompleters}{Integer maximum number of completers in the trial.}
#'   \item{RespLag}{Numeric follow-up duration from enrollment to response measurement.}
#' }
#' @param AdaptInfo Named list of sample size re-estimation parameters for this two-arm binary design. Access elements by name, for example `AdaptInfo$SSRFuncScale`. Available only for
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
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore).}
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale.}
#'   \item{Delta}{Estimated experimental-minus-control treatment effect (proportion difference for binary outcomes;
#'     mean difference for continuous outcomes).}
#'   \item{AnalysisTime}{Optional numeric calendar time of the analysis: the look time at an interim analysis and
#'     the study duration at the final analysis. Compute and return this value in the R function.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#'   \item{ReEstCompleters}{Required integer re-estimated total number of completers for the sample size
#'     re-estimation design.}
#' }
#' @details This example applies one-sided efficacy boundaries on the Z scale. Extend its boundary logic
#'   before using p-value-scale boundaries or two-sided designs.
#'
######################################################################################################################## .

AnalyzeBinarySSR <- function( SimData, DesignParam, AdaptInfo = NULL, LookInfo = NULL, UserParam = NULL ) {
    nErrorCode <- 0
    nDecision <- 0
    dTestStatistic <- NA
    dAnalysisTime <- NA

    ###########################################################
    ## Step 1 — Data Preparation and Analysis Time Computation
    ###########################################################

    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        vCumCompleters <- LookInfo$InfoFrac * DesignParam$MaxCompleters
        nQtyOfCompleters <- vCumCompleters[ nLookIndex ]
    } else {
        nQtyOfLooks <- 1
        nLookIndex <- 1
        nQtyOfCompleters <- DesignParam$MaxCompleters
    }

    SimData$CalendarResponseTime <- SimData$ArrivalTime + DesignParam$RespLag
    SimData <- SimData[ order( SimData$CalendarResponseTime ), ]
    vCompleterTimes <- SimData$CalendarResponseTime
    dAnalysisTime <- vCompleterTimes[ nQtyOfCompleters ]
    dfSimDataCurrLook <- SimData[ SimData$CalendarResponseTime <= dAnalysisTime, ]

    ###########################################################
    ## Step 2 — Test Statistic And Delta Computation
    ###########################################################

    vPatientOutcome <- dfSimDataCurrLook$Response
    vPatientTreatment <- dfSimDataCurrLook$TreatmentID

    vOutcomesCtrl <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesExp <- vPatientOutcome[ vPatientTreatment == 1 ]

    nCtrl <- length( vOutcomesCtrl )
    nExp <- length( vOutcomesExp )
    nCtrlResp <- sum( vOutcomesCtrl )
    nExpResp <- sum( vOutcomesExp )

    dCtrlPi <- ifelse( nCtrl > 0, nCtrlResp / nCtrl, NA )
    dExpPi <- ifelse( nExp > 0, nExpResp / nExp, NA )
    dDelta <- dExpPi - dCtrlPi

    nTotal <- nCtrl + nExp
    nTotalResp <- nCtrlResp + nExpResp
    dPooledPi <- ifelse( nTotal > 0, nTotalResp / nTotal, NA )

    dSE <- sqrt( dPooledPi * ( 1 - dPooledPi ) * ( 1 / nCtrl + 1 / nExp ) )

    if ( !is.na( dSE ) && dSE > 0 && !is.na( dDelta ) ) {
        dTestStatistic <- dDelta / dSE
    } else {
        dTestStatistic <- NA
        nErrorCode <- 1
    }

    ###########################################################
    ## Step 3 — Conditional Power Computation
    ###########################################################

    dOrigCp <- NA
    dZCrit <- DesignParam$CriticalPoint
    dTau <- 1
    nReEstCompleters <- DesignParam$MaxCompleters

    if ( !is.na( dTestStatistic ) ) {
        # Z-critical
        if ( !is.null( LookInfo ) && !is.null( LookInfo$EffBdry ) ) {
            dZCrit <- LookInfo$EffBdry[ nQtyOfLooks ]
        }

        # Information fraction
        if ( !is.null( LookInfo ) ) {
            dTau <- LookInfo$InfoFrac[ nLookIndex ]
        }

        # Conditional power
        dTailSign <- 1
        if ( DesignParam$TailType == 0 ) {
            dTailSign <- -1
        }
        dOrigCp <- 1 - stats::pnorm( ( dTailSign * ( dZCrit - dTestStatistic * sqrt( dTau ) ) ) /
            sqrt( 1 - dTau + 1e-12 ) )
    }

    ###########################################################
    ## Step 4 — Re-estimated Completers Computation
    ###########################################################

    if ( !is.null( AdaptInfo ) && AdaptInfo$SSRFuncScale == 0 ) {
        if ( is.na( dOrigCp ) ) {
            nReEstCompleters <- DesignParam$MaxCompleters
        } else if ( dOrigCp > AdaptInfo$PromZoneMin &&
            dOrigCp < AdaptInfo$PromZoneMax ) {
            nReEstCompleters <- DesignParam$MaxCompleters *
                AdaptInfo$MaxSSMultInp$MaxSSMult
        } else {
            nReEstCompleters <- DesignParam$MaxCompleters
        }
    } else if ( !is.null( AdaptInfo ) && AdaptInfo$SSRFuncScale == 1 ) {
        if ( is.na( dOrigCp ) ) {
            nReEstCompleters <- DesignParam$MaxCompleters
        } else {
            vStepLowerBound <- AdaptInfo$MaxSSMultInp$From
            vStepUpperBound <- AdaptInfo$MaxSSMultInp$To
            vStepMultiplier <- AdaptInfo$MaxSSMultInp$MaxSSMult

            nIdx <- which( dOrigCp > vStepLowerBound &
                dOrigCp <= vStepUpperBound )

            if ( length( nIdx ) == 0 ) {
                nReEstCompleters <- DesignParam$MaxCompleters
            } else {
                nReEstCompleters <- DesignParam$MaxCompleters *
                    vStepMultiplier[ nIdx ]
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
        Decision        = as.integer( nDecision ),
        TestStat        = as.double( dTestStatistic ),
        ReEstCompleters = as.integer( nReEstCompleters ),
        Delta           = as.double( dDelta ),
        AnalysisTime    = as.double( dAnalysisTime ),
        ErrorCode       = as.integer( nErrorCode )
    ) )
}
