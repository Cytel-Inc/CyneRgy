######################################################################################################################## .
#' @name AnalyzeNormalSSR
#' @title Analyze continuous subject responses
#' @description Implements continuous-outcome analysis with conditional power-based sample size re-estimation
#'   (SSR).
#' The function:
#' \enumerate{
#'   \item At and after the adaptation look, recomputes the interim test statistic from the completers at the
#'     adaptation look, computes conditional power under the current trend, and determines the re-estimated
#'     completers using a continuous or step-function SSR rule
#'   \item Determines the number of completers for the current look, using the modified look positions after an
#'     increase
#'   \item Computes the standardized test statistic from the completers at the current analysis time
#'   \item Generates a decision at the current look (efficacy, continue, or futility at final look)
#' }
#' @author Shubham Lahoti, J. Kyle Wathen, and Gabriel Potvin
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
#'   \item{TailType}{Integer. Nature of critical region: - `0`: Left-tailed. - `1`: Right-tailed. East Horizon
#'     Design: Only available if `Test Type = One-sided`.}
#'   \item{CriticalPoint}{Numeric. Critical value (for one-sided tests). East Horizon Explore: Only available if
#'     `Statistical Design = Fixed Sample`. East Horizon Design: Only available if `Test Type = One-sided` and
#'     `Statistical Design = Fixed Sample`.}
#'   \item{MaxCompleters}{Integer maximum number of completers in the trial.}
#'   \item{RespLag}{Numeric follow-up duration from enrollment to response measurement.}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumLooks}{Integer total number of analysis looks.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{InfoFrac}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the information fraction
#'     for each look. Used to compute the planned completers when `CumCompleters` is not available.}
#'   \item{CumCompleters}{Vector of Integer. Vector of length `LookInfo$NumLooks`, containing the planned
#'     cumulative number of completers for each look. These values are not updated after adaptation.}
#'   \item{RejType}{Integer one-sided rejection type supported by the decision helpers: 0 = upper efficacy;
#'     1 = upper futility; 2 = lower efficacy; 3 = lower futility; 4 = upper efficacy and lower futility;
#'     5 = lower efficacy and upper futility.}
#'   \item{EffBdry}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the efficacy boundary
#'     values (for one-sided tests) for each look. East Horizon Explore: Set to `NA` for `Endpoint Type =
#'     Continuous with Repeated Measures`. East Horizon Design: Only available if `Test Type = One-sided`.}
#' }
#' @param AdaptInfo Named list of sample size re-estimation parameters for this two-arm continuous design. Access elements by name, for example `AdaptInfo$SSRFuncScale`. Available only for
#'   designs with sample size re-estimation.
#' \describe{
#'   \item{AdaptInterimNum}{Integer. Interim analysis number (starting at 1) at which adaptation happens. Must
#'     be at least 1 and smaller than `LookInfo$NumLooks`.}
#'   \item{PromZoneMin}{Numeric. Minimum threshold for promising zone (inclusive).}
#'   \item{PromZoneMax}{Numeric. Maximum threshold for promising zone (exclusive).}
#'   \item{SSRFuncScale}{Numeric. Sample size re-estimation function scale for promising zone: - `0`: Continuous. -
#'     `1`: Step. - `2`: User-specified R. `SSRFuncScale = 1 (step)` is available only in East Horizon Explore.
#'     Any other value leaves the sample size unchanged.}
#'   \item{MaxSSMultInp}{Named List. Maximum sample size multiplier inputs. If `SSRFuncScale = 0 (continuous)`, it
#'     is a Named List of Numeric: - `MaxSSMultInp["From"] = PromZoneMin`. - `MaxSSMultInp["To"] = PromZoneMax`. -
#'     `MaxSSMultInp["MaxSSMult"]` is a numeric specified by the user. If `SSRFuncScale = 1 (step)`, it is a Named
#'     List of Array of Numeric: - `MaxSSMultInp["From"]` is an array of size `NumSteps` containing the lower
#'     threshold for each step. - `MaxSSMultInp["To"]` is an array of size `NumSteps` containing the upper
#'     threshold for each step. - `MaxSSMultInp["MaxSSMult"]` is an array of size `NumSteps` containing the maximum
#'     sample size multiplier for each step. Each step covers `From <= CP < To`.}
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
#'     simulations. This example returns `1` when no completers are available, `-1` when step-function inputs
#'     are missing, and `-2` when `AdaptInfo$AdaptInterimNum` is missing or invalid.}
#'   \item{ReEstCompleters}{Required integer re-estimated total number of completers for the sample size
#'     re-estimation design. Equal to the planned maximum completers before the adaptation look or when the
#'     sample size is not increased.}
#' }
#' @details This example applies one-sided efficacy boundaries on the Z scale. Extend its boundary logic
#'   before using p-value-scale boundaries or two-sided designs.
#'
#' @details
#' ## Sample Size Re-estimation Rule
#'
#' At the adaptation look `k = AdaptInfo$AdaptInterimNum`, with `n_k` planned completers out of `N` planned
#' completers, the information fraction is `tau = n_k / N`. Conditional power under the current trend is
#' `CP = 1 - pnorm( s * ( c - Z / sqrt( tau ) ) / sqrt( 1 - tau ) )`, where `Z` is the interim Wald statistic,
#' `c = LookInfo$EffBdry[ LookInfo$NumLooks ]` is the final efficacy boundary, and `s = 1` for a right-tailed
#' test or `s = -1` for a left-tailed test. This two-stage formula ignores intermediate looks.
#'
#' - Continuous (`SSRFuncScale = 0`): if `PromZoneMin <= CP < PromZoneMax`, the re-estimated completers are
#'   `floor( N * MaxSSMult )`. This custom rule jumps directly to the maximum multiplier and differs from the
#'   native East Horizon rule, which targets `TargetCP` up to the maximum.
#' - Step (`SSRFuncScale = 1`): the multiplier of the step with `From <= CP < To` is used. East Horizon Design
#'   passes `NA` for intermediate step cut-points when there is more than one step; this example then returns
#'   `ErrorCode = -1`.
#'
#' The re-estimated completers are never lower than `N`. East Horizon does not update `LookInfo` after adaptation,
#' so at each later look the function recomputes the interim decision on the same completers and moves the look
#' positions as the engine does: the final look uses the re-estimated completers and intermediate looks are
#' scaled proportionally. If fewer completers are available than targeted, the last available completer is used.
#'
#' The final test is the naive Wald statistic on all completers, not a Cui, Hung, and Wang (CHW) weighted
#' statistic, so the type I error should be checked by simulation under the null hypothesis. Dropout
#' information is ignored.
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

AnalyzeNormalSSR <- function( SimData, DesignParam, LookInfo = NULL, AdaptInfo = NULL, UserParam = NULL ) {
    nErrorCode <- 0
    nDecision <- 0
    dTestStatistic <- NA
    dDelta <- NA
    dAnalysisTime <- NA

    ###########################################################
    ## Step 1 - Planned Look Positions
    ###########################################################
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        if ( length( LookInfo$CumCompleters ) == nQtyOfLooks && !anyNA( LookInfo$CumCompleters ) ) {
            vPlannedCompleters <- LookInfo$CumCompleters
        } else {
            vPlannedCompleters <- round( LookInfo$InfoFrac * DesignParam$MaxCompleters )
        }
    } else {
        nQtyOfLooks <- 1
        nLookIndex <- 1
        vPlannedCompleters <- DesignParam$MaxCompleters
    }
    nPlannedMaxCompleters <- vPlannedCompleters[ nQtyOfLooks ]
    vLookCompleters <- vPlannedCompleters
    nReEstCompleters <- nPlannedMaxCompleters

    # Completers are ordered by the calendar time at which their response is observed
    SimData$CalendarResponseTime <- SimData$ArrivalTime + DesignParam$RespLag
    SimData <- SimData[ order( SimData$CalendarResponseTime ), ]

    ComputeStatistics <- function( dfCompleters ) {
        vCtrl <- dfCompleters$Response[ dfCompleters$TreatmentID == 0 ]
        vExp <- dfCompleters$Response[ dfCompleters$TreatmentID == 1 ]
        dEstDelta <- mean( vExp ) - mean( vCtrl )
        dEstSE <- sqrt( stats::var( vExp ) / length( vExp ) + stats::var( vCtrl ) / length( vCtrl ) )
        dZ <- NA
        if ( !is.na( dEstDelta ) && !is.na( dEstSE ) && dEstSE > 0 ) {
            dZ <- dEstDelta / dEstSE
        }
        return( list( Delta = dEstDelta, TestStat = dZ ) )
    }

    ###########################################################
    ## Step 2 - Sample Size Re-estimation At And After The Adaptation Look
    ###########################################################
    if ( !is.null( AdaptInfo ) && !is.null( LookInfo ) ) {
        nAdaptLook <- AdaptInfo$AdaptInterimNum
        if ( length( nAdaptLook ) != 1 || is.na( nAdaptLook ) || nAdaptLook < 1 || nAdaptLook >= nQtyOfLooks ) {
            return( list( ErrorCode = -2L, Decision = 0L, TestStat = NA_real_,
                ReEstCompleters = as.integer( nPlannedMaxCompleters ), Delta = NA_real_ ) )
        }

        if ( isTRUE( AdaptInfo$SSRFuncScale == 1 ) ) {
            vStepLowerBound <- AdaptInfo$MaxSSMultInp$From
            vStepUpperBound <- AdaptInfo$MaxSSMultInp$To
            vStepMultiplier <- AdaptInfo$MaxSSMultInp$MaxSSMult
            if ( length( vStepLowerBound ) == 0 || anyNA( vStepLowerBound ) || anyNA( vStepUpperBound ) ||
                anyNA( vStepMultiplier ) || length( vStepUpperBound ) != length( vStepLowerBound ) ||
                length( vStepMultiplier ) != length( vStepLowerBound ) ) {
                return( list( ErrorCode = -1L, Decision = 0L, TestStat = NA_real_,
                    ReEstCompleters = as.integer( nPlannedMaxCompleters ), Delta = NA_real_ ) )
            }
        }

        if ( nLookIndex >= nAdaptLook ) {
            # The interim data are unchanged after adaptation because East Horizon adds the new subjects after
            # the adaptation analysis time, so the interim decision can be recomputed at every later look.
            nAdaptCompleters <- vPlannedCompleters[ nAdaptLook ]
            if ( nrow( SimData ) >= nAdaptCompleters && nAdaptCompleters > 0 ) {
                dAdaptTime <- SimData$CalendarResponseTime[ nAdaptCompleters ]
                lAdaptStats <- ComputeStatistics( SimData[ SimData$CalendarResponseTime <= dAdaptTime, ] )
                dZCrit <- LookInfo$EffBdry[ nQtyOfLooks ]

                if ( !is.na( lAdaptStats$TestStat ) && length( dZCrit ) == 1 && !is.na( dZCrit ) ) {
                    # Conditional power under the current trend, using the two-stage approximation that ignores
                    # intermediate looks: CP = 1 - pnorm( s * ( c - Z / sqrt( tau ) ) / sqrt( 1 - tau ) )
                    dTau <- nAdaptCompleters / nPlannedMaxCompleters
                    dTailSign <- ifelse( DesignParam$TailType == 0, -1, 1 )
                    dCP <- 1 - stats::pnorm( dTailSign * ( dZCrit - lAdaptStats$TestStat / sqrt( dTau ) ) /
                        sqrt( 1 - dTau ) )

                    dMultiplier <- 1
                    if ( isTRUE( AdaptInfo$SSRFuncScale == 0 ) ) {
                        ### Continuous: jump to the maximum multiplier anywhere in the promising zone
                        if ( dCP >= AdaptInfo$PromZoneMin && dCP < AdaptInfo$PromZoneMax ) {
                            dMultiplier <- AdaptInfo$MaxSSMultInp$MaxSSMult
                        }
                    } else if ( isTRUE( AdaptInfo$SSRFuncScale == 1 ) ) {
                        ### Step Function
                        vIdx <- which( dCP >= vStepLowerBound & dCP < vStepUpperBound )
                        if ( length( vIdx ) > 0 ) {
                            dMultiplier <- vStepMultiplier[ vIdx[ 1 ] ]
                        }
                    }
                    nReEstCompleters <- max( nPlannedMaxCompleters,
                        floor( nPlannedMaxCompleters * dMultiplier + 1e-9 ) )
                }
            }

            # Post-adaptation look positions, computed as in the East Horizon engine
            if ( nReEstCompleters > nPlannedMaxCompleters ) {
                dRatio <- ( nReEstCompleters - nAdaptCompleters ) / ( nPlannedMaxCompleters - nAdaptCompleters )
                for ( nLook in seq.int( nAdaptLook + 1, length.out = nQtyOfLooks - nAdaptLook - 1 ) ) {
                    vLookCompleters[ nLook ] <- floor( nAdaptCompleters +
                        dRatio * ( vPlannedCompleters[ nLook ] - nAdaptCompleters ) + 0.5 )
                }
                vLookCompleters[ nQtyOfLooks ] <- nReEstCompleters
            }
        }
    }

    ###########################################################
    ## Step 3 - Analysis Time And Completers At The Current Look
    ###########################################################
    nQtyOfCompleters <- min( vLookCompleters[ nLookIndex ], nrow( SimData ) )
    if ( nQtyOfCompleters < 1 ) {
        return( list( ErrorCode = 1L, Decision = 0L, TestStat = NA_real_,
            ReEstCompleters = as.integer( nReEstCompleters ), Delta = NA_real_ ) )
    }
    dAnalysisTime <- SimData$CalendarResponseTime[ nQtyOfCompleters ]

    # Only completers are analyzed; subjects still awaiting their response are excluded
    dfSimDataCurrLook <- SimData[ SimData$CalendarResponseTime <= dAnalysisTime, ]

    ###########################################################
    ## Step 4 - Test Statistic And Delta Computation
    ###########################################################
    lStats <- ComputeStatistics( dfSimDataCurrLook )
    dDelta <- lStats$Delta
    dTestStatistic <- lStats$TestStat

    ###########################################################
    ## Step 5 - Decision Computation
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
    ## Step 6 - Return Output
    ###########################################################
    return( list(
        Decision         = as.integer( nDecision ),
        TestStat         = as.double( dTestStatistic ),
        ReEstCompleters  = as.integer( nReEstCompleters ),
        Delta            = as.double( dDelta ),
        AnalysisTime     = as.double( dAnalysisTime ),
        ErrorCode        = as.integer( nErrorCode )
    ) )
}
