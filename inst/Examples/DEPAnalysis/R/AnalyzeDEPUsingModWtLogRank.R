######################################################################################################################## .
#' @name AnalyzeDEPUsingModWtLogRank
#'
#' @title Compute the modestly weighted log rank test statistic.
#'
#' @description Compute modestly weighted log rank test statistic given simulation data.
#'
#' @author Gabriel Potvin, Anoop Singh Rawat, Pradip Maske
#'
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the native fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{Response1}{Numeric vector of subject responses for the first endpoint. Survival times are measured from
#'     enrollment.}
#'   \item{Response2}{Numeric vector of subject responses for the second endpoint. Survival times are measured from
#'     enrollment.}
#'   \item{ClndrRespTime}{Numeric vector of response times on the calendar scale, with one element per subject. For
#'     a survival endpoint, this is ArrivalTime plus the event or censoring time measured from enrollment. Applies
#'     to the first endpoint.}
#'   \item{ClndrRespTime2}{Numeric vector of response times on the calendar scale, with one element per subject.
#'     For a survival endpoint, this is ArrivalTime plus the event or censoring time measured from enrollment.
#'     Applies to the second endpoint.}
#'   \item{CensorIndOrg}{Original integer vector of censor indicators before any analysis-time adjustment: 0 =
#'     dropout/non-completer; 1 = completer. Applies to the first endpoint.}
#'   \item{CensorIndOrg2}{Original integer vector of censor indicators before any analysis-time adjustment: 0 =
#'     dropout/non-completer; 1 = completer. Applies to the second endpoint.}
#'   \item{DropOutTime}{Numeric vector of generated dropout times measured from each subject's enrollment, with one
#'     element per subject. Inf indicates no dropout.}
#' }
#'
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below.
#' \describe{
#'   \item{EndpointType}{Vector of Integer. Vector of length equal to the number of endpoints, indicating the
#'     endpoint types: – 0: Continuous. – 1: Binary. – 2: Time-to-Event.}
#'   \item{EndpointName}{Vector of String. Vector of length equal to the number of endpoints, containing the
#'     endpoint names.}
#'   \item{WinCond}{Integer. Winning condition: - `1`: At least endpoint 1. - `2`: At least endpoint 2. - `3`: At
#'     least one endpoint. - `4`: Both endpoints.}
#'   \item{TailType}{Named List of Integer. Named List of length equal to the number of endpoints, indicating the
#'     nature of critical region for each endpoint. For example, `TailType["Endpoint 1"]` is the nature for
#'     Endpoint 1. Possible values: – `0`: Left-tailed. – `1`: Right-tailed.}
#'   \item{FollowUpType}{Named List of Integer. Named List of length equal to the number of endpoints, indicating
#'     the follow-up type for each endpoint. For example, `FollowUpType["Endpoint 1"]` is the type for Endpoint 1.
#'     Possible values: – `0`: Until the end of the study. – `1`: For a fixed period.}
#'   \item{FollowUpDur}{Named List of Numeric. Named List of length equal to the number of endpoints, indicating
#'     the follow-up duration for each endpoint. For example, `FollowUpDur["Endpoint 1"]` is the duration for
#'     Endpoint 1.}
#'   \item{TrialType}{Named List of Integer. Named List of length equal to the number of endpoints, indicating the
#'     trial type for each endpoint. For example, `TrialType["Endpoint 1"]` is the type for Endpoint 1. Possible
#'     values: – `0`: Superiority. – `1`: Non-inferiority.}
#'   \item{VarType}{Integer. Variance type: - `0`: Pooled. - `1`: Unpooled. Only available for `Dual Endpoint =
#'     TTE-Binary`.}
#'   \item{PlanEndTrial}{Integer. Planned end of trial: - `1`: Full information for both endpoints. - `2`: Full
#'     information for endpoint 1. - `3`: Full information for endpoint 2.}
#'   \item{AllocInfo}{Vector of Numeric. Vector of length equal to the number of treatment arms (number of arms -
#'     1), containing the ratios of the treatment group sample sizes to control group sample size.}
#'   \item{Alpha}{Numeric. Type I Error.}
#'   \item{CriticalPoint}{Named List of Numeric. Named List of length equal to the number of endpoints, indicating
#'     the critical value for each endpoint. For example, `CriticalPoint["Endpoint 1"]` is the value for Endpoint
#'     1. Only available if `Statistical Design = Fixed Sample`.}
#'   \item{LowerCriticalPoint}{Named List of Numeric. Named List of length equal to the number of endpoints,
#'     indicating the lower critical value for each endpoint. For example, `LowerCriticalPoint["Endpoint 1"]` is
#'     the value for Endpoint 1. Only available if `Statistical Design = Fixed Sample` and `Tail Type = Left
#'     Tailed`.}
#'   \item{UpperCriticalPoint}{Named List of Numeric. Named List of length equal to the number of endpoints,
#'     indicating the upper critical value for each endpoint. For example, `UpperCriticalPoint["Endpoint 1"]` is
#'     the value for Endpoint 1. Only available if `Statistical Design = Fixed Sample` and `Tail Type = Right
#'     Tailed`.}
#'   \item{SampleSize}{Integer. Sample size of the trial.}
#'   \item{MultAdj}{Integer. Multiplicity adjustment method: - `0`: None. - `1`: Fallback. - `2`: Fixed sequence. -
#'     `3`: Weighted Bonferroni. - `4`: Weighted Bonferroni-Holms. - `5`: Weighted Hochberg. Only available if the
#'     multiplicity adjustment method is not custom.}
#'   \item{TestOrder}{Integer. Testing order: - `1`: Start with endpoint 1. - `2`: Start with endpoint 2. Only
#'     available if `Multiplicity Adjustment = 1 (Fallback) or 2 (Fixed Sequence)`.}
#'   \item{MaxEvents}{Named List of Integer. Named List of length equal to the number of endpoints, indicating the
#'     maximum events for each endpoint. For example, `MaxEvents["Endpoint 1"]` is the maximum events for Endpoint
#'     1. Set to `NA` for endpoints with `Endpoint Type = Binary`.}
#'   \item{MaxCompleters}{Named List of Integer. Named List of length equal to the number of endpoints, indicating
#'     the maximum number of completers for each endpoint. For example, `MaxCompleters["Endpoint 1"]` is the
#'     maximum number of completers for Endpoint 1. Not available for `Dual Endpoint = TTE-TTE`. Set to `NA` for
#'     endpoints with `Endpoint Type = Time-to-Event`.}
#'   \item{TrtEffNull}{Named List of Numeric. Named List of length equal to the number of endpoints, indicating the
#'     treatment effect under the null hypothesis for each endpoint. For example, `TrtEffNull["Endpoint 1"]` is the
#'     treatment effect for Endpoint 1. Specified in natural log scale for endpoints with `Endpoint Type =
#'     Time-to-Event`.}
#'   \item{AlphaAlloc}{Named List of Numeric. Named List of length equal to the number of endpoints, indicating the
#'     type I error allocation (\%) for each endpoint. For example, `AlphaAlloc["Endpoint 1"]` is the error
#'     allocation for Endpoint 1. Not available for `Multiplicity Adjustment = 0 (None) or 2 (Fixed Sequence)`.}
#'   \item{TargetSSFA}{Named List of Numeric. Named List of length equal to the number of endpoints, indicating the
#'     target sample size (final analysis) for each endpoint. For example, `TargetSSFA["Endpoint 1"]` is the target
#'     sample size for Endpoint 1. Not available for `Dual Endpoint = TTE-TTE`. Set to `NA` for endpoints with
#'     `Endpoint Type = Time-to-Event`.}
#'   \item{TestStat}{Named List of Numeric. Named List of length equal to the number of endpoints, indicating the
#'     test statistic output for each endpoint. For example, `TestStat["Endpoint 1"]` is the test statistic output
#'     for Endpoint 1. Set to `NA` for endpoints with a pending analysis. For example, if "Endpoint 1" is tested
#'     first, then for the analysis of "Endpoint 1" we will have `TestStat["Endpoint 2"] = NA`.}
#'   \item{TestID}{Integer test identifier supplied by East Horizon for the selected test.}
#' }
#'
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumEndpointLooks}{Named List of Integer. Named List of length equal to the number of endpoints,
#'     indicating the number of looks for each endpoint. For example, `NumEndpointLooks["Endpoint 1"]` is the
#'     number of looks for Endpoint 1.}
#'   \item{NumLooks}{Integer. Number of looks, defined as the maximum across all endpoints.}
#'   \item{CurrLookIndex}{Integer. Current index look, starting from 1.}
#'   \item{SyncInterim}{Integer. Interim synchronization option: - `1`: Based on endpoint 1. - `2`: Based on
#'     endpoint 2.}
#'   \item{InputInfoFrac}{Named List of Vector of Numeric. Named List of length equal to the number of endpoints,
#'     containing the information fraction vector for each endpoint. For example, `InputInfoFrac["Endpoint 1"]` is
#'     a vector of length `LookInfo$NumLooks` containing the information fraction for each look for Endpoint 1.
#'     East Horizon Explore: For "Endpoint 1", some initial entries will be `NA` if `SyncInterim = 2`. Some later
#'     entries will be `NA` if `SyncInterim = 1` and `NumEndpointLooks["Endpoint 2"] > NumEndpointLooks["Endpoint
#'     1"]`.}
#'   \item{CumCompleters}{Named List of Vector of Integer. Named List of length equal to the number of endpoints,
#'     containing the cumulative number of completers vector for each endpoint. For example,
#'     `CumCompleters["Endpoint 1"]` is a vector of length `LookInfo$NumLooks` containing the cumulative number of
#'     completers for each look for Endpoint 1. East Horizon Explore: Not available for `Dual Endpoint = TTE-TTE`.
#'     Set to `NA` for endpoints with `Endpoint Type = Time-to-Event`.}
#'   \item{CumEvents}{Named List of Vector of Integer. Named List of length equal to the number of endpoints,
#'     containing the cumulative events vector for each endpoint. For example, `CumEvents["Endpoint 1"]` is a
#'     vector of length `LookInfo$NumLooks` containing the cumulative number of events for each look for Endpoint
#'     1. East Horizon Explore: Set to `NA` for endpoints with `Endpoint Type = Binary`.}
#'   \item{RejType}{Named List of Integer. Named List of length equal to the number of endpoints, containing the
#'     rejection type for each endpoint. For example, `RejType["Endpoint 1"]` is the rejection type for Endpoint 1.
#'     Possible values: – `0`: One-sided efficacy upper. – `1`: One-sided futility upper. – `2`: One-sided efficacy
#'     lower. – `3`: One-sided futility lower. – `4`: One-sided efficacy upper, futility lower. – `5`: One-sided
#'     efficacy lower, futility upper.}
#'   \item{EffBdryScale}{Named List of Integer. Named List of length equal to the number of endpoints, containing
#'     the efficacy boundary scale for each endpoint. For example, `EffBdryScale["Endpoint 1"]` is the efficacy
#'     boundary scale for Endpoint 1. Possible values: – `0`: Z scale.}
#'   \item{EffBdry}{Named List of Vector of Numeric. Named List of length equal to the number of endpoints,
#'     containing the efficacy boundary values for each endpoint. For example, `EffBdry["Endpoint 1"]` is a vector
#'     of length `LookInfo$NumLooks` containing the efficacy boundary values for each look for Endpoint 1. East
#'     Horizon Explore: Set to `NA` if efficacy boundaries are skipped for some looks.}
#'   \item{EffBdryLower}{Named list of numeric vectors. Named List of length equal to the number of endpoints,
#'     containing the lower efficacy boundary values for each endpoint. For example, `EffBdryLower["Endpoint 1"]`
#'     is a vector of length `LookInfo$NumLooks` containing the lower efficacy boundary values for each look for
#'     Endpoint 1. East Horizon Explore: Only available if `Tail Type = Left-tailed`. Two-sided tests do not exist,
#'     so this variable is not useful: use EffBdry instead. Set to `NA` if efficacy boundaries are skipped for some
#'     looks.}
#'   \item{EffBdryUpper}{Named list of numeric vectors. Named List of length equal to the number of endpoints,
#'     containing the upper efficacy boundary values for each endpoint. For example, `EffBdryUpper["Endpoint 1"]`
#'     is a vector of length `LookInfo$NumLooks` containing the upper efficacy boundary values for each look for
#'     Endpoint 1. East Horizon Explore: Only available if `Tail Type = Right-tailed`. Two-sided tests do not
#'     exist, so this variable is not useful: use EffBdry instead. Set to `NA` if efficacy boundaries are skipped
#'     for some looks.}
#'   \item{FutBdryScale}{Named List of Integer. Named List of length equal to the number of endpoints, containing
#'     the futility boundary scale for each endpoint. For example, `FutBdryScale["Endpoint 1"]` is the futility
#'     boundary scale for Endpoint 1. Possible values: – `0`: Z scale. – `2`: Delta scale - `6`: Hazard ratio
#'     scale.}
#'   \item{FutBdry}{Named List of Vector of Numeric. Named List of length equal to the number of endpoints,
#'     containing the futility boundary values for each endpoint. For example, `FutBdry["Endpoint 1"]` is a vector
#'     of length `LookInfo$NumLooks` containing the futility boundary values for each look for Endpoint 1. East
#'     Horizon Explore: Set to `NA` if futility boundaries are skipped for some looks.}
#'   \item{FutBdryLower}{Named list of numeric vectors. Named List of length equal to the number of endpoints,
#'     containing the lower futility boundary values for each endpoint. For example, `FutBdryLower["Endpoint 1"]`
#'     is a vector of length `LookInfo$NumLooks` containing the lower futility boundary values for each look for
#'     Endpoint 1. East Horizon Explore: Only available if `Tail Type = Left-tailed`. Two-sided tests do not exist,
#'     so this variable is not useful: use FutBdry instead. Set to `NA` if futility boundaries are skipped for some
#'     looks.}
#'   \item{FutBdryUpper}{Named list of numeric vectors. Named List of length equal to the number of endpoints,
#'     containing the upper futility boundary values for each endpoint. For example, `FutBdryUpper["Endpoint 1"]`
#'     is a vector of length `LookInfo$NumLooks` containing the upper futility boundary values for each look for
#'     Endpoint 1. East Horizon Explore: Only available if `Tail Type = Right-tailed`. Two-sided tests do not
#'     exist, so this variable is not useful: use FutBdry instead. Set to `NA` if futility boundaries are skipped
#'     for some looks.}
#'   \item{BindingType}{Named List of Integer. Named List of length equal to the number of endpoints, containing
#'     the binding type for each endpoint. For example, `BindingType["Endpoint 1"]` is the binding type for
#'     Endpoint 1. Possible values: - `0`: Non-binding. - `1`: Binding.}
#' }
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale for the endpoint currently being analyzed.}
#'   \item{HR}{Estimated treatment-to-control hazard ratio for the current survival endpoint. Required when the
#'     function analyzes a time-to-event endpoint.}
#'   \item{Delta}{Estimated experimental-minus-control proportion difference for the current binary endpoint.
#'     Required when the function analyzes a binary endpoint.}
#'   \item{AnalysisTime}{Optional numeric calendar time of the analysis: the look time at an interim analysis and
#'     the study duration at the final analysis. Compute and return this value in the R function.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' @details The current code assumes there are no dropouts. Modify the code accordingly for dropout case.
#'
#' The dual-endpoint engine exposes Response1 and Response2 and the endpoint-specific calendar times and original
#'   censor indicators listed in SimData. Each endpoint-specific design/list value is indexed by its actual
#'   EndpointName.
######################################################################################################################## .

AnalyzeDEPUsingModWtLogRank <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    # The endpoint ID and name of the endpoint to be analysed.
    # This needs to be manually specified as of now
    nAnalysisEndpointIndex <- 1
    strAnalysisEndpointName <- DesignParam$EndpointName[ nAnalysisEndpointIndex ]

    # Set delay parameter as 0 if not specified by the user
    if ( is.null( UserParam[[ strAnalysisEndpointName ]]$delay ) ) {
        UserParam[[ strAnalysisEndpointName ]]$delay <- 0
    }

    dAnalysisTime <- ComputeDEPAnalysisTime( SimData, DesignParam, LookInfo )
    dfAnalysisData <- SimData[ SimData$ArrivalTime <= dAnalysisTime, ] # Slicing the data to be used for analysis

    # Compute the Observed Time variable for the analysis
    if ( nAnalysisEndpointIndex == 1 ) {
        dfAnalysisData$Event <- dfAnalysisData$CensorIndOrg * ( dfAnalysisData$ClndrRespTime < dAnalysisTime )
        dfAnalysisData$ObservedTime <- pmin(
            dAnalysisTime - dfAnalysisData$ArrivalTime,
            dfAnalysisData$ClndrRespTime - dfAnalysisData$ArrivalTime
        )
    } else {
        dfAnalysisData$Event <- dfAnalysisData$CensorIndOrg2 * ( dfAnalysisData$ClndrRespTime2 < dAnalysisTime )
        dfAnalysisData$ObservedTime <- pmin(
            dAnalysisTime - dfAnalysisData$ArrivalTime,
            dfAnalysisData$ClndrRespTime2 - dfAnalysisData$ArrivalTime
        )
    }

    # Order the data by observed time
    dfAnalysisData <- dfAnalysisData[ order( dfAnalysisData$ObservedTime ), ]

    # Compute Observed HR
    coxModel <- survival::coxph( survival::Surv( ObservedTime, Event ) ~ TreatmentID, data = dfAnalysisData )
    dTrueHR <- exp( coxModel$coefficients )

    dfAnalysisData$EventOnTreatment <- ifelse( dfAnalysisData$TreatmentID == 1, dfAnalysisData$Event, 0 )
    dfAnalysisData$EventOnControl <- ifelse( dfAnalysisData$TreatmentID == 0, dfAnalysisData$Event, 0 )

    # Subjects at risk at baseline
    nSubjectsAtRiskTreatment <- nrow( dfAnalysisData[ dfAnalysisData$TreatmentID == 1, ] )
    nSubjectsAtRiskControl <- nrow( dfAnalysisData[ dfAnalysisData$TreatmentID == 0, ] )

    # Initialize numerator and denominator
    dNum <- 0
    dDen <- 0
    weight <- 1
    # Iterate over subjects
    for ( nSubject in 1:nrow( dfAnalysisData ) ) {
        # Non-event: update risk set
        if ( dfAnalysisData$Event[ nSubject ] == 0 ) {
            if ( dfAnalysisData$TreatmentID[ nSubject ] == 1 ) {
                nSubjectsAtRiskTreatment <- nSubjectsAtRiskTreatment - 1
            }
            if ( dfAnalysisData$TreatmentID[ nSubject ] == 0 ) {
                nSubjectsAtRiskControl <- nSubjectsAtRiskControl - 1
            }
        }

        # Event: update dNum and dDen
        if ( dfAnalysisData$Event[ nSubject ] == 1 ) {
            nEventsOnTreatment <- dfAnalysisData$EventOnTreatment[ nSubject ]
            nEventsOnControl <- dfAnalysisData$EventOnControl[ nSubject ]
            nEvents <- nEventsOnTreatment + nEventsOnControl
            nSubjectsAtRisk <- nSubjectsAtRiskTreatment + nSubjectsAtRiskControl

            # Weight for modestly weighted log-rank test
            weight <- ifelse( dfAnalysisData$ObservedTime[ nSubject ] <= UserParam[[ DesignParam$EndpointName[[ nAnalysisEndpointIndex ]] ]]$delay,
                weight * 1 / ( 1 - nEvents / nSubjectsAtRisk ), weight
            )

            dNum <- dNum + weight * ( nEventsOnTreatment - nSubjectsAtRiskTreatment * nEvents / nSubjectsAtRisk )

            if ( nSubjectsAtRisk != 1 ) {
                dDen <- dDen + weight^2 * (
                    nSubjectsAtRiskTreatment * nSubjectsAtRiskControl *
                        ( nSubjectsAtRisk - nEvents ) * nEvents /
                        ( ( nSubjectsAtRisk - 1 ) * nSubjectsAtRisk^2 )
                )
            }

            # Update risk set for next iteration
            nSubjectsAtRiskTreatment <- nSubjectsAtRiskTreatment - nEventsOnTreatment
            nSubjectsAtRiskControl <- nSubjectsAtRiskControl - nEventsOnControl
        }
    }

    # Compute test statistic
    dTS <- dNum / sqrt( dDen )
    nErrorCode <- 0

    return( list(
        TestStat  = as.double( dTS ),
        HR        = as.double( dTrueHR ),
        ErrorCode = as.integer( nErrorCode )
    ) )
}

# ComputeDEPAnalysisTime() : Function to compute the analysis time for DEP.
ComputeDEPAnalysisTime <- function( SimData, DesignParam, LookInfo = NULL ) {
    bGSD <- ifelse( is.null( LookInfo ), FALSE, TRUE ) # Is the trial using Group sequential Design?

    if ( bGSD ) { # Group Sequential Design
        nSyncEndpointIndex <- LookInfo$SyncInterim # Endpoint ID for the endpoint to be used for look positioning
        nSyncEndpointType <- DesignParam$EndpointType[[ nSyncEndpointIndex ]] # Endpoint type of the endpoint used for look positioning
        nOtherEndpointIndex <- ifelse( LookInfo$SyncInterim == 1, 2, 1 ) # Endpoint ID for endpoint not being used for look positioning
        nOtherEndpointType <- DesignParam$EndpointType[[ nOtherEndpointIndex ]] # Endpoint type of the endpoint not being used for look positioning

        # Look info was provided so use it
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex

        # CumTargets will be planned cumulative events/completers for the Endpoint used for the current look positioning.
        if ( nLookIndex <= LookInfo$NumEndpointLooks[ nSyncEndpointIndex ] ) {
            if ( nSyncEndpointType == 2 ) {
                CumTargets <- LookInfo$CumEvents[[ DesignParam$EndpointName[ nSyncEndpointIndex ] ]]
            } else {
                CumTargets <- LookInfo$CumCompleters[[ DesignParam$EndpointName[ nSyncEndpointIndex ] ]]
            }
        } else {
            if ( nOtherEndpointType == 2 ) {
                CumTargets <- LookInfo$CumEvents[[ DesignParam$EndpointName[ nOtherEndpointIndex ] ]]
            } else {
                CumTargets <- LookInfo$CumCompleters[[ DesignParam$EndpointName[ nOtherEndpointIndex ] ]]
            }
        }

        nQtyOfTargets <- CumTargets[ nLookIndex ]

        EPIDforSlicingData <- ifelse( nLookIndex <= LookInfo$NumEndpointLooks[ nSyncEndpointIndex ], nSyncEndpointIndex, nOtherEndpointIndex )
        if ( EPIDforSlicingData == 1 ) {
            SimDataAnlys <- SimData[ order( SimData$ClndrRespTime, SimData$CensorIndOrg ), ]
            idxAnlys <- which( cumsum( SimDataAnlys$CensorIndOrg ) >= nQtyOfTargets )
            dAnalysisTime <- ifelse( length( idxAnlys ) > 0,
                SimDataAnlys$ClndrRespTime[ min( idxAnlys ) ],
                SimDataAnlys$ClndrRespTime[ DesignParam$SampleSize ]
            )
        } else {
            SimDataAnlys <- SimData[ order( SimData$ClndrRespTime2, SimData$CensorIndOrg2 ), ]
            idxAnlys <- which( cumsum( SimDataAnlys$CensorIndOrg2 ) >= nQtyOfTargets )
            dAnalysisTime <- ifelse( length( idxAnlys ) > 0,
                SimDataAnlys$ClndrRespTime2[ min( idxAnlys ) ],
                SimDataAnlys$ClndrRespTime2[ DesignParam$SampleSize ]
            )
        }
    } else { # FSD design
        nQtyOfLooks <- 1
        nLookIndex <- 1

        # nQtyOfTargets will be planned events/completers for the Endpoint on which end of the trial is defined.
        if ( DesignParam$PlanEndTrial == 2 || DesignParam$PlanEndTrial == 1 ) { # Full info on Endpoint 1 or Both Endpoints
            nQtyOfTargets <- ifelse( DesignParam$EndpointType[ 1 ] == 1,
                DesignParam$MaxCompleters[[ DesignParam$EndpointName[ 1 ] ]],
                DesignParam$MaxEvents[[ DesignParam$EndpointName[ 1 ] ]]
            )
            SimDataEP1 <- SimData[ order( SimData$ClndrRespTime, SimData$CensorIndOrg ), ]
            idxEP1 <- which( cumsum( SimDataEP1$CensorIndOrg ) >= nQtyOfTargets )
            AnalysisTimeEP1 <- ifelse( length( idxEP1 ) > 0,
                SimDataEP1$ClndrRespTime[ min( idxEP1 ) ],
                SimDataEP1$ClndrRespTime[ DesignParam$SampleSize ]
            )
        }
        if ( DesignParam$PlanEndTrial == 3 || DesignParam$PlanEndTrial == 1 ) { # Full info on Endpoint 2 or Both Endpoints
            nQtyOfTargets <- ifelse( DesignParam$EndpointType[ 2 ] == 1,
                DesignParam$MaxCompleters[[ DesignParam$EndpointName[ 2 ] ]],
                DesignParam$MaxEvents[[ DesignParam$EndpointName[ 2 ] ]]
            )
            SimDataEP2 <- SimData[ order( SimData$ClndrRespTime2, SimData$CensorIndOrg2 ), ]
            idxEP2 <- which( cumsum( SimDataEP2$CensorIndOrg2 ) >= nQtyOfTargets )
            AnalysisTimeEP2 <- ifelse( length( idxEP2 ) > 0,
                SimDataEP2$ClndrRespTime2[ min( idxEP2 ) ],
                SimDataEP2$ClndrRespTime2[ DesignParam$SampleSize ]
            )
        }

        dAnalysisTime <- ifelse( DesignParam$PlanEndTrial == 1, max( AnalysisTimeEP1, AnalysisTimeEP2 ),
            ifelse( DesignParam$PlanEndTrial == 2, AnalysisTimeEP1, AnalysisTimeEP2 )
        )
    }
    return( dAnalysisTime )
}
