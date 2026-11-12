######################################################################################################################## .
#' @name AnalyzeDEPUsingModWtLogRank
#' @title Compute the modestly weighted log rank test statistic.
#' @description Compute modestly weighted log rank test statistic given simulation data.
#' @author Gabriel Potvin, Anoop Singh Rawat, Pradip Maske
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
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
#' }
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{EndpointType}{Vector of Integer. Vector of length equal to the number of endpoints, indicating the
#'     endpoint types: - 0: Continuous. - 1: Binary. - 2: Time-to-Event.}
#'   \item{EndpointName}{Vector of String. Vector of length equal to the number of endpoints, containing the
#'     endpoint names.}
#'   \item{PlanEndTrial}{Integer. Planned end of trial: - `1`: Full information for both endpoints. - `2`: Full
#'     information for endpoint 1. - `3`: Full information for endpoint 2.}
#'   \item{SampleSize}{Integer planned total sample size of the trial.}
#'   \item{MaxEvents}{Named List of Integer. Named List of length equal to the number of endpoints, indicating the
#'     maximum events for each endpoint. For example, `MaxEvents["Endpoint 1"]` is the maximum events for Endpoint
#'     1. Set to `NA` for endpoints with `Endpoint Type = Binary`.}
#'   \item{MaxCompleters}{Named List of Integer. Named List of length equal to the number of endpoints, indicating
#'     the maximum number of completers for each endpoint. For example, `MaxCompleters["Endpoint 1"]` is the
#'     maximum number of completers for Endpoint 1. Not available for `Dual Endpoint = TTE-TTE`. Set to `NA` for
#'     endpoints with `Endpoint Type = Time-to-Event`.}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumEndpointLooks}{Named List of Integer. Named List of length equal to the number of endpoints,
#'     indicating the number of looks for each endpoint. For example, `NumEndpointLooks["Endpoint 1"]` is the
#'     number of looks for Endpoint 1.}
#'   \item{NumLooks}{Integer. Number of looks, defined as the maximum across all endpoints.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{SyncInterim}{Integer. Interim synchronization option: - `1`: Based on endpoint 1. - `2`: Based on
#'     endpoint 2.}
#'   \item{CumCompleters}{Named list of cumulative completer-count vectors, indexed by EndpointName.
#'     Each vector contains one count per look. Set to NA for time-to-event endpoints.}
#'   \item{CumEvents}{Named List of Vector of Integer. Named List of length equal to the number of endpoints,
#'     containing the cumulative events vector for each endpoint. For example, `CumEvents["Endpoint 1"]` is a vector of
#'     length `LookInfo$NumLooks` containing the cumulative number of events for each look for Endpoint 1. East Horizon
#'     Explore: Set to `NA` for endpoints with `Endpoint Type = Binary`.}
#' }
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale for the endpoint currently being analyzed.}
#'   \item{HR}{Estimated treatment-to-control hazard ratio for the current survival endpoint. Required when the
#'     function analyzes a time-to-event endpoint.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
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
        dfAnalysisData$Event <- dfAnalysisData$CensorIndOrg * ( dfAnalysisData$ClndrRespTime <= dAnalysisTime )
        dfAnalysisData$ObservedTime <- pmin(
            dAnalysisTime - dfAnalysisData$ArrivalTime,
            dfAnalysisData$ClndrRespTime - dfAnalysisData$ArrivalTime
        )
    } else {
        dfAnalysisData$Event <- dfAnalysisData$CensorIndOrg2 * ( dfAnalysisData$ClndrRespTime2 <= dAnalysisTime )
        dfAnalysisData$ObservedTime <- pmin(
            dAnalysisTime - dfAnalysisData$ArrivalTime,
            dfAnalysisData$ClndrRespTime2 - dfAnalysisData$ArrivalTime
        )
    }

    # Order the data by observed time
    dfAnalysisData <- dfAnalysisData[ order( dfAnalysisData$ObservedTime ), ]

    # Compute Observed HR
    cCoxModel <- survival::coxph( survival::Surv( ObservedTime, Event ) ~ TreatmentID, data = dfAnalysisData )
    dTrueHR <- exp( cCoxModel$coefficients )

    dfAnalysisData$EventOnTreatment <- ifelse( dfAnalysisData$TreatmentID == 1, dfAnalysisData$Event, 0 )
    dfAnalysisData$EventOnControl <- ifelse( dfAnalysisData$TreatmentID == 0, dfAnalysisData$Event, 0 )

    # Subjects at risk at baseline
    nSubjectsAtRiskTreatment <- nrow( dfAnalysisData[ dfAnalysisData$TreatmentID == 1, ] )
    nSubjectsAtRiskControl <- nrow( dfAnalysisData[ dfAnalysisData$TreatmentID == 0, ] )

    # Initialize numerator and denominator
    dNum <- 0
    dDen <- 0
    dWeight <- 1
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
            dWeight <- ifelse( dfAnalysisData$ObservedTime[ nSubject ] <= UserParam[[ DesignParam$EndpointName[[ nAnalysisEndpointIndex ]] ]]$delay,
                dWeight * 1 / ( 1 - nEvents / nSubjectsAtRisk ), dWeight
            )

            dNum <- dNum + dWeight * ( nEventsOnTreatment - nSubjectsAtRiskTreatment * nEvents / nSubjectsAtRisk )

            if ( nSubjectsAtRisk != 1 ) {
                dDen <- dDen + dWeight^2 * (
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

        # vCumTargets will be planned cumulative events/completers for the Endpoint used for the current look positioning.
        if ( nLookIndex <= LookInfo$NumEndpointLooks[ nSyncEndpointIndex ] ) {
            if ( nSyncEndpointType == 2 ) {
                vCumTargets <- LookInfo$CumEvents[[ DesignParam$EndpointName[ nSyncEndpointIndex ] ]]
            } else {
                vCumTargets <- LookInfo$CumCompleters[[ DesignParam$EndpointName[ nSyncEndpointIndex ] ]]
            }
        } else {
            if ( nOtherEndpointType == 2 ) {
                vCumTargets <- LookInfo$CumEvents[[ DesignParam$EndpointName[ nOtherEndpointIndex ] ]]
            } else {
                vCumTargets <- LookInfo$CumCompleters[[ DesignParam$EndpointName[ nOtherEndpointIndex ] ]]
            }
        }

        nQtyOfTargets <- vCumTargets[ nLookIndex ]

        nEndpointIDForSlicingData <- ifelse( nLookIndex <= LookInfo$NumEndpointLooks[ nSyncEndpointIndex ], nSyncEndpointIndex, nOtherEndpointIndex )
        if ( nEndpointIDForSlicingData == 1 ) {
            dfSimDataAnalysis <- SimData[ order( SimData$ClndrRespTime, SimData$CensorIndOrg ), ]
            vAnalysisIndices <- which( cumsum( dfSimDataAnalysis$CensorIndOrg ) >= nQtyOfTargets )
            dAnalysisTime <- ifelse( length( vAnalysisIndices ) > 0,
                dfSimDataAnalysis$ClndrRespTime[ min( vAnalysisIndices ) ],
                dfSimDataAnalysis$ClndrRespTime[ DesignParam$SampleSize ]
            )
        } else {
            dfSimDataAnalysis <- SimData[ order( SimData$ClndrRespTime2, SimData$CensorIndOrg2 ), ]
            vAnalysisIndices <- which( cumsum( dfSimDataAnalysis$CensorIndOrg2 ) >= nQtyOfTargets )
            dAnalysisTime <- ifelse( length( vAnalysisIndices ) > 0,
                dfSimDataAnalysis$ClndrRespTime2[ min( vAnalysisIndices ) ],
                dfSimDataAnalysis$ClndrRespTime2[ DesignParam$SampleSize ]
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
            dfSimDataEndpoint1 <- SimData[ order( SimData$ClndrRespTime, SimData$CensorIndOrg ), ]
            vEndpoint1Indices <- which( cumsum( dfSimDataEndpoint1$CensorIndOrg ) >= nQtyOfTargets )
            dEndpoint1AnalysisTime <- ifelse( length( vEndpoint1Indices ) > 0,
                dfSimDataEndpoint1$ClndrRespTime[ min( vEndpoint1Indices ) ],
                dfSimDataEndpoint1$ClndrRespTime[ DesignParam$SampleSize ]
            )
        }
        if ( DesignParam$PlanEndTrial == 3 || DesignParam$PlanEndTrial == 1 ) { # Full info on Endpoint 2 or Both Endpoints
            nQtyOfTargets <- ifelse( DesignParam$EndpointType[ 2 ] == 1,
                DesignParam$MaxCompleters[[ DesignParam$EndpointName[ 2 ] ]],
                DesignParam$MaxEvents[[ DesignParam$EndpointName[ 2 ] ]]
            )
            dfSimDataEndpoint2 <- SimData[ order( SimData$ClndrRespTime2, SimData$CensorIndOrg2 ), ]
            vEndpoint2Indices <- which( cumsum( dfSimDataEndpoint2$CensorIndOrg2 ) >= nQtyOfTargets )
            dEndpoint2AnalysisTime <- ifelse( length( vEndpoint2Indices ) > 0,
                dfSimDataEndpoint2$ClndrRespTime2[ min( vEndpoint2Indices ) ],
                dfSimDataEndpoint2$ClndrRespTime2[ DesignParam$SampleSize ]
            )
        }

        dAnalysisTime <- ifelse( DesignParam$PlanEndTrial == 1, max( dEndpoint1AnalysisTime, dEndpoint2AnalysisTime ),
            ifelse( DesignParam$PlanEndTrial == 2, dEndpoint1AnalysisTime, dEndpoint2AnalysisTime )
        )
    }
    return( dAnalysisTime )
}
