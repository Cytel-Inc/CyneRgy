######################################################################################################################## .
#' @name Analyze.RepeatedMeasures
#' @title Analyze repeated-measures subject responses
#' @description Analyze repeated-measures subject responses. Use the documented inputs and outputs to integrate
#'   this function with the simulation workflow.
#' @author Gabriel Potvin and Anoop Singh Rawat
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$TreatmentID`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{Response1, ..., ResponseNumVisit}{Numeric response vectors, one per visit, with one element per subject.
#'     Replace NumVisit by the actual number of visits.}
#'   \item{CensorInd1, ..., CensorIndNumVisit}{Integer censor-indicator vectors, one per visit: 0 =
#'     dropout/non-completer; 1 = completer.}
#'   \item{DropoutVisitID}{Integer vector of 1-based visit IDs after which subjects drop out, with one element per
#'     subject.}
#'   \item{ArrTimeVisit[VisitID]}{Numeric vector of visit times measured from each subject's enrollment,
#'     with one element per subject. Replace VisitID by the actual visit number. Add ArrivalTime to obtain
#'     calendar visit times.}
#' }
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$TailType`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{TailType}{Integer. Nature of critical region: - `0`: Left-tailed. - `1`: Right-tailed. East Horizon
#'     Design: Only available if `Test Type = One-sided`.}
#'   \item{NumVisit}{Integer number of visits. East Horizon Explore: Only available for `Endpoint Type =
#'     Continuous with Repeated Measures`. East Horizon Design: Not available.}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumLooks}{Integer total number of analysis looks.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{InfoFrac}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the information fraction
#'     for each look.}
#'   \item{CumCompleters}{Integer vector of cumulative completer counts, one per look.}
#'   \item{RejType}{Integer one-sided rejection type supported by the decision helpers: 0 = upper efficacy;
#'     1 = upper futility; 2 = lower efficacy; 3 = lower futility; 4 = upper efficacy and lower futility;
#'     5 = lower efficacy and upper futility.}
#'   \item{InterimVisit}{Integer. Index of the visit which is driving the interims (starts at 1). East Horizon
#'     Explore: Only available for `Endpoint Type = Continuous with Repeated Measures`. East Horizon Design: Not
#'     available.}
#'   \item{IncludePipeline}{Integer. Indicates whether to include pipeline subjects in the interim: - `0`: Do not
#'     include. - `1`: Include. East Horizon Explore: Only available for `Endpoint Type = Continuous with Repeated
#'     Measures`. East Horizon Design: Not available.}
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
#'   \item{PrimDelta}{Estimated treatment effect for the primary contrast. Required for Delta-scale futility using
#'     the primary contrast.}
#'   \item{SecDelta}{Estimated treatment effect for the secondary contrast. Required for Delta-scale futility using
#'     the secondary contrast.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

Analyze.RepeatedMeasures <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {
    # This example is for the case of no dropouts only
    nErrorCode <- 0
    nDecision <- 0
    dPrimDelta <- 0
    dSecDelta <- 0

    # Step 1: Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
    if ( !is.null( LookInfo ) ) {
        nQtyOfLooks <- LookInfo$NumLooks
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfPatsForInterim <- LookInfo$CumCompleters[ nLookIndex ]
        nAnalysisVisit <- LookInfo$InterimVisit
        nRejType <- LookInfo$RejType
        nTailType <- DesignParam$TailType
    } else {
        nLookIndex <- 1
        nQtyOfLooks <- 1
        nQtyOfPatsForInterim <- nrow( SimData )
        nTailType <- DesignParam$TailType
    }

    dfWideData <- data.frame( id = seq_len( nrow( SimData ) ), TreatmentID = SimData$TreatmentID )
    vResponseColumns <- c( )
    vVisitTimesColumns <- c( )
    for ( i in 1:DesignParam$NumVisit ) {
        dfWideData[[ paste0( "Response", i ) ]] <- SimData[ , paste0( "Response", i ) ]
        vResponseColumns <- c( vResponseColumns, paste0( "Response", i ) )
        dfWideData[[ paste0( "CalendarVisitTime", i ) ]] <- SimData[ , "ArrivalTime" ] + SimData[ , paste0( "ArrTimeVisit", i ) ]
        vVisitTimesColumns <- c( vVisitTimesColumns, paste0( "CalendarVisitTime", i ) )
    }

    dfLongData <- stats::reshape( dfWideData,
        varying = c( vResponseColumns, vVisitTimesColumns ),
        direction = "long", sep = "", idvar = "id", timevar = "Visit"
    )
    dfLongData <- dfLongData[ order( dfLongData$Visit, dfLongData$CalendarVisitTime ), ]

    if ( !is.null( LookInfo ) ) {
        dAnalysisTime <- dfLongData[ dfLongData[[ "Visit" ]] == nAnalysisVisit, ][ nQtyOfPatsForInterim, "CalendarVisitTime" ]

        if ( LookInfo$IncludePipeline == 0 ) {
            vSubjectsForAnalysis <- unique( dfLongData[ dfLongData[[ "Visit" ]] == nAnalysisVisit & dfLongData[[ "CalendarVisitTime" ]] <= dAnalysisTime, "id" ] )
        } else {
            vSubjectsForAnalysis <- unique( dfLongData[ dfLongData[[ "CalendarVisitTime" ]] <= dAnalysisTime, "id" ] )
        }

        dfAnalysisData <- dfLongData[ dfLongData[[ "id" ]] %in% vSubjectsForAnalysis, ]
        if ( nLookIndex < nQtyOfLooks ) {
            dfAnalysisData <- dfAnalysisData[ dfAnalysisData$CalendarVisitTime <= dAnalysisTime, ]
        }
    } else {
        dfAnalysisData <- dfLongData
    }

    cMMRMModel <- nlme::gls( Response ~ TreatmentID,
        na.action = stats::na.omit, data = dfAnalysisData,
        correlation = nlme::corSymm( form = ~ Visit | id ),
        weights = nlme::varIdent( form = ~ 1 | Visit )
    )

    dTValue <- summary( cMMRMModel )$tTable[ "TreatmentID", "t-value" ]
    dPValue <- stats::pt( dTValue, cMMRMModel$dims$N - cMMRMModel$dims$p,
        lower.tail = DesignParam$TailType == 0
    )

    # Get group sequential boundaries
    if ( !is.null( LookInfo ) ) {
        vInformationRates <- LookInfo$InfoFrac
        if ( is.null( vInformationRates ) ) {
            vInformationRates <- NA_real_
        }
        vGroupSequentialBoundaries <- rpact::getDesignGroupSequential( kMax = nQtyOfLooks, informationRates = vInformationRates,
            alpha = DesignParam$Alpha, sided = 1, typeOfDesign = "OF" )
        dAlpha <- vGroupSequentialBoundaries$stageLevels[ nLookIndex ]
    } else {
        dAlpha <- DesignParam$Alpha
    }

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
        bIAEfficacyCondition = dPValue <= dAlpha,
        bFAEfficacyCondition = dPValue <= dAlpha
    )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    return( list( Decision = as.integer( nDecision ), PrimDelta = as.double( dPrimDelta ), SecDelta = as.double( dSecDelta ), ErrorCode = as.integer( nErrorCode ) ) )
}
