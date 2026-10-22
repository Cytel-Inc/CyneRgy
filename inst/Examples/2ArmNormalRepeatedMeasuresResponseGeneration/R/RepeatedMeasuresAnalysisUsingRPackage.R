######################################################################################################################## .
#' @name MMRMAna
#' @title Analyze repeated-measures subject responses
#' @description Analyze repeated-measures subject responses. Use the documented inputs and outputs to integrate
#'   this function with the simulation workflow.
#' @author Shubham Lahoti
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$TreatmentID`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
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
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{NumVisit}{Integer number of visits. East Horizon Explore: Only available for `Endpoint Type =
#'     Continuous with Repeated Measures`. East Horizon Design: Not available.}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details This fixed-sample example assumes a common treatment effect across all post-baseline visits and
#'   returns its signed normal-equivalent statistic, computed from the model's two-sided t-test p-value.
#'   It does not estimate the configured visit-specific primary and secondary
#'   contrasts. Extend the model before returning PrimDelta or SecDelta.
#'
######################################################################################################################## .

MMRMAna <- function( SimData, DesignParam, UserParam = NULL, LookInfo = NULL ) {
    # This example fits a fixed-sample model with a common treatment effect across follow-up visits.

    SimData$id <- seq_len( nrow( SimData ) )
    nVisits <- DesignParam$NumVisit
    dfLongData <- stats::reshape( SimData,
        varying = paste0( "Response", seq_len( nVisits ) ),
        direction = "long",
        sep = "",
        idvar = "id"
    )
    dfLongData <- dfLongData[ order( dfLongData$id, dfLongData$time ), ]

    # Match baseline responses to subjects before restricting the model data to follow-up visits.
    dfBaseline <- dfLongData[ dfLongData$time == 1, c( "id", "Response" ) ]
    dfFollowUp <- dfLongData[ dfLongData$time != 1, ]
    dfFollowUp$Baseline <- dfBaseline$Response[ match( dfFollowUp$id, dfBaseline$id ) ]
    dfFollowUp$time <- dfFollowUp$time - 1

    cMMRM <- nlme::gls( Response ~ Baseline + TreatmentID,
        data = dfFollowUp,
        na.action = stats::na.omit,
        correlation = nlme::corSymm( form = ~ time | id ),
        weights = nlme::varIdent( form = ~ 1 | time )
    )
    mCoefficients <- summary( cMMRM )$tTable
    dTValue <- mCoefficients[ "TreatmentID", "t-value" ]
    dTwoSidedPValue <- mCoefficients[ "TreatmentID", "p-value" ]
    dTestStatistic <- sign( dTValue ) * stats::qnorm( dTwoSidedPValue / 2, lower.tail = FALSE )

    # Primary and secondary contrasts require a model that estimates visit-specific treatment effects.
    return( list( TestStat = as.double( dTestStatistic ), ErrorCode = 0L ) )
}
