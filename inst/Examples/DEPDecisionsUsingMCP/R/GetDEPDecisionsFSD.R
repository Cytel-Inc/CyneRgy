######################################################################################################################## .
#' @name GetDEPDecisionsFSD
#' @title Make fixed-sample dual-endpoint decisions with Bonferroni adjustment
#' @description Compute decisions for DEP given test statistic and total Alpha using Bonferroni multiplicity
#'   adjustment method.
#' @author Gabriel Potvin, Anoop Singh Rawat, Pradip Maske
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$TreatmentID`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#' }
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$TailType`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{EndpointName}{Vector of String. Vector of length equal to the number of endpoints, containing the
#'     endpoint names.}
#'   \item{TailType}{Named List of Integer. Named List of length equal to the number of endpoints, indicating the
#'     nature of critical region for each endpoint. For example, `TailType["Endpoint 1"]` is the nature for
#'     Endpoint 1. Possible values: - `0`: Left-tailed. - `1`: Right-tailed.}
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' @param TestStat Named list of endpoint test statistics on the Wald (Z) scale, returned by the endpoint analyses.
#'   Access elements using the actual endpoint names.
#' @param OutList Optional named list used to pass outputs between analysis looks. Return it at one look to receive
#'   the same list as input at the next look; the input is NULL at the first look. Access elements by name.
#'   Available for designs that support passing state between looks.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Named list of integer endpoint decisions in DesignParam$EndpointName order: 0 = no boundary
#'     crossed; 1 = lower efficacy boundary crossed; 2 = upper efficacy boundary crossed; 4 = futility boundary
#'     crossed. The multiplicity-adjustment integration point uses 4 for futility.}
#'   \item{OutList}{Optional named list used to pass outputs between analysis looks. Return it at one look to
#'     receive the same list as input at the next look; the input is NULL at the first look. Access elements by
#'     name. Available for designs that support passing state between looks.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details This example divides Alpha equally between the two endpoints and uses the test statistics supplied
#'   by their analyses. Dropout and censoring adjustments belong in those endpoint analyses. The dual-endpoint
#'   engine exposes Response1 and Response2 and the endpoint-specific calendar times and original censor
#'   indicators listed in SimData. Each endpoint-specific design/list value is indexed by its actual EndpointName.
######################################################################################################################## .

GetDEPDecisionsFSD <- function( SimData, DesignParam, LookInfo = NULL, TestStat, OutList = NULL, UserParam = NULL ) {
    vEndpointName <- DesignParam$EndpointName
    if ( is.null( names( TestStat ) ) ) {
        names( TestStat ) <- vEndpointName
    }
    if ( is.null( names( DesignParam$TailType ) ) ) {
        names( DesignParam$TailType ) <- vEndpointName
    }
    lDecision <- as.list( stats::setNames( rep( 0L, length( vEndpointName ) ), vEndpointName ) )

    for ( strEndpointName in vEndpointName ) {
        dTestStat <- TestStat[[ strEndpointName ]]
        nTailType <- DesignParam$TailType[[ strEndpointName ]]
        if ( !is.na( dTestStat ) ) {
            if ( nTailType == 0 ) {
                if ( stats::pnorm( dTestStat ) < DesignParam$Alpha / 2 ) {
                    lDecision[[ strEndpointName ]] <- 1L
                }
            } else {
                if ( stats::pnorm( dTestStat, lower.tail = FALSE ) < DesignParam$Alpha / 2 ) {
                    lDecision[[ strEndpointName ]] <- 2L
                }
            }
        }
    }

    nErrorCode <- 0L
    lOutList <- list( OutVal = 0 )

    return( list( Decision = lDecision, OutList = lOutList, ErrorCode = nErrorCode ) )
}
