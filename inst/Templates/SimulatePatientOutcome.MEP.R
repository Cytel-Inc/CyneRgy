######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#'
#' @title Template: Simulate multiple-endpoint subject responses
#'
#' @description Simulate multiple-endpoint subject responses. Use this template as a starting point for custom
#'   logic. Preserve the engine-supplied argument names and access named list elements by name. Supply additional
#'   user-defined inputs through UserParam where that argument is supported.
#'
#' @param NumPat Integer number of subjects in the trial.
#'
#' @param NumArms Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param EndpointType Integer vector of endpoint types, in EndpointName order: 0 = continuous, 1 = binary, 2 =
#'   time-to-event.
#'
#' @param EndpointName Character vector of endpoint names, in the order specified in East Horizon. Use the actual
#'   names to access endpoint-specific list elements.
#'
#' @param RespParams List of endpoint-specific parameter lists, in EndpointName order.
#' \describe{
#'   \item{Continuous (EndpointType = 0)}{Control and Treatment each contain the mean and standard deviation, in
#'     that order, for example `list( Control = c( Mean = 5, SD = 2 ), Treatment = c( Mean = 10, SD = 2 ) )`.}
#'   \item{Binary (EndpointType = 1)}{Control and Treatment are response probabilities between 0 and 1, for example
#'     `list( Control = 0.1, Treatment = 0.5 )`.}
#'   \item{Time-to-event (EndpointType = 2)}{SurvMethod selects 1 = hazard rates, 2 = cumulative survival
#'     percentages, or 3 = median survival times. Control contains the method-specific control parameters and HR
#'     contains treatment-to-control hazard ratios. For method 1, NumPiece is the number of hazard pieces and
#'     StartAtTime contains their starting times. For method 2, ByTime contains the times at which Control survival
#'     percentages are specified. Method 3 uses the control median survival time.}
#' }
#'
#' @param Correlation Square matrix of integer correlation categories in EndpointName order. Category 0 =
#'   uncorrelated; absolute values 1, 2, 3, 4, and 5 indicate very weak, weak, moderate, strong, and very strong
#'   correlation. Positive values indicate positive correlation; negative values indicate negative correlation.
#'   This is an engine category matrix, not a numeric Pearson correlation matrix.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response}{Required named list of numeric response vectors, indexed by EndpointName, with one value per
#'     subject in each vector. Time-to-event responses are measured from enrollment.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' @details The integration-point documentation also lists optional ArrivalRank and Corr outputs but does not
#'   specify their structure. These templates and examples return Response and ErrorCode; consult the requirements
#'   of the target East Horizon version before using those optional outputs.
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumPat, NumArms, TreatmentID, ArrivalTime, EndpointType, EndpointName, RespParams, Correlation, UserParam = NULL ) {
    # Step 1 - Initialize a response vector for each endpoint ####
    nErrorCode <- 0
    lResponse <- list( )
    for ( nEndpointIndex in seq_along( EndpointName ) ) {
        lResponse[[ EndpointName[ nEndpointIndex ] ]] <- rep( 0, NumPat )
    }

    # Step 2 - Validate custom inputs and set defaults when needed ####
    # Access additional parameters through UserParam by name.

    # Step 3 - Replace each placeholder vector with simulated endpoint responses ####
    # Use RespParams and Correlation to generate responses for the requested endpoints.

    return( list( Response = lResponse, ErrorCode = as.integer( nErrorCode ) ) )
}
