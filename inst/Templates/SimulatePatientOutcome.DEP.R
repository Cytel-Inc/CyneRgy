######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#'
#' @title Template: Simulate correlated dual-endpoint responses
#'
#' @description Simulate correlated dual-endpoint responses. Use this template as a starting point for custom
#'   logic. Preserve the engine-supplied argument names and access named list elements by name. Supply additional
#'   user-defined inputs through UserParam where that argument is supported.
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArm Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param EndpointType Integer vector of endpoint types, in EndpointName order: 0 = continuous, 1 = binary, 2 =
#'   time-to-event.
#'
#' @param EndpointName Character vector of endpoint names, in the order specified in East Horizon. Use the actual
#'   names to access endpoint-specific list elements.
#'
#' @param Correlation Integer correlation category between endpoints: 0 = uncorrelated; absolute values 1, 2, 3, 4,
#'   and 5 indicate very weak, weak, moderate, strong, and very strong correlation. Positive values indicate
#'   positive correlation; negative values indicate negative correlation.
#'
#' @param SurvMethod Named list indexed by EndpointName. For each time-to-event endpoint: 1 = hazard rates; 2 =
#'   cumulative survival percentages; 3 = median survival times. The value is NA for a non-survival endpoint.
#'
#' @param NumPrd Named list indexed by EndpointName, containing the integer number of survival periods for each
#'   time-to-event endpoint and NA for a non-survival endpoint.
#'
#' @param PrdTime Named list indexed by EndpointName. Each survival endpoint contains its period times: starting
#'   times of hazard pieces for SurvMethod = 1, times for cumulative survival percentages for SurvMethod = 2, or 0
#'   for SurvMethod = 3. The value is NA for a non-survival endpoint.
#'
#' @param SurvParam Named list indexed by EndpointName. Each survival endpoint contains a NumPrd-by-NumArm array:
#'   hazard rates for SurvMethod = 1, cumulative survival percentages for SurvMethod = 2, or a single row of median
#'   survival times for SurvMethod = 3. Column 1 is control. The value is NA for a non-survival endpoint.
#'
#' @param PropResp Named list indexed by EndpointName. Each binary endpoint contains a numeric vector of response
#'   probabilities by arm, with control first; the value is NA for a non-binary endpoint.
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
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArm, ArrivalTime, TreatmentID, EndpointType, EndpointName, Correlation, SurvMethod = NULL, NumPrd = NULL, PrdTime = NULL, SurvParam = NULL, PropResp = NULL, UserParam = NULL ) {
    # Step 1 - Initialize the return variables or other variables needed ####
    nErrorCode <- 0
    vPatientOutcomeEP1 <- rep( 0, NumSub )
    vPatientOutcomeEP2 <- rep( 0, NumSub )
    lResponse <- list( )

    # Step 2 - Validate custom variable input and set defaults ####
    if ( is.null( UserParam ) ) {
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues. Also, if there is a default value for the parameters, you may want to set them here.

        # EXAMPLE - Set the default if needed
        # UserParam <- list( dProbOfZeroOutcomeCtrl = 0, dProbOfZeroOutcomeExp = 0 )
    }

    # Step 3 - Simulate the patient data and store in lResponse ####
    for ( nSubjID in 1:NumSub ) {
        # Write code to simulate patient data with a specified correlation.
    }

    # Use appropriate error handling and modify the
    # error appropriately in each of the methods.

    lResponse[[ EndpointName[[ 1 ]] ]] <- vPatientOutcomeEP1
    lResponse[[ EndpointName[[ 2 ]] ]] <- vPatientOutcomeEP2

    return( list( Response = as.list( lResponse ), ErrorCode = as.integer( nErrorCode ) ) )
}
