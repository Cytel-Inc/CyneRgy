######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#'
#' @title Template: Simulate subject survival times
#'
#' @description Simulate subject survival times. Use this template as a starting point for custom logic. Preserve
#'   the engine-supplied argument names and access named list elements by name. Supply additional user-defined
#'   inputs through UserParam where that argument is supported.
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
#' @param SurvMethod Integer survival input method: 1 = hazard rates; 2 = cumulative survival percentages; 3 =
#'   median survival times.
#'
#' @param NumPrd Integer number of survival periods. Equals 1 for multi-arm confirmatory designs and stratified
#'   survival generation.
#'
#' @param PrdTime Times used to specify survival parameters: starting times of hazard pieces for SurvMethod = 1;
#'   times at which cumulative survival percentages are specified for SurvMethod = 2; 0 for SurvMethod = 3. Legacy
#'   East Horizon inputs may be vectors; East Horizon inputs may be period-by-arm arrays (stratum-by-arm arrays with
#'   stratification).
#'
#' @param SurvParam Array of survival parameters with NumPrd rows and NumArm columns, or one row per stratum when
#'   stratification is enabled. Column 1 is control; subsequent columns are experimental arms. Values are hazard
#'   rates for SurvMethod = 1, cumulative survival percentages for SurvMethod = 2, and median survival times for
#'   SurvMethod = 3. Without stratification, the median-survival method has one row.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) {
    # Step 1 - Initialize the return variables or other variables needed ####
    nErrorCode <- 0
    vPatientOutcome <- rep( 0, NumSub ) # Note, as you simulate the patient data put in this vector so it can be returned

    # Step 2 - Validate custom variable input and set defaults ####
    if ( is.null( UserParam ) ) {
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues. Also, if there is a default value for the parameters you may want to set them here. Default values usually
        # are applied to have the same functionality as East Horizon, see the first example.

        # EXAMPLE - Set the default if needed
        # UserParam <- list( dProbOfZeroOutcomeCtrl = 0, dProbOfZeroOutcomeExp = 0 )
    }

    # Step 3 - Simulate the patient data and store in vPatientOutcome ####

    # Example 1 - using the parameters East Horizon Explore sent. If you don't need this block of code you may delete it.
    if ( SurvMethod == 1 ) { # Hazard rates
        # Simulate patient data using the hazard rates
    }

    if ( SurvMethod == 2 ) { # Cumulative % survivals
        # Simulate patient data using cumulative % survivals
    }

    if ( SurvMethod == 3 ) { # Median survival times
        # Simulate patient data using median survival times
    }

    # Example 2 - Loop over the patient vector and simulate from an Exponential distribution, as an example
    # vTrueRates <- c( 1/UserParam$dMeanCtrl, 1/UserParam$dMeanExp )
    # for( nPatIndx in 1:NumSub )
    # {
    #     nTreatmentID                 <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index
    #     vPatientOutcome[ nPatIndx ]  <- rexp( 1, vTrueRates[ nTreatmentID ] )
    # }

    # Use appropriate error handling and modify the
    # error appropriately in each of the methods.

    return( list( SurvivalTime = as.double( vPatientOutcome ), ErrorCode = as.integer( nErrorCode ) ) )
}
