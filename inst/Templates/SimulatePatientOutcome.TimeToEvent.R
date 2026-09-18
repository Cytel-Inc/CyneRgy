######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Simulate Time-to-Event Patient Outcomes
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param SurvMethod Integer survival-generation method: 1 for hazard rates, 2 for cumulative survival probabilities, or 3 for median survival times.
#' @param NumPrd Integer number of survival periods.
#' @param PrdTime Numeric matrix with `NumPrd` rows and `NumArm` columns, indicating the times used to specify survival parameters. For `SurvMethod = 1`, entries are hazard-piece start times; for `SurvMethod = 2`, entries are times at which cumulative survival is specified; for `SurvMethod = 3`, entries default to 0.
#' @param SurvParam Numeric matrix with `NumPrd` rows and `NumArm` columns containing arm-specific survival parameters.
#'   \describe{
#'     \item{SurvMethod = 1}{Hazard rates for each period and arm. Entry `[i, j]` is the hazard rate in period `i` for arm `j`.}
#'     \item{SurvMethod = 2}{Cumulative survival probabilities for each period and arm. Entry `[i, j]` is the cumulative survival probability in period `i` for arm `j`.}
#'     \item{SurvMethod = 3}{One row of median survival times, with one value per arm.}
#'   }
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'             \item{SurvivalTime}{Required numeric value. A vector of generated time to response values for each subject.}
#'             \item{ErrorCode}{Optional integer value \describe{
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'             }
#' @description
#' This template can be used as a starting point for developing custom functionality.  The function signature must remain the same.
#' However, you may choose to ignore the parameters SurvMethod, NumPrd, PrdTime, and SurvParam if the patient simulator
#' you are creating only requires use of parameters the user will add to UserParam
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{
    # Step 1 - Initialize the return variables or other variables needed ####
    nError          <- 0
    vPatientOutcome <- rep( 0, NumSub ) # Note, as you simulate the patient data put in this vector so it can be returned

    # Step 2 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {

        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues. Also, if there is a default value for the parameters you may want to set them here. Default values usually
        # are applied to have the same functionality as East Horizon, see the first example.

        # EXAMPLE - Set the default if needed
        # UserParam <- list( dProbOfZeroOutcomeCtrl = 0, dProbOfZeroOutcomeExp = 0 )
    }

    # Step 3 - Simulate the patient data and store in vPatientOutcome ####

    # Example 1 - using the parameters East Horizon Explore sent. If you don't need this block of code you may delete it.
    if( SurvMethod == 1 ) # Hazard rates
    {
        # Simulate patient data using the hazard rates
    }

    if( SurvMethod == 2 ) # Cumulative % survivals
    {
        # Simulate patient data using cumulative % survivals
    }

    if( SurvMethod == 3 ) # Median survival times
    {
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

    return( list( SurvivalTime = as.double( vPatientOutcome ), ErrorCode = as.integer( nError ) ) )
}
