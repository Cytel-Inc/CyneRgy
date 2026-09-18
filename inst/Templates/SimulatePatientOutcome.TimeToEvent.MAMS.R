######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}

#' @name {{FUNCTION_NAME}}
#' @title Simulate Multi-Arm Time-to-Event Patient Outcomes
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#'
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#'        length(ArrivalTime) = NumSub
#'
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#'        0 = control, 1,2,...,NumArm-1 for treatment arms.
#'        length(TreatmentID) = NumSub
#'
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
#'
#' @return The function must return a list in the return statement of the function. The information below lists
#'         elements of the list, if the element is required or optional and a description of the return values if needed.
#'         \describe{
#'         \item{SurvivalTime}{Required numeric vector. Contains generated survival times for all subjects}
#'         \item{ErrorCode}{Optional integer value
#'                         \describe{
#'                         \item{ErrorCode = 0}{No Error}
#'                         \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                         \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                         }
#'                         }
#'         }
#'
#' @description
#' This is patient data generation task template for Multi-Arm, Time to Events.
#'
#' The function signature must remain unchanged. However, additional user-defined logic
#' and parameters may be incorporated through the UserParam list if needed.
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{

    # Step 1 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {

        # If this function requires user defined parameters to be sent via the UserParam variable
        # check to make sure the values are valid and take care of any issues.
        # Also, if there is a default value for the parameters you may want to set them here

    }

    # Step 2 - Initialize variables ####
    # Initialize error codes and vectors used to store simulated survival times
    nError  <- 0
    vResponse <- c()

    # Step 3 - Determine which survival generation method will be used ####
    # The implementation supports:
    #   Method 1 - Piecewise exponential hazard model
    #   Method 2 - Survival probability based piecewise exponential model
    #   Method 3 - Median survival time based exponential model

    # Step 4 - Implement the data-generation logic ####

    # Step 5 - Error checking ####
    # Verify that all subjects received valid survival times
    # and that no missing values were generated
    if( length( vResponse ) != NumSub || any( is.na( vResponse ) == TRUE ) )
        nError <- -100

    # Step 6 - Build the return object ####
    lReturn <- list(
        SurvivalTime = as.double( vResponse ),
        ErrorCode    = as.integer( nError )
    )

return( lReturn )

}
