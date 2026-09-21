######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Simulate Repeated-Measures Patient Outcomes
#' @description Simulate visit-specific continuous responses with the configured means, standard deviations, and correlation matrix.
#' @param NumSub Integer number of subjects in the trial.
#' @param NumVisit Integer number of visits.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param Inputmethod Integer input-method code: 0 for actual means and standard deviations; 1 for change from baseline.
#' @param VisitTime Numeric vector of length `NumVisit`, indicating the visit times.
#' @param MeanControl Numeric vector of length `NumVisit`, containing control-arm means by visit.
#' @param MeanTrt Numeric vector of length `NumVisit`, containing treatment-arm means by visit.
#' @param StdDevControl Numeric vector of length `NumVisit`, containing control-arm standard deviations by visit.
#' @param StdDevTrt Numeric vector of length `NumVisit`, containing treatment-arm standard deviations by visit.
#' @param CorrMat Numeric `NumVisit` by `NumVisit` correlation matrix between visits.
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#'
#' @return A list containing:
#'             \describe{
#'                  \item{Response1, ..., ResponseNumVisit}{Required numeric vectors of length `NumSub`, with one generated response vector for each visit.}
#'                  \item{ErrorCode}{Optional integer value \describe{
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'             }
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( NumSub, NumVisit, ArrivalTime, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL )
{
    # TO DO : Modify this function appropriately
    nError       <- 0
    vOutResponse <- c()
    retval       <- list()

    # Add code to simulate the patient data as desired.
    # Example of how to create the return list with Response1, Response2, ..., ResponseNumVisit
    # Store the generated continuous response values in # an array called retval.
    # Initializing Response Array to 0
    for( i in 1:NumVisit )
    {
        strVisitName <- paste0( "Response", i )
        vOutResponse <- rep( 0, NumSub )
        retval[[ strVisitName ] ] <- as.double( vOutResponse )
    }

    # Use appropriate error handling and modify the
    # error appropriately
    retval$ErrorCode <- as.integer( nError )
    return( retval )
}
