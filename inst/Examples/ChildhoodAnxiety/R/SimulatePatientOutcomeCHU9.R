######################################################################################################################## .
#' @name SimulatePatientOutcome
#' @title Simulate bounded childhood anxiety score changes
#' @description Generate baseline and follow-up scores for control and experimental arms, round and restrict each
#'   score to the instrument range 9 to 45, and return the baseline-minus-follow-up change. Positive responses
#'   indicate improvement.
#' @author Audrey Wathen, J. Kyle Wathen
#' @param NumSub Integer number of subjects in the trial.
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#' @param Mean Numeric vector of mean responses by arm, with the control arm first, followed by experimental arms
#'   in TreatmentID order.
#' @param StdDev Numeric vector of response standard deviations by arm, with the control arm first, followed by
#'   experimental arms in TreatmentID order.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' \itemize{
#'     \item \code{dMeanFollowUpCtrl} – Mean at follow-up for the control group.
#'     \item \code{dMeanFollowUpExp} – Mean at follow-up for the experimental group.
#'     \item \code{dStdDevFollowUpCtrl} – Standard deviation at follow-up for the control group.
#'     \item \code{dStdDevFollowUpExp} – Standard deviation at follow-up for the experimental group.
#'   }
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details UserParam is required for this example. Missing required fields return ErrorCode = -1. Required fields
#'   are dMeanFollowUpCtrl, dMeanFollowUpExp, dStdDevFollowUpCtrl, dStdDevFollowUpExp. The mean fields specify
#'   arm-specific follow-up means; the standard deviation fields specify the corresponding arm-specific standard
#'   deviations. Responses are baseline minus follow-up, rounded and bounded by the instrument's score range (9 to
#'   45), so changes lie between -36 and 36. In this example, Mean and StdDev specify the baseline
#'   distributions; UserParam supplies the follow-up distributions.
######################################################################################################################## .

SimulatePatientOutcome <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL ) {
    # Required custom parameters define the baseline or follow-up distributions.
    vRequiredParams <- c( "dMeanFollowUpCtrl", "dMeanFollowUpExp", "dStdDevFollowUpCtrl", "dStdDevFollowUpExp" )
    if ( is.null( UserParam ) || !all( vRequiredParams %in% names( UserParam ) ) ||
        any( vapply( UserParam[ vRequiredParams ], is.null, logical( 1 ) ) ) ) {
        return( list( Response = rep( 0, NumSub ), ErrorCode = -1L ) )
    }

    # Initialize variable
    nErrorCode <- 0 # East Horizon code for no errors occurred
    vPatientOutcome <- rep( 0, NumSub ) # Initialize the subject-level response vector.
    vMeanFollowUp <- c( UserParam$dMeanFollowUpCtrl, UserParam$dMeanFollowUpExp )
    vStdDevFollowUp <- c( UserParam$dStdDevFollowUpCtrl, UserParam$dStdDevFollowUpExp )
    # Create vector with the standard deviation

    # Loop over the patients and simulate the outcome according to the treatment they received
    for ( nPatIndx in 1:NumSub ) {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index

        # Simulate from a normal distribution and round to nearest integer
        dBaselineScore <- round( stats::rnorm( 1, Mean[ nTreatmentID ], StdDev[ nTreatmentID ] ) )

        # Generate the follow-up score for this treatment arm.
        dFollowUpScore <- round( stats::rnorm( 1, vMeanFollowUp[ nTreatmentID ], vStdDevFollowUp[ nTreatmentID ] ) )

        # Ensure outcome is within specified range
        dBaselineScore <- max( min( dBaselineScore, 45 ), 9 )
        dFollowUpScore <- max( min( dFollowUpScore, 45 ), 9 )

        # Note: Response = Baseline - Followup so a value above 0 means the patient improved.
        vPatientOutcome[ nPatIndx ] <- dBaselineScore - dFollowUpScore
    }

    # Error Checking
    if ( any( is.na( vPatientOutcome ) ) ) {
        nErrorCode <- -100
    }

    # Build the return object, add other variables to the list as needed
    lReturn <- list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nErrorCode ) )
    return( lReturn )
}
