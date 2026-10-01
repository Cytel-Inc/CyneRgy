######################################################################################################################## .
#' @name GenerateMMRMResponses
#'
#' @title Simulate Response Data for MMRM Analysis in Two-arm Confirmatory Trial
#'
#' @description This function simulates multivariate normal responses for subjects in a mixed model for repeated
#'   measures (MMRM) setting.
#'
#' @author Jacob Wathen
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumVisit Integer number of visits.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param Inputmethod Integer input method: 0 = actual means and standard deviations at each visit; 1 = expected
#'   changes from baseline at each visit. Preserve this engine-supplied spelling.
#'
#' @param VisitTime Numeric vector of visit times measured from enrollment, of length NumVisit and ordered by
#'   visit.
#'
#' @param MeanControl Numeric vector of control-arm mean responses of length NumVisit, ordered by visit.
#'
#' @param MeanTrt Numeric vector of experimental-arm mean responses of length NumVisit, ordered by visit.
#'
#' @param StdDevControl Numeric vector of control-arm response standard deviations of length NumVisit, ordered by
#'   visit.
#'
#' @param StdDevTrt Numeric vector of experimental-arm response standard deviations of length NumVisit, ordered by
#'   visit.
#'
#' @param CorrMat Numeric correlation matrix between visits, with NumVisit rows and NumVisit columns.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response1, ..., ResponseNumVisit}{Numeric response vectors, one per visit, with one element per subject.
#'     Replace NumVisit by the actual number of visits.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' @details Return each visit response as a separate named list element: Response1, Response2, ...,
#'   ResponseNumVisit. Optional ArrivalTime may be included in the function signature when calendar arrival times
#'   are needed; it has the same definition as at the enrollment integration point.
#'
#' Usage of Inputmethod in this example: Integer input method (currently not used).
#'
#' Usage of VisitTime in this example: Numeric vector. Visit times (currently not used).
######################################################################################################################## .

GenerateMMRMResponses <- function( NumSub, NumVisit, ArrivalTime, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL ) {
    # Initialize outputs
    nErrorCode <- 0
    lRet <- list( )

    # Step 1: Validate input dimensions ####
    if ( length( MeanControl ) != NumVisit ||
        length( MeanTrt ) != NumVisit ||
        length( StdDevControl ) != NumVisit ||
        length( StdDevTrt ) != NumVisit ||
        nrow( CorrMat ) != NumVisit ||
        ncol( CorrMat ) != NumVisit ) {
        nErrorCode <- -1
        lRet$ErrorCode <- as.integer( nErrorCode )
        return( lRet )
    }

    # Step 2: Build covariance matrices for each arm ####
    mCovMatControl <- ( StdDevControl %*% t( StdDevControl ) ) * CorrMat
    mCovMatTrt <- ( StdDevTrt %*% t( StdDevTrt ) ) * CorrMat

    # Step 3: Draw multivariate-normal samples for each arm and preserve subject order ####
    mResponses <- matrix( 0, nrow = NumSub, ncol = NumVisit )

    if ( any( TreatmentID == 0 ) ) {
        mResponses[ TreatmentID == 0, ] <- MASS::mvrnorm(
            n = sum( TreatmentID == 0 ), mu = MeanControl, Sigma = mCovMatControl
        )
    }
    if ( any( TreatmentID == 1 ) ) {
        mResponses[ TreatmentID == 1, ] <- MASS::mvrnorm(
            n = sum( TreatmentID == 1 ), mu = MeanTrt, Sigma = mCovMatTrt
        )
    }

    # Step 5: Return the simulated outcomes and error code ####
    for ( i in seq_len( NumVisit ) ) {
        lRet[[ paste0( "Response", i ) ]] <- as.double( mResponses[ , i ] )
    }

    lRet$ErrorCode <- as.integer( nErrorCode )

    return( lRet )
}
