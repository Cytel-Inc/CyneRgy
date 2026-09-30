######################################################################################################################## .
#' @name GenRespDiffOfMeansRepMeasures
#'
#' @title Simulate repeated-measures subject responses
#'
#' @description The following function generates Response Values for Two Arm Continuous Endpoint: Repeated Measures
#'
#' @author Shubham Lahoti
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
#' @param VisitTime Numeric vector of visit times of length NumVisit.
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
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
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
######################################################################################################################## .

GenRespDiffOfMeansRepMeasures <- function( NumSub, NumVisit, ArrivalTime, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL ) {
    nErrorCode <- 0
    lReturn <- list( )
    nQtyTimePoints <- length( MeanControl )

    # Conversion of Correlation matrix to Covariance matrix

    mIntermediateControl <- StdDevControl %*% t( StdDevControl )
    mIntermediateTrt <- StdDevTrt %*% t( StdDevTrt )

    # mIntermediate is an n*n matrix whose generic term is StdDev[i]*StdDev[j] (n is your number of Time points)

    mCovarianceControl <- mIntermediateControl * CorrMat
    mCovarianceTrt <- mIntermediateTrt * CorrMat

    vQtyPatientsPerArm <- table( TreatmentID )

    mCtrl <- MASS::mvrnorm( vQtyPatientsPerArm[ 1 ], MeanControl, Sigma = mCovarianceControl )
    mExp <- MASS::mvrnorm( vQtyPatientsPerArm[ 2 ], MeanTrt, Sigma = mCovarianceTrt )

    # Initialize a matrix to hold the outcomes
    mOutcomes <- matrix( nrow = sum( vQtyPatientsPerArm ), ncol = nQtyTimePoints )

    # Get outcomes for control group
    mOutcomes[ TreatmentID == 0, ] <- mCtrl

    # Get outcomes for experimental group
    mOutcomes[ TreatmentID == 1, ] <- mExp

    # Build the return list; East Horizon expects a Response variable in the return so just make it the first type ####
    lReturn <- list( Response = as.double( mOutcomes[ , 1 ] ), ErrorCode = as.integer( 0 ) )

    # Add all the types to the list
    for ( nTime in 1:nQtyTimePoints ) {
        strTypeName <- paste0( "Response", nTime )
        lReturn[[ strTypeName ]] <- as.double( mOutcomes[ , nTime ] )
    }

    lReturn$ErrorCode <- as.integer( nErrorCode )

    return( lReturn )
}
