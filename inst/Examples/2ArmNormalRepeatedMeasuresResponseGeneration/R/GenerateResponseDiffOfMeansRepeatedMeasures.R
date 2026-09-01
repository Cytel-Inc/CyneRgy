######################################################################################################################## .
#' @name GenRespDiffOfMeansRepMeasures
#' @title Generate Responses for Two-Arm Normal Repeated Measures
#' @author Shubham Lahoti
#' @description The following function generates Response Values for Two Arm Continuous Endpoint: Repeated Measures
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
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'
#' @return A named list containing `Response`, one response vector per visit named `Response1` through `ResponseNumVisit`, and integer `ErrorCode`.
######################################################################################################################## .

GenRespDiffOfMeansRepMeasures      <- function( NumSub, NumVisit, ArrivalTime, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL )
{
  nError                           <- 0
  lReturn                          <- list()
  nQtyTimePoints                   <- length( MeanControl )

  # Conversion of Correlation matrix to Covariance matrix

  mIntermediateControl             <- StdDevControl %*% t( StdDevControl )
  mIntermediateTrt                 <- StdDevTrt %*% t( StdDevTrt )

  # mIntermediate is an n*n matrix whose generic term is StdDev[i]*StdDev[j] (n is your number of Time points)

  mCovarianceControl               <- mIntermediateControl * CorrMat
  mCovarianceTrt                   <- mIntermediateTrt * CorrMat

  vQtyPatientsPerArm               <- table( TreatmentID )

  mCtrl                            <- MASS::mvrnorm( vQtyPatientsPerArm[ 1 ], MeanControl, Sigma = mCovarianceControl )
  mExp                             <- MASS::mvrnorm( vQtyPatientsPerArm[ 2 ], MeanTrt, Sigma = mCovarianceTrt )

  # Initialize a matrix to hold the outcomes
  mOutcomes                        <- matrix( nrow = sum( vQtyPatientsPerArm ), ncol = nQtyTimePoints )

  # Get outcomes for control group
  mOutcomes[ TreatmentID == 0, ]   <- mCtrl

  # Get outcomes for experimental group
  mOutcomes[ TreatmentID == 1, ]   <- mExp

  # Build the return list; East Horizon expects a Response variable in the return so just make it the first type ####
  lReturn <- list( Response = as.double( mOutcomes[ , 1 ] ), ErrorCode = as.integer( 0 ) )

  # Add all the types to the list
  for( nTime in 1:nQtyTimePoints )
  {
    strTypeName <- paste0( "Response", nTime )
    lReturn[[ strTypeName ] ]       <- as.double( mOutcomes[ , nTime ] )
  }

  lReturn$ErrorCode <- as.integer( nError )

  return( lReturn )
}
