#' @param GenerateResponseDiffOfMeansRepeatedMeasures
#' @name Generate Responses for 2 arm Normal Repeated measures
#' @Endpoints Repeated measures (2 arm continuous endpoint)
#' @author Shubham Lahoti
#' @description The following function generates Response Values for Two Arm Continuous Endpoint: Repeated Measures
#' @param NumSub The number of subjects that need to be simulated, integer value. The argument value is passed from Engine.
#' @param ProbDrop A Dropout probability for both the arms. The argument value is passed from Engine.
#' @param NumVisit Number of Visits
#' @param TreatmentID Array specifying indexes of arms to which subjects are allocated ﴾one arm index per subject. Index for placebo / control is 0.
#' @param Inputmethod There were two options  1) Actual values, 2) Change from baseline. 
#' Actual values: You give mean and SD values for each visit and using those you will generate responses.
#' Change from baseline: Expected change from baseline at each visit rather than the true means.
#' @param VisitTime Visit Times
#' @param MeanControl Control Mean for all visits
#' @param MeanTrt Treatment Mean for all visits
#' @param StdDevControl Control Standard Deviations for all visits
#' @param StdDevTrt Treatment Standard Deviations for all visits
#' @param CorrMat Correlation Matrix between all visits
#' @param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL. It is an optional parameter.


#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Nonfatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted.

#' @return retval : A set of arrays of response for all subjects. Each array corresponds to each visit user has specified 

GenRespDiffOfMeansRepMeasures      <- function( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL )
{
  Error 	                         <- 0
  lReturn                          <- list()
  nQtyTimePoints                   <- length( MeanControl )
  
  # Conversion of Correlation matrix to Covariance matrix
  
  mIntermediateControl             <- StdDevControl %*% t( StdDevControl )
  mIntermediateTrt                 <- StdDevTrt %*% t( StdDevTrt )
  
  # mIntermediate is an n*n matrix whose generic term is StdDev[i]*StdDev[j] (n is your number of Time points)
  
  mCovarianceTrt                   <- mIntermediateTrt * CorrMat
  
  vQtyPatientsPerArm               <- table( TreatmentID )
  
  mCtrl                            <- MASS::mvrnorm()( vQtyPatientsPerArm[ 1 ], MeanControl, Sigma = mCovarianceControl ) 
  mExp                             <- MASS::mvrnorm()( vQtyPatientsPerArm[ 2 ], MeanTrt,  Sigma = mCovarianceTrt ) 
 
  # Initialize a matrix to hold the outcomes
  mOutcomes                        <- matrix( nrow = sum( vQtyPatientsPerArm ), ncol = nQtyTimePoints )
  
  # Get outcomes for control group
  mOutcomes[ TreatmentID == 0, ]   <- mCtrl
  
  # Get outcomes for experimental group
  mOutcomes[ TreatmentID == 1, ]   <- mExp

  # Build the return list   East expects a Response variable in the return so just make it the first type #### 
  lReturn <- list( Response = as.double( mOutcomes[,1] ), ErrorCode = as.integer( 0 )  )
  
  # Add all the types to the list 
  for( nTime in 1:nQtyTimePoints )
  {
    strTypeName <- paste0( "Response", nTime )
    lReturn[[ strTypeName ]]       <- as.double( mOutcomes[ ,nTime ] )
  }
  
  lReturn$ErrorCode <- as.integer( Error )
  
  return( lReturn )
}
