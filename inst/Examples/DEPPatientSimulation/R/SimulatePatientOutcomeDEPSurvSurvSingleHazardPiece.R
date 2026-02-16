#' @name SimulatePatientOutcomeDEPSurvSurvSingleHazardPiece
#' @author Pradip Maske
#' @title Simulate patient outcomes for Survival-Survival Dual Endpoint design using single piece hazard rates as inputs.
#' 
#' @description
#' In this example, the response (Survival times) is generated for two correlated Time to Event Endpoints.
#' The hazard inputs are single piece hazard rates in this example.
#' The steps to simulating patient data in this example follows a two-step procedure.  
#' Step 1: Generate two standard normal samples, each of size NumSub. 
#' Step 2: Transform the sample to be correlated (on normal scale) as per the specified input. 
#' Step 3: Convert the normal responses to the TTE (exponential) responses by using corresponding endpoints hazard input.
#' 
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param NumArm The number of arms in the trial including experimental and control, integer value
#' @param ArrivalTime Arrival times of the subjects, numeric vector, length( ArrivalTime ) = NumSub.
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param EndpointType A vector of endpoint type for each endpoint, 0 (Continuous), 1 (Binary), 2 (TTE). length( EndpointType ) = number of endpoints.
#' @param EndpointName A vector of endpoint names for each endpoint, length( EndpointType ) = number of endpoints.
#' The parameters SurvMethod, NumPrd, PrdTime, SurvParam, and PropResp are lists containing two elements -- first corresponds to the first endpoint and second to second.
#' Below are the details of elements within the respective parameters.
#' @param SurvMethod A list containing the input methods for each endpoint. TTE endpoint has values 1 (Hazard Rates), 2 (Cumulative % Survivals), 3 (Median Survival Times). Non-TTE endpoints have value NA.
#' @param NumPrd A list containing the number of time periods specified for each endpoint. For TTE endpoints, this represents the number of time intervals for which hazard rates or survival percentages are defined. For non-TTE endpoints, this value is set to NA.
#' @param PrdTime \describe{ 
#'      A list where each element is a vector of period times for TTE endpoints, dependent on the corresponding SurvMethod value. For non-TTE endpoints, this value is set to NA.
#'      \item{If SurvMethod is 1 (Hazard Rates)}{Element is a vector specifying the starting times of each hazard piece. The number of elements equals NumPrd.}
#'      \item{If SurvMethod is 2 (Cumulative % Survivals)}{Element is a vector specifying the time points at which the cumulative survival percentages are defined. The number of elements equals NumPrd.}
#'      \item{If SurvMethod is 3 (Median Survival Times)}{Element is 0 by default as no time periods need to be defined.}
#'      }
#' @param SurvParam \describe{
#'    A list where each element is a 2D array of parameters used to generate survival times based on the corresponding SurvMethod value:
#'    \item{If SurvMethod is 1 (Hazard Rates)}{The element is an array (NumPrd rows, NumArm columns) that specifies arm-specific hazard rates (one rate per arm per time period). 
#'    Element [i, j] specifies the hazard rate in the i-th time period for the j-th arm.
#'    Arms are arranged in columns: column 1 is control arm, column 2 is experimental arm.
#'    Time periods are arranged in rows: row 1 is time period 1, row 2 is time period 2, etc.}
#'    \item{If SurvMethod is 2 (Cumulative % Survivals)}{The element is an array (NumPrd rows, NumArm columns) that specifies arm-specific cumulative survival percentages. 
#'    Element [i, j] specifies the cumulative survival percentage at the i-th time point for the j-th arm.}
#'    \item{If SurvMethod is 3 (Median Survival Times)}{The element is a 1 x NumArm array specifying the median survival time for each arm. 
#'    Column 1 is control arm, column 2 is experimental arm.}
#'  }
#' @param PropResp \describe{
#'    A list where each element is a vector of expected proportions of responders in each arm for binary endpoints.
#'    \item{For binary endpoints}{Element is a vector of length NumArm, where each value represents the expected response proportion for the corresponding arm.}
#'    \item{For non-binary endpoints}{Element is set to NA as response proportions are not applicable.}
#'    }
#' @param Correlation \describe{Correlation between two endpoints as mentioned below,}
#'    \item{0} {Uncorrelated}  
#'    \item{1} {Very Weak Positive}  
#'    \item{2} {Weak Positive}  
#'    \item{3} {Moderate Positive} 
#'    \item{4} {Strong Positive}  
#'    \item{5} {Very Strong Positive}
#'    \item{-1} {Very Weak Negative} 
#'    \item{-2} {Weak Negative} 
#'    \item{-3} {Moderate Negative}  
#'    \item{-4} {Strong Negative} 
#'    \item{-5} {Very Strong Negative}  
#' @param  UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example.
#' If UserParam are supplied in East Horizon, they will be an element in the list, eg UserParam$ParameterName.  
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed. 
#'             \describe{
#'             \item{Response}{Required. A list which contains vectors of generated response values for each endpoint. It will contain survival times for TTE endpoints and appropriate response values for Binary and Continous endpoints.}
#'             \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     } 
 
SimulatePatientOutcomeDEPSurvSurvSingleHazardPiece <- function( NumSub, NumArm, ArrivalTime = NULL, TreatmentID, 
                                                                EndpointType, EndpointName, Correlation, 
                                                                SurvMethod, NumPrd, PrdTime, 
                                                                SurvParam, PropResp = NULL, UserParam = NULL )
{
    nError              <- 0
    vPatientOutcomeEP1  <- rep( 0, NumSub )  
    vPatientOutcomeEP2  <- rep( 0, NumSub )  
    lResponse           <- list()
    
    if( !is.null( UserParam ))
    {
      # Customized logic for data generation using UserParam will go here.
    }
    else 
    {
      # Get correlation matrix given qualitative correlation input
      mCor <- GetCorrMatrix( Correlation )
        
      # Cholesky decomposition of correlation matrix
      mChol <- chol( mCor )
      
      # Generating (NumSub * 2) standard normal responses 
      mZ <- matrix( rnorm( NumSub*2, 0, 1 ), ncol = 2 )
      
      # Intermediate matrix
      mNormResp <- mZ %*% mChol
      
      # Surv times
      for( nSubjID in 1:NumSub ) 
      {
        #browser()
        vPatientOutcomeEP1[ nSubjID ] <- (- log( pnorm( mNormResp[ nSubjID, 1 ]))/ SurvParam[[ 1 ]][ 1, TreatmentID[ nSubjID ] + 1 ])
        vPatientOutcomeEP2[ nSubjID ] <- (- log( pnorm( mNormResp[ nSubjID, 2 ]))/ SurvParam[[ 2 ]][ 1, TreatmentID[ nSubjID ] + 1 ])
      }
      if( length( vPatientOutcomeEP1 ) != NumSub || any( is.na( vPatientOutcomeEP1 ) == TRUE ) ||
          length( vPatientOutcomeEP2 ) != NumSub || any( is.na( vPatientOutcomeEP2 ) == TRUE ))
      {
        nError <- -100
      }
    } 
    
    lResponse[[ EndpointName[[ 1 ]]]] <- vPatientOutcomeEP1
    lResponse[[ EndpointName[[ 2 ]]]] <- vPatientOutcomeEP2
    
  return( list( Response = as.list( lResponse ), ErrorCode = as.integer( nError )))
}


# Helper function to create correlation matrix given qualitative correlation
GetCorrMatrix <- function( Correlation ) 
{
  rho <- ifelse( Correlation ==  0, 0, 
         ifelse( Correlation ==  1, 0.15,
         ifelse( Correlation ==  2, 0.3,
         ifelse( Correlation ==  3, 0.5,
         ifelse( Correlation ==  4, 0.7,
         ifelse( Correlation ==  5, 0.85,
         ifelse( Correlation == -1, -0.15,
         ifelse( Correlation == -2, -0.3,
         ifelse( Correlation == -3, -0.5,
         ifelse( Correlation == -4, -0.7,
         ifelse( Correlation == -5, -0.85 )))))))))))
  
  # Return the 2x2 correlation matrix
  return( matrix( c( 1, rho, rho, 1 ), nrow = 2 ))
}
