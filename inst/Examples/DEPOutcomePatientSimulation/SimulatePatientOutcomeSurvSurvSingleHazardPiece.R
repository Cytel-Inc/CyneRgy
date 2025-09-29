#' @name SimulatePatientOutcomeSurvSurv.DEP
#' @author Pradip Maske
#' @title Simulate patient outcomes for Survival-Survival Dual Endpoint design. 
#' @param NumSub The number of subjects that need to be simulated, integer value
#' @param NumArm The number of arms in the trial including experimental and control, integer value
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param EndpointType A vector of endpoint type for each endpoint, 0 (Continuous), 1 (Binary), 2 (TTE). length( EndpointType ) = number of endpoints.
#' @param EndpointName A vector of endpoint names for each endpoint, length( EndpointType ) = number of endpoints.
#' The parameters SurvMethod, NumPrd, PrdTime, SurvParam, and PropResp are lists containing two elements, each corresponding to the input for a specific endpoint.
#' Below is the detail of elements within the respective parameters.
#' @param SurvMethod Contains elements for input methods for TTE endpoints. An element has value 1 (Hazard Rates), 2 (Cumulative % Survivals), 3 (Median Survival Times), NA (non-TTE Endpoint).
#' @param NumPrd Contains elements representing the number of time periods specified for TTE endpoints, and are set to NA for non-TTE endpoints.
#' @param PrdTime \describe{ 
#'      Contains elements where each is a vector of period times for TTE endpoints, depend on corresponding SurvMethod as mentioned below, and are set to NA for non-TTE endpoints.
#'      \item{If input method is Hazard Rates}{element is a vector of starting times of hazard pieces.}
#'      \item{If input method is Cumulative % Survivals}{element is a vector of times at which the cumulative % survivals are specified.}
#'      \item{If input method is Median Survival Times}{element is 0 by default.}
#'      }
#' @param SurvParam \describe{Contains elements where each is a 2D array of parameters used to generate survival times. Each array has the following structure:
#'    \item{If input method is Hazard Rates}{The element is an array (NumPrd rows, NumArm columns) that specifies arm by arm hazard rates (one rate per arm per piece). 
#'    Thus [i, j] the element of the array specifies hazard rate in ith period for jth arm.
#'    Arms are in columns with column 1 is control, column 2 is experimental
#'    Time periods are in rows, row 1 is time period 1, row 2 is time period 2...}
#'    \item{If input method is Cumulative % Survivals}{The element is an array (NumPrd rows,NumArm columns) specifies arm by arm the Cum % Survivals (one value per arm per piece). Thus, SurvParam [i, j] specifies Cum % Survivals in ith period for jth arm.}
#'    \item{If input method is Median Survival Times}{The element is a 1 x 2 array with median survival times on each arms. Column 1 is control, column 2 is experimental }
#'  }
#' @param PropResp \describe{Contains elements where each is a vector of expected proportions of responders in each arm for binary endpoints, and are set to NA for non-binary endpoints.
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
#' @param  UserParam A list of user defined parameters in East.   You must have a default = NULL, as in this example.
#' If UseParam are supplied in East or East Horizon, they will be an element in the list, eg UserParam$ParameterName.  
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed. 
#'             \describe{
#'             \item{Response}{Required. A list which contains arrays of generated response values for each endpoint. It will contain survival times for TTE endpoints and appropriate response values for Binary and Continous endpoints.}
#'             \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }


#' In this example, the response (Survival times) is generated for two correlated Time to Event Endpoints.
#' The hazard inputs are supposed to be single piece, and the generated times will be exponentially distributed. 
#' The steps to simulating patient data in this example follows a two-step procedure.  
#'  Step 1: Generate two standard normal samples, each of size NumSub. 
#'  Step 2: Transform the sample to be correlated (on normal scale) as per the specified input. 
#'  Step 3: Convert the normal responses to the TTE (exponential) responses by using corresponding endpoints hazard input.
 
SimulatePatientOutcomeSurvSurv.DEP <- function( NumSub, NumArm, ArrivalTime=NULL, TreatmentID, 
                                               EndpointType, EndpointName, Correlation, 
                                               SurvMethod, NumPrd, PrdTime, 
                                               SurvParam, PropResp=NULL, UserParam = NULL )
{
    # Note: It can be helpful to save to the parameters that East sent.
    # The next two lines show how you could save the UserParam variable to an Rds file
    # setwd(["ENTER THE DESIRED LOCATION TO SAVE THE FILE"])
    # saveRDS( UserParam, "UserParam.Rds")

    Error               <- 0
    vPatientOutcomeEP1  <- rep( 0, NumSub )  
    vPatientOutcomeEP2  <- rep( 0, NumSub )  
    Response            <- list()
    
    if( !is.null( UserParam ) )
    {
      # Customized logic for data generation using UserParam will go here.
    } else 
    {
      # Get correlation matrix given qualitative correlation input
      mCor <- GetCorrMatrix(Correlation)
        
      # Cholesky decomposition of correlation matrix
      mChol <- chol(mCor)
      
      # Generating (NumSub * 2) standard normal responses 
      mZ <- matrix( rnorm( NumSub*2, 0, 1 ), ncol = 2)
      
      # Intermediate matrix
      mNormResp <- mZ %*% mChol
      
      # Surv times
      for(nSubjID in 1:NumSub) 
      {
        #browser()
        vPatientOutcomeEP1[nSubjID] <- (- log(pnorm(mNormResp[nSubjID, 1]))/ SurvParam[[1]][1, TreatmentID[nSubjID]+1])
        vPatientOutcomeEP2[nSubjID] <- (- log(pnorm(mNormResp[nSubjID, 2]))/ SurvParam[[2]][1, TreatmentID[nSubjID]+1])
      }
      if(length(vPatientOutcomeEP1) !=NumSub || any(is.na(vPatientOutcomeEP1)==TRUE) ||
         length(vPatientOutcomeEP2) !=NumSub || any(is.na(vPatientOutcomeEP2)==TRUE))
      {
        Error <- -100
      }
    } 
    
    Response[[EndpointName[[1]]]] <- vPatientOutcomeEP1
    Response[[EndpointName[[2]]]] <- vPatientOutcomeEP2
    
  return( list( Response = as.list( Response ), ErrorCode = as.integer( Error ) ) )
}


# Helper function to create correlation matrix given qualitative correlation
GetCorrMatrix <- function(Correlation) 
{
  rho <- ifelse( Correlation == 0, 0, 
                 ifelse( Correlation == 1, 0.15,
                 ifelse( Correlation == 2, 0.3,
                 ifelse( Correlation == 3, 0.5,
                 ifelse( Correlation == 4, 0.7,
                 ifelse( Correlation == 5, 0.85,
                 ifelse( Correlation == -1, -0.15,
                 ifelse( Correlation == -2, -0.3,
                 ifelse( Correlation == -3, -0.5,
                 ifelse( Correlation == -4, -0.7,
                 ifelse( Correlation == -5, -0.85 ) ) ) ) ) ) ) ) ) ) )
  
  # Return the 2x2 correlation matrix
  return( matrix( c(1, rho, rho, 1), nrow = 2) )
}


data <- readRDS("C:\\Users\\pradip.maske\\Downloads\\MedSurv_Corr_-Ve.Rds")
SimulatePatientOutcomeSurvSurv.DEP( NumSub = data$NumSub, NumArm=data$NumArm, TreatmentID=data$TreatmentID, 
                                                EndpointType=data$EndpointType, EndpointName=data$EndpointName, 
                                                Correlation = data$Correlation, 
                                                SurvMethod = data$SurvMethod, NumPrd=data$NumPrd, 
                                                PrdTime = data$PrdTime, SurvParam=data$SurvParam)


tempdata <- data
tempdata$SurvMethod$`Overall Survival` <- tempdata$SurvMethod$`Progress Free Survival` <- 1
data <- SimulatePatientOutcomeSurvSurv.DEP( NumSub = data$NumSub, NumArm=data$NumArm, TreatmentID=data$TreatmentID, 
                                            EndpointType=data$EndpointType, EndpointName=data$EndpointName, 
                                            Correlation = data$Correlation, 
                                            SurvMethod = data$SurvMethod, NumPrd=data$NumPrd, 
                                            PrdTime = data$PrdTime, SurvParam=data$SurvParam)
