##' Generate Multi-Endpoint Patient Responses
##'
##' @param NumPat Integer. Number of patients in the trial.
##' @param NumArms Integer. Number of arms in the trial (including placebo/control).
##' @param TreatmentID Vector of integers. Treatment assignment for each subject (0 = control, 1 = treatment).
##' @param ArrivalTime Vector of patient arrival times
##' @param EndpointType Vector of integers. Types of endpoints (0 for continuous, 1 for binary, 2 for time-to-event).
##' @param EndpointName Vector of character strings. Names of the endpoints.
##' @param RespParams List. Parameters for each endpoint containing control and treatment group parameters.
##'   Each element corresponds to one endpoint and should contain:
##'   \itemize{
##'     \item For continuous endpoints (EndpointType = 0):
##'       \code{list(Control = c(Mean = 5, SD = 2), Treatment = c(Mean = 10, SD = 2))} where Mean is mean and SD is standard deviation
##'     \item For binary endpoints (EndpointType = 1):
##'       \code{list(Control = .1, Treatment = .5)} where Control and Treatment are the expected proportion of responders on each arm (0-1)
##'     \item For time-to-event endpoints (EndpointType = 2):
##'       \code{list(SurvMethod = 1, NumPiece = periods, StartAtTime = times, Control = params, HR = hazard_ratio)}
##'       \code{list(SurvMethod = 2, NumPiece = periods, ByTime = times, Control = params, HR = hazard_ratio)}
##'       \code{list(SurvMethod = 3, NumPiece = 1, StartAtTime = 0, Control = params, HR = hazard_ratio)}
##'       where:
##'       \itemize{
##'         \item \code{SurvMethod}: 1 = Hazard Rates, 2 = Cumulative % Survival Rates, 3 = Median Survival Times
##'         \item \code{NumPiece}: Number of periods (1 for single period, >1 for piecewise survival)
##'         \item \code{StartAtTime}: Vector of time periods. Contains Starting times of hazard pieces for SurvMethod = 1 and is always 0 for SurvMethod = 3
##'         \item \code{ByTime}: Vector of time periods. Contains times at which cumulative % survivals are specified for SurvMethod = 2
##'         \item \code{Control}: Vector of Control group parameters (hazard rate for method 1, Cumulative survival % for method 2, median survival for method 3)
##'         \item \code{HR}: Vector of Hazard ratio (treatment vs control)
##'       }
##'   }
##' @param Correlation Matrix. Correlation matrix between endpoints (NumEP x NumEP).
##' @param UserParam List. Optional user-defined parameters (default = NULL)
##'
##' @return List containing:
##'   \item{Response}{List of response vectors for each endpoint}
##'   \item{ErrorCode}{Integer error code (0 = success)}
GenerateMEPResponse <- function(NumPat, NumArms, TreatmentID, ArrivalTime, EndpointType, EndpointName, RespParams, Correlation, UserParam = NULL) {
    nError               <- 0
    lResponse            <- list()
    NumEP                <- length( EndpointType )
    
    # Cholesky decomposition of correlation matrix
    mChol <- chol( Correlation )
    
    # Generating (NumPat * NumEP) standard normal responses 
    mZ <- matrix( rnorm( NumPat * NumEP, 0, 1 ), ncol = NumEP )
    
    # Intermediate matrix with correlated normal responses
    mNormResp <- mZ %*% mChol
    
    # Loop through each endpoint
    for( nEP in 1:NumEP )
    {
        vPatientOutcome <- rep( 0, NumPat )
        
        if( EndpointType[ nEP ] == 2 ) # Time-to-event endpoint
        {
            # Get parameters from RespParams
            lParams <- RespParams[[ EndpointName[ nEP ]]]
            dHR <- lParams$HR
            
            if( lParams$SurvMethod == 1 ) # Hazard rates
            {
                vHazardCtrl <- lParams$Control
                vHazardTrt <- vHazardCtrl * dHR
            }
            else if( lParams$SurvMethod == 2 ) # Cumulative % survival
            {
                # Convert cumulative survival to hazard rate
                dTime <- lParams$ByTime
                dSurvCtrl <- lParams$Control / 100
                vHazardCtrl <- -log( dSurvCtrl ) / dTime
                vHazardTrt <- vHazardCtrl * dHR
            }
            else if( lParams$SurvMethod == 3 ) # Median survival times
            {
                dMedianCtrl <- lParams$Control
                vHazardCtrl <- log(2) / dMedianCtrl
                vHazardTrt <- vHazardCtrl * dHR
            }
            
            # Generate survival times
            for( nSubjID in 1:NumPat )
            {
                dHazard <- ifelse( TreatmentID[ nSubjID ] == 0, vHazardCtrl, vHazardTrt )
                vPatientOutcome[ nSubjID ] <- -log( pnorm( mNormResp[ nSubjID, nEP ])) / dHazard
            }
        }
        else if( EndpointType[ nEP ] == 1 ) # Binary endpoint
        {
            # Get parameters from RespParams
            lParams <- RespParams[[ EndpointName[ nEP ]]]
            vPropResp <- c( lParams$Control, lParams$Treatment )
            
            # Thresholds for binary outcome
            vThreshold <- qnorm( vPropResp )
            
            for( nSubjID in 1:NumPat )
            {
                vPatientOutcome[ nSubjID ] <- as.numeric( mNormResp[ nSubjID, nEP ] < vThreshold[ TreatmentID[ nSubjID ] + 1 ])
            }
        }
        else if( EndpointType[ nEP ] == 0 ) # Continuous endpoint
        {
            # Get parameters from RespParams
            lParams <- RespParams[[ EndpointName[ nEP ]]]
            vMeanCtrl <- lParams$Control[1]
            vSDCtrl <- lParams$Control[2]
            vMeanTrt <- lParams$Treatment[1]
            vSDTrt <- lParams$Treatment[2]
            
            for( nSubjID in 1:NumPat )
            {
                if( TreatmentID[ nSubjID ] == 0 )
                {
                    vPatientOutcome[ nSubjID ] <- vMeanCtrl + vSDCtrl * mNormResp[ nSubjID, nEP ]
                }
                else
                {
                    vPatientOutcome[ nSubjID ] <- vMeanTrt + vSDTrt * mNormResp[ nSubjID, nEP ]
                }
            }
        }
        
        # Check for errors
        if( length( vPatientOutcome ) != NumPat || any( is.na( vPatientOutcome ) == TRUE ))
        {
            stop( paste( "Error generating patient outcomes for endpoint", EndpointName[ nEP ], ": Invalid or missing values detected" ))
        }
        
        # Store response
        lResponse[[ EndpointName[ nEP ]]] <- vPatientOutcome
    }
    
    return( list( Response = as.list( lResponse ), ErrorCode = as.integer( nError )))
}