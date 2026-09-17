########################################################################################################################
#' @name SimulateBinaryAndPFS
#' @title Simulate Binary Response and Progression-Free Survival (PFS)
#' @description This function simulates subject-level binary response outcomes and progression-free survival (PFS) times 
#' for a multi-arm clinical trial.

#' @param NumSub Integer. Total number of subjects to simulate.
#' @param NumArm Integer. Total number of arms including control.
#' @param ArrivalTime Numeric vector of subject arrival times.
#' @param TreatmentID Integer vector indicating treatment assignment for each subject (0 = control, 1...n = treatment arms).
#' @param PropResp Numeric vector of response probabilities for each arm. Length must equal `NumArm`.
#' @param UserParam Optional list of user-defined parameters:
#'        \describe{
#'          \item{MedianSurvCtrl}{Median survival time for the control arm}
#'          \item{HR1, HR2, ..., HR(n)}{Hazard ratios for each treatment arm relative to control}
#'        }
#'
#' @return A list with the following components:
#'         \describe{
#'          \item{Response}{Integer vector of binary response outcomes}
#'          \item{PFSNonCens}{Numeric vector of PFS times relative to patient enrollment}
#'          \item{ErrorCode}{Status code indicating success or error type:
#'              \describe{
#'                \item{ErrorCode = 0}{No Error}
#'                \item{ErrorCode = -1}{Hazard ratio parameters (HR1...HRn) are missing or not consecutive}
#'                \item{ErrorCode = -2}{NA or invalid values encountered in simulation output}
#'              }}
#'         }
########################################################################################################################

SimulateBinaryAndPFS <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL )
{
    # Step 1. Initialize error code and output vectors
    nErrorCode     <- 0
    vBinaryOutcome <- rep( 0, NumSub )
    vPFSNonCens    <- rep( NA, NumSub )
    
    # Step 2. Ensure all parameters are present for simulation of the PFS data
    if ( is.null( UserParam ) ) 
    {
        UserParam <- list( MedianSurvCtrl = 12 )
        for ( i in 1:( NumArm - 1 ) ) 
        {
            UserParam[[ paste0( "HR", i ) ]] <- 0.7
        }
    } 
    else 
    {
        if ( is.null( UserParam$MedianSurvCtrl )) 
        {
            UserParam$MedianSurvCtrl <- 12
        }
        for ( i in 1:( NumArm - 1 ) ) 
        {
            HRName <- paste0( "HR", i )
            if ( is.null( UserParam[[ HRName ]] ) ) {
                UserParam[[ HRName ]] <- 0.7
            }
        }
    }
    # Check that HR1, HR2, ..., HR(n) exist and are consecutive
    HRNames   <- names( UserParam )[ grepl( "^HR", names( UserParam ) ) ]
    HRNumbers <- sort( as.integer( sub( "^HR", "", HRNames ) ) )
    
    if ( !all( HRNumbers == seq_len( NumArm - 1 ) ) ) 
    {
        nErrorCode <- -1
        return( list( Response = as.double( vBinaryOutcome ),
                      PFSNonCens = as.double(vPFSNonCens),
                      ErrorCode = as.integer( nErrorCode ) ) )
    }
    
    # Step 3. Convert median survival -> exponential rate
    dRateCtrl   <- log( 2 ) / UserParam$MedianSurvCtrl
    vRates      <- numeric ( NumArm )
    vRates[ 1 ] <- dRateCtrl
    
    for ( i in 2:NumArm ) 
    {
        HRName <- paste0( "HR", i-1 )
        vRates[ i ] <- dRateCtrl * UserParam[[ HRName ]]
    }
    
    # Step 4. Simulate binary and PFS outcomes for each subject
    for ( nPatIndx in 1:NumSub )
    {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # 1-based index for R, while it's 0-based index in assignments
        
        # Simulate binary response
        vBinaryOutcome[ nPatIndx ] <- rbinom( 1, 1, PropResp[ nTreatmentID ] )
        
        # Simulate time-to-event (exponential distribution)
        dRate <- vRates[ nTreatmentID ]
        if ( dRate > 0 ) 
        {
            dEventTime <- rexp( 1, rate = dRate )
        } 
        else 
        {
            dEventTime <- Inf
        }
        vPFSNonCens[ nPatIndx ] <- dEventTime
    }
    
    # Check for NA or invalid values
    if ( any( is.na( vBinaryOutcome ) ) || any( is.na( vPFSNonCens ) ) ) 
    {
        nErrorCode <- -2
    }
    
    return( list( Response = as.double( vBinaryOutcome ), 
                  PFSNonCens = as.double( vPFSNonCens ), 
                  ErrorCode = as.integer( nErrorCode ) ) )
}

