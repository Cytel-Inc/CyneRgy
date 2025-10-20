#' @name GenerateMMRMResponses
#' @title Simulate Response Data for MMRM Analysis in Two-arm Confirmatory Trial
#' @description This function simulates multivariate normal responses for subjects in a mixed model for repeated measures (MMRM) setting.
#' @param NumSub Integer. Number of subjects to simulate.
#' @param NumVisit Integer. Number of visits.
#' @param TreatmentID Integer vector of length `NumSub`. Treatment assignment for each subject (for two arm confirmatory: 0 = control, 1 = treatment).
#' @param Inputmethod  Character. Placeholder for input method (currently not used).
#' @param VisitTime Numeric vector. Visit times (currently not used).
#' @param MeanControl Numeric vector of length `NumVisit`. Mean response values for control group.
#' @param MeanTrt Numeric vector of length `NumVisit`. Standard deviations for treatment group.
#' @param StdDevControl Numeric vector of length `NumVisit`. Standard deviations for control group.
#' @param StdDevTrt Numeric vector of length `NumVisit`. Standard deviations for treatment group.
#' @param CorrMat Correlation matrix between all visits.
#' @param UserParam Optional list. Additional user-defined parameters (currently unused).

#' @return A list containing:
#'       \describe
#'       {
#'         \item \code{Response1}, \code{Response2}, ..., \code{ResponseN}: Simulated response vectors for each visit.
#'         \item \code{ErrorCode}: Integer error code (0 = success, -1 = input dimension mismatch).
#'       }
#' @export
######################################################################################################################## .

GenerateMMRMResponses <- function( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL ) 
{
    library(MASS)
    
    # Initialize outputs
    nError <- 0
    lRet   <- list()
    
    # Step 1: Validate input dimensions ####
    if ( length( MeanControl )   != NumVisit ||
         length( MeanTrt )       != NumVisit ||
         length( StdDevControl ) != NumVisit ||
         length( StdDevTrt )     != NumVisit ||
         nrow( CorrMat )         != NumVisit ||
         ncol( CorrMat )         != NumVisit ) 
    {
        nError <- -1                        
        lRet$ErrorCode <- as.integer( nError )
        return( lRet )                   
    }
    
    # Step 2: Build covariance matrices for each arm ####
    CovMatControl <- ( StdDevControl %*% t( StdDevControl ) ) * CorrMat
    CovMatTrt     <- ( StdDevTrt     %*% t( StdDevTrt ) )     * CorrMat
    
    # Step 3: Draw multivariate‐normal samples for each arm ####
    ControlResponses <- mvrnorm( n    = sum( TreatmentID == 0 ),
                                 mu    = MeanControl,
                                 Sigma = CovMatControl )
    
    TrtResponses     <- mvrnorm( n     = sum( TreatmentID == 1 ),
                                 mu    = MeanTrt,
                                 Sigma = CovMatTrt )
    
    # Step 4: Combine responses into a matrix ####
    Responses <- matrix( 0, nrow = NumSub, ncol = NumVisit )
    
    Responses[ TreatmentID == 0, ] <- ControlResponses
    Responses[ TreatmentID == 1, ] <- TrtResponses
    
    # Step 5: Return the simulated outcomes and error code ####
    for ( i in seq_len( NumVisit ) ) 
    {
        lRet[[ paste0( "Response", i ) ]] <- as.double( Responses[ , i ] )
    }
    
    lRet$ErrorCode <- as.integer( nError )
    
    return( lRet )
    
}
