######################################################################################################################## .
#' @name GenerateMMRMResponses
#' @title Simulate Response Data for MMRM Analysis in Two-arm Confirmatory Trial
#' @description
#' Simulates multivariate normal repeated-measures responses for control and
#' treatment subjects.
#' @author Jacob Wathen
#' @param NumSub Integer number of subjects in the trial.
#' @param NumVisit Integer number of visits.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param Inputmethod Integer input-method code: 0 for actual means and standard deviations; 1 for change from baseline. Not used by this example.
#' @param VisitTime Numeric vector of length `NumVisit`, indicating the visit times. Not used by this example.
#' @param MeanControl Numeric vector of length `NumVisit`, containing control-arm means by visit.
#' @param MeanTrt Numeric vector of length `NumVisit`, containing treatment-arm mean responses by visit.
#' @param StdDevControl Numeric vector of length `NumVisit`, containing control-arm standard deviations by visit.
#' @param StdDevTrt Numeric vector of length `NumVisit`, containing treatment-arm standard deviations by visit.
#' @param CorrMat Numeric `NumVisit` by `NumVisit` correlation matrix between visits.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'
#' @return A list containing:
#'       \describe{
#'         \item \code{Response1}, \code{Response2}, ..., \code{ResponseN}: Simulated response vectors for each visit.
#'         \item \code{ErrorCode}: Integer error code (0 = success, -1 = input dimension mismatch).
#'       }
######################################################################################################################## .

GenerateMMRMResponses <- function( NumSub, NumVisit, ArrivalTime, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL )
{
    # Initialize outputs
    nError <- 0
    lRet   <- list()

    # Step 1: Validate input dimensions ####
    if( length( MeanControl )   != NumVisit ||
         length( MeanTrt )      != NumVisit ||
         length( StdDevControl ) != NumVisit ||
         length( StdDevTrt )    != NumVisit ||
         nrow( CorrMat )        != NumVisit ||
         ncol( CorrMat )        != NumVisit )
    {
        nError <- -1
        lRet$ErrorCode <- as.integer( nError )
        return( lRet )
    }

    # Step 2: Build covariance matrices for each arm ####
    CovMatControl <- ( StdDevControl %*% t( StdDevControl ) ) * CorrMat
    CovMatTrt     <- ( StdDevTrt     %*% t( StdDevTrt ) )     * CorrMat

    # Step 3: Draw multivariate‐normal samples for each arm ####
    ControlResponses <- MASS::mvrnorm( n     = sum( TreatmentID == 0 ),
                                      mu    = MeanControl,
                                      Sigma = CovMatControl )

    TrtResponses     <- MASS::mvrnorm( n     = sum( TreatmentID == 1 ),
                                      mu    = MeanTrt,
                                      Sigma = CovMatTrt )

    # Step 4: Combine responses into a matrix ####
    Responses <- matrix( 0, nrow = NumSub, ncol = NumVisit )

    Responses[ TreatmentID == 0, ] <- ControlResponses
    Responses[ TreatmentID == 1, ] <- TrtResponses

    # Step 5: Return the simulated outcomes and error code ####
    for( i in seq_len( NumVisit ) )
    {
        lRet[[ paste0( "Response", i ) ] ] <- as.double( Responses[ , i ] )
    }

    lRet$ErrorCode <- as.integer( nError )

    return( lRet )

}
