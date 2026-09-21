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
#' @param Inputmethod Integer input-method code: 0 for actual means and standard deviations; 1 for change from baseline.
#' @param VisitTime Numeric vector of length `NumVisit`, indicating the visit times.
#' @param MeanControl Numeric vector of length `NumVisit`, containing control-arm means by visit.
#' @param MeanTrt Numeric vector of length `NumVisit`, containing treatment-arm means by visit.
#' @param StdDevControl Numeric vector of length `NumVisit`, containing control-arm standard deviations by visit.
#' @param StdDevTrt Numeric vector of length `NumVisit`, containing treatment-arm standard deviations by visit.
#' @param CorrMat Numeric `NumVisit` by `NumVisit` correlation matrix between visits.
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#'
#' @return A list containing:
#' \describe{
#'   \item{Response1, ..., ResponseNumVisit}{Required numeric vectors of length `NumSub`, with one generated response vector for each visit.}
#'   \item{ErrorCode}{Integer error code; 0 indicates success and -1 indicates an input-dimension mismatch.}
#' }
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
