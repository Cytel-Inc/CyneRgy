######################################################################################################################## .
#' @title Compute Weibull scale parameter
#' 
#' @description
#' Compute the scale parameter for the Weibull distribution 
#' with a given median (`dMedian`) and shape parameter (`dShape`).
#'
#' @param dShape The shape parameter of the Weibull distribution.
#' @param dMedian The median of the Weibull distribution.
######################################################################################################################## .

ComputeScaleGivenShapeMedian <- function( dShape, dMedian )
{
    dScale <- dMedian/exp( log( -log( 0.5) )/dShape )
    return( dScale )
}