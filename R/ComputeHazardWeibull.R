######################################################################################################################## .
#' @name ComputeHazardWeibull
#' @title Compute Hazard of Weibull Distribution
#'
#' @description 
#' Function to compute the hazard of the Weibull distribution.
#'
#' @param vTime A vector of times to compute the hazard of the Weibull distribution.
#' @param dShape The shape parameter of the Weibull distribution. See `rweibull`.
#' @param dScale The scale parameter of the Weibull distribution. See `rweibull`.
#' @export
######################################################################################################################## .

ComputeHazardWeibull <- function( vTime, dShape, dScale )
{
    vHaz <- ( dShape/dScale ) * ( vTime/dScale )^( dShape-1 )
    return ( vHaz )
}