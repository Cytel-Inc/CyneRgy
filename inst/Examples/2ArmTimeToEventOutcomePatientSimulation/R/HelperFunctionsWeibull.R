######################################################################################################################## .
# Helper functions to go with the Weibull example.  ####
######################################################################################################################## .

######################################################################################################################## .
#' @title Compute Hazard of Weibull Distribution
#'
#' @description 
#' Function to compute the hazard of the Weibull distribution.
#'
#' @param vTime A vector of times to compute the hazard of the Weibull distribution.
#' @param dShape The shape parameter of the Weibull distribution. See `rweibull`.
#' @param dScale The scale parameter of the Weibull distribution. See `rweibull`.
######################################################################################################################## .

ComputeHazardWeibull <- function( vTime, dShape, dScale )
{
    vHaz <- ( dShape/dScale ) * ( vTime/dScale )^( dShape-1 )
    return ( vHaz )
}

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

######################################################################################################################## .
# Example - Weibull with Constant Hazards with median of 12 vs 16 ####
######################################################################################################################## .
dShapeS     <- 1
dMedianS    <- 12

dScaleS     <- ComputeScaleGivenShapeMedian( dShapeS, dMedianS )
dScaleS

nQtyPats    <- 10000
vTime       <- seq( 0.05, 40, 0.05)
vHazardS    <- ComputeHazardWeibull( vTime, dShapeS, dScaleS )
vDataS      <- rweibull( nQtyPats, dShapeS, dScaleS )


dShapeE     <- 1
dMedianE    <- 16
dScaleE     <- ComputeScaleGivenShapeMedian( dShapeE, dMedianE )
dScaleE

vHazardE    <- ComputeHazardWeibull( vTime, dShapeE, dScaleE )
vDataE      <- rweibull( nQtyPats, dShapeE, dScaleE )


plot( vTime, vHazardS, type = 'l', xlab = "Time (Months)", ylab="Hazard", main ="Hazard: Standard of Care (Solid), Experimental (Dashed)" )
lines( vTime, vHazardE, lty =2)
# 
# 
# print( paste( "Parameters for S: Shape = ", round( dShapeS, 3), ", Scale= ", round( dScaleS, 3 )) )
# print( paste( "Parameters for E: Shape = ", round( dShapeE, 3), ", Scale= ", round( dScaleE, 3 )) )
# print( paste( "Observed median on S: ", median( vDataS ) ) )
# print( paste( "Observed median on E: ", median( vDataE ) ) )
# print( paste( "Observed HR=", median( vDataS )/median( vDataE ) ) )
# 
######################################################################################################################## .
# Example - Weibull with increasing hazards with median of 12 vs 16 ####
######################################################################################################################## .
dShapeS     <- 3
dMedianS    <- 12

dScaleS     <- ComputeScaleGivenShapeMedian( dShapeS, dMedianS )
dScaleS

nQtyPats    <- 10000
vTime       <- seq( 0.05, 40, 0.05)
vHazardS    <- ComputeHazardWeibull( vTime, dShapeS, dScaleS )
vDataS      <- rweibull( nQtyPats, dShapeS, dScaleS )


dShapeE     <- 4
dMedianE    <- 16
dScaleE     <- ComputeScaleGivenShapeMedian( dShapeE, dMedianE )
dScaleE

vHazardE    <- ComputeHazardWeibull( vTime, dShapeE, dScaleE )
vDataE      <- rweibull( nQtyPats, dShapeE, dScaleE )


plot( vTime, vHazardS, type = 'l', xlab = "Time (Months)", ylab="Hazard", main ="Hazard: Standard of Care (Solid), Experimental (Dashed)" )
lines( vTime, vHazardE, lty =2)
# 
# 
# print( paste( "Parameters for S: Shape = ", round( dShapeS, 3), ", Scale= ", round( dScaleS, 3 )) )
# print( paste( "Parameters for E: Shape = ", round( dShapeE, 3), ", Scale= ", round( dScaleE, 3 )) )
# print( paste( "Observed median on S: ", median( vDataS ) ) )
# print( paste( "Observed median on E: ", median( vDataE ) ) )
# print( paste( "Observed HR=", median( vDataS )/median( vDataE ) ) )
# 
# ######################################################################################################################## .
# # Example - Weibull with decreasing hazards with median of 12 vs 16 ####
# ######################################################################################################################## .
dShapeS     <- 0.7
dMedianS    <- 12

dScaleS     <- ComputeScaleGivenShapeMedian( dShapeS, dMedianS )
dScaleS

nQtyPats    <- 10000
vTime       <- seq( 0.05, 40, 0.05)
vHazardS    <- ComputeHazardWeibull( vTime, dShapeS, dScaleS )
vDataS      <- rweibull( nQtyPats, dShapeS, dScaleS )


dShapeE     <- 0.8
dMedianE    <- 16
dScaleE     <- ComputeScaleGivenShapeMedian( dShapeE, dMedianE )
dScaleE

vHazardE    <- ComputeHazardWeibull( vTime, dShapeE, dScaleE )
vDataE      <- rweibull( nQtyPats, dShapeE, dScaleE )


plot( vTime, vHazardS, type = 'l', xlab = "Time (Months)", ylab="Hazard", main ="Hazard: Standard of Care (Solid), Experimental (Dashed)" )
lines( vTime, vHazardE, lty =2)

# print( paste( "Parameters for S: Shape = ", round( dShapeS, 3), ", Scale= ", round( dScaleS, 3 )) )
# print( paste( "Parameters for E: Shape = ", round( dShapeE, 3), ", Scale= ", round( dScaleE, 3 )) )
# print( paste( "Observed median on S: ", median( vDataS ) ) )
# print( paste( "Observed median on E: ", median( vDataE ) ) )
# print( paste( "Observed HR=", median( vDataS )/median( vDataE ) ) )
# 


