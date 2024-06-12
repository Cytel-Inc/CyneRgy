######################################################################################################################## .
# Helper function to go with the Weibull example.  ####
######################################################################################################################## .

#' @param  vTime A vector of times to compute the hazard of the Weibull distribution at
#' @param dShape The shape of the Weibull distribution, see rweibull
#' @param dScale The scale of the Weibull distribution, see rweibull
#' @description Function to compute the hazard of the Weibull distribution 
ComputeHazardWeibulll <- function( vTime, dShape, dScale )
{
    vHaz <- (dShape/dScale) * (vTime/dScale )^(dShape-1)
    return ( vHaz )
}


#' Compute the scale parameter for the Weibull distribution with median = dMedian and scale parameter = dScale
#' @param dShape The shape of the Weibull distribution
#' @param dMedian The median of the Weibull distribution
#' @description Function to compute the scale given the shape and median
ComputeScaleGivenShapeMedian <- function( dShape, dMedian )
{
    dScale <- dMedian/exp( log( -log( 0.5) )/dShape )
    return( dScale )
}

######################################################################################################################## .
# Example - Weibull with increasing hazards with median of 12 vs 15 ####
######################################################################################################################## .
# dShapeS     <- 1.9
# dMedianS    <- 12
# 
# dScaleS     <- ComputeScaleGivenShapeMedian( dShapeS, dMedianS )
# dScaleS
# 
# nQtyPats    <- 10000
# vTime       <- seq( 0.05, 40, 0.05)
# vHazardS    <- ComputeHazardWeibulll( vTime, dShapeS, dScaleS )
# vDataS      <- rweibull( nQtyPats, dShapeS, dScaleS )
# 
# 
# dShapeE     <- 1.9
# dMedianE    <- 15
# dScaleE     <- ComputeScaleGivenShapeMedian( dShapeE, dMedianE )
# dScaleE
# 
# vHazardE    <- ComputeHazardWeibulll( vTime, dShapeE, dScaleE )
# vDataE      <- rweibull( nQtyPats, dShapeE, dScaleE )
# 
# 
# plot( vTime, vHazardS, type = 'l', xlab = "Time", ylab="Hazard", main ="Hazard: Standard of Care (Solid), Experimental (Dashed)" )
# lines( vTime, vHazardE, lty =2)
# 
# 
# print( paste( "Parameters for S: Shape = ", round( dShapeS, 3), ", Scale= ", round( dScaleS, 3 )) )
# print( paste( "Parameters for E: Shape = ", round( dShapeE, 3), ", Scale= ", round( dScaleE, 3 )) )
# print( paste( "Observed median on S: ", median( vDataS ) ) )
# print( paste( "Observed median on E: ", median( vDataE ) ) )
# print( paste( "Observed HR=", median( vDataS )/median( vDataE ) ) )
# 
# ######################################################################################################################## .
# # Example - Weibull with decreasing hazard with median of 12 vs 15 ####
# ######################################################################################################################## .
# # Standard of Care Treatment
# dShapeS     <- 0.6
# dMedianS    <- 12
# 
# dScaleS     <- ComputeScaleGivenShapeMedian( dShapeS, dMedianS )
# dScaleS
# 
# nQtyPats    <- 10000
# vTime       <- seq( 0.05, 40, 0.05)
# vHazardS    <- ComputeHazardWeibulll( vTime, dShapeS, dScaleS )
# vDataS      <- rweibull( nQtyPats, dShapeS, dScaleS )
# 
# 
# dShapeE     <- 0.6
# dMedianE    <- 15
# dScaleE     <- ComputeScaleGivenShapeMedian( dShapeE, dMedianE )
# dScaleE
# 
# vHazardE    <- ComputeHazardWeibulll( vTime, dShapeE, dScaleE )
# vDataE      <- rweibull( nQtyPats, dShapeE, dScaleE )
# 
# 
# plot( vTime, vHazardS, type = 'l', xlab = "Time", ylab="Hazard", main ="Hazard: Standard of Care (Solid), Experimental (Dashed)" )
# lines( vTime, vHazardE, lty =2)
# 
# print( paste( "Parameters for S: Shape = ", round( dShapeS, 3), ", Scale= ", round( dScaleS, 3 )) )
# print( paste( "Parameters for E: Shape = ", round( dShapeE, 3), ", Scale= ", round( dScaleE, 3 )) )
# print( paste( "Observed median on S: ", median( vDataS ) ) )
# print( paste( "Observed median on E: ", median( vDataE ) ) )
# print( paste( "Observed HR=", median( vDataS )/median( vDataE ) ) )
# 


