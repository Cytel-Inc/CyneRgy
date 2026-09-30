######################################################################################################################## .
#' @name ComputeHazardWeibull
#'
#' @title Compute Hazard Weibull
#'
#' @description Function to compute the hazard of the Weibull distribution
#'
#' @author Valeria A. G. Mazzanti, J. Kyle Wathen, and Gabriel Potvin
#'
#' @param vTime Numeric vector of times at which to evaluate the Weibull hazard.
#'
#' @param dShape Positive numeric shape parameter of the Weibull distribution, as used by `stats::rweibull()`.
#'
#' @param dScale Positive numeric scale parameter of the Weibull distribution, as used by `stats::rweibull()`.
#'
#' @return Numeric vector of Weibull hazard values at vTime.
######################################################################################################################## .

ComputeHazardWeibull <- function( vTime, dShape, dScale ) {
    vHaz <- ( dShape / dScale ) * ( vTime / dScale )^( dShape - 1 )
    return( vHaz )
}

# ComputeScaleGivenShapeMedian computes the Weibull scale parameter corresponding to a supplied shape and median.
# It accepts `dShape` and `dMedian` and returns the Weibull scale parameter.
######################################################################################################################## .
#' @name ComputeScaleGivenShapeMedian
#'
#' @title Compute Scale Given Shape Median
#'
#' @description Compute the scale parameter of a Weibull distribution from its shape and median survival time.
#'
#' @author Valeria A. G. Mazzanti, J. Kyle Wathen, and Gabriel Potvin
#'
#' @param dShape Positive numeric shape parameter of the Weibull distribution, as used by `stats::rweibull()`.
#'
#' @param dMedian Positive numeric median survival time of the Weibull distribution.
#'
#' @return Positive numeric Weibull scale parameter.
######################################################################################################################## .

ComputeScaleGivenShapeMedian <- function( dShape, dMedian ) {
    dScale <- dMedian / exp( log( -log( 0.5 ) ) / dShape )
    return( dScale )
}

# ----------------------------------------------------------------------------------------------------------------------
# Example - Weibull with Constant Hazards with median of 12 vs 16 ####
# ----------------------------------------------------------------------------------------------------------------------
dShapeS <- 1
dMedianS <- 12

dScaleS <- ComputeScaleGivenShapeMedian( dShapeS, dMedianS )
dScaleS

nQtyPats <- 10000
vTime <- seq( 0.05, 40, 0.05 )
vHazardS <- ComputeHazardWeibull( vTime, dShapeS, dScaleS )
vDataS <- stats::rweibull( nQtyPats, dShapeS, dScaleS )

dShapeE <- 1
dMedianE <- 16
dScaleE <- ComputeScaleGivenShapeMedian( dShapeE, dMedianE )
dScaleE

vHazardE <- ComputeHazardWeibull( vTime, dShapeE, dScaleE )
vDataE <- stats::rweibull( nQtyPats, dShapeE, dScaleE )

plot( vTime, vHazardS, type = "l", xlab = "Time (Months)", ylab = "Hazard", main = "Hazard: Standard of Care (Solid), Experimental (Dashed)" )
graphics::lines( vTime, vHazardE, lty = 2 )
#
#
# print( paste( "Parameters for S: Shape = ", round( dShapeS, 3), ", Scale= ", round( dScaleS, 3 )) )
# print( paste( "Parameters for E: Shape = ", round( dShapeE, 3), ", Scale= ", round( dScaleE, 3 )) )
# print( paste( "Observed median on S: ", median( vDataS ) ) )
# print( paste( "Observed median on E: ", median( vDataE ) ) )
# print( paste( "Observed HR=", median( vDataS )/median( vDataE ) ) )
#
# ----------------------------------------------------------------------------------------------------------------------
# Example - Weibull with increasing hazards with median of 12 vs 16 ####
# ----------------------------------------------------------------------------------------------------------------------
dShapeS <- 3
dMedianS <- 12

dScaleS <- ComputeScaleGivenShapeMedian( dShapeS, dMedianS )
dScaleS

nQtyPats <- 10000
vTime <- seq( 0.05, 40, 0.05 )
vHazardS <- ComputeHazardWeibull( vTime, dShapeS, dScaleS )
vDataS <- stats::rweibull( nQtyPats, dShapeS, dScaleS )

dShapeE <- 4
dMedianE <- 16
dScaleE <- ComputeScaleGivenShapeMedian( dShapeE, dMedianE )
dScaleE

vHazardE <- ComputeHazardWeibull( vTime, dShapeE, dScaleE )
vDataE <- stats::rweibull( nQtyPats, dShapeE, dScaleE )

plot( vTime, vHazardS, type = "l", xlab = "Time (Months)", ylab = "Hazard", main = "Hazard: Standard of Care (Solid), Experimental (Dashed)" )
graphics::lines( vTime, vHazardE, lty = 2 )
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
dShapeS <- 0.7
dMedianS <- 12

dScaleS <- ComputeScaleGivenShapeMedian( dShapeS, dMedianS )
dScaleS

nQtyPats <- 10000
vTime <- seq( 0.05, 40, 0.05 )
vHazardS <- ComputeHazardWeibull( vTime, dShapeS, dScaleS )
vDataS <- stats::rweibull( nQtyPats, dShapeS, dScaleS )

dShapeE <- 0.8
dMedianE <- 16
dScaleE <- ComputeScaleGivenShapeMedian( dShapeE, dMedianE )
dScaleE

vHazardE <- ComputeHazardWeibull( vTime, dShapeE, dScaleE )
vDataE <- stats::rweibull( nQtyPats, dShapeE, dScaleE )

plot( vTime, vHazardS, type = "l", xlab = "Time (Months)", ylab = "Hazard", main = "Hazard: Standard of Care (Solid), Experimental (Dashed)" )
graphics::lines( vTime, vHazardE, lty = 2 )

# print( paste( "Parameters for S: Shape = ", round( dShapeS, 3), ", Scale= ", round( dScaleS, 3 )) )
# print( paste( "Parameters for E: Shape = ", round( dShapeE, 3), ", Scale= ", round( dScaleE, 3 )) )
# print( paste( "Observed median on S: ", median( vDataS ) ) )
# print( paste( "Observed median on E: ", median( vDataE ) ) )
# print( paste( "Observed HR=", median( vDataS )/median( vDataE ) ) )
#
