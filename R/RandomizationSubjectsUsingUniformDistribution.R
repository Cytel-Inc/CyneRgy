#################################################################################################### .
#   Program/Function Name: RandomizationSubjectsUsingUniformDistribution
#   Description: Randomize subjects between two treatment arms.
#################################################################################################### .
#' @name RandomizationSubjectsUsingUniformDistribution
#' @title Randomize Subjects Between Two Arms
#'
#' @description Randomly assigns subjects to control (`0`) and experimental (`1`) arms while enforcing the requested final
#' allocation counts.
#'
#' @param NumSub Integer number of subjects to randomize.
#' @param NumArms Integer number of trial arms. This function supports two arms.
#' @param AllocRatio Numeric experimental-to-control allocation ratio.
#' @param UserParam Optional list of user-defined parameters. Retained for compatibility with the randomization integration point.
#'
#' @return A list containing integer vectors `TreatmentID` and `ErrorCode`. `ErrorCode` is `0` on success and `-1` when
#' `NumArms` is not two.
#'
#' @export
#################################################################################################### .

RandomizationSubjectsUsingUniformDistribution <- function( NumSub, NumArms, AllocRatio, UserParam = NULL )
{
    Error <- 0

    if( NumArms != 2 )
        return( list( TreatmentID = rep( 0L, NumSub ), ErrorCode = as.integer( -1 ) ) )

    if( NumSub < 1 || length( AllocRatio ) != 1 || !is.finite( AllocRatio ) || AllocRatio <= 0 )
        stop( "NumSub and AllocRatio must be positive.", call. = FALSE )

    vAllocRatio        <- c( 1, AllocRatio )
    dAllocFraction     <- vAllocRatio / sum( vAllocRatio )
    vSampleSizeArmWise <- c( round( NumSub * dAllocFraction[ 1 ] ), NumSub - round( NumSub * dAllocFraction[ 1 ] ) )
    vTreatmentID       <- as.integer( stats::runif( NumSub ) > dAllocFraction[ 1 ] )

    nQtyOnExperimental <- sum( vTreatmentID )
    if( nQtyOnExperimental > vSampleSizeArmWise[ 2 ] )
    {
        vIndex <- sample( which( vTreatmentID == 1 ), nQtyOnExperimental - vSampleSizeArmWise[ 2 ] )
        vTreatmentID[ vIndex ] <- 0
    }
    else if( nQtyOnExperimental < vSampleSizeArmWise[ 2 ] )
    {
        vIndex <- sample( which( vTreatmentID == 0 ), vSampleSizeArmWise[ 2 ] - nQtyOnExperimental )
        vTreatmentID[ vIndex ] <- 1
    }

    return( list( TreatmentID = as.integer( vTreatmentID ), ErrorCode = as.integer( Error ) ) )
}
