######################################################################################################################## .
#' @name RandomizationSubjectsUsingUniformDistribution
#'
#' @title Randomize Subjects to Two Arms Using Uniform Sampling
#'
#' @description The following function randomly allots the subjects on either of two arms (control and treatment).
#'   Steps: 1) We generate a random number from Uniform(0, 1). Save it as u. 2) Let p = Allocation fraction on
#'   Control arm and 1 - p = Allocation fraction on treatment arm. 3) If u <= p then allot the subject to Control
#'   arm else allot the subject to treatment arm. 4) Make sure that Total sample size = Sample size on control +
#'   Sample size on treatment arm
#'
#' @author Shubham Lahoti, Gabriel Potvin, Anoop Singh Rawat
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArms Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#'
#' @param AllocRatio Numeric vector of experimental-to-control allocation ratios, one element per experimental arm.
#'   The control allocation is 1, so a ratio of 2 assigns twice as many subjects to that experimental arm as to
#'   control.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

RandomizationSubjectsUsingUniformDistribution <- function( NumSub, NumArms, AllocRatio, UserParam = NULL ) {
    nErrorCode <- 0

    # Variable check
    if ( NumArms != 2 ) {
        # Only two-arm designs are supported
        return( list( TreatmentID = rep( 0L, NumSub ), ErrorCode = as.integer( -1 ) ) )
    }

    # Allocation ratio on control and treatment arm
    vAllocRatio <- c( 1, AllocRatio )

    # Convert the Allocation Ratio to Allocation Fraction for control and treatment arm
    dAllocFraction <- c( vAllocRatio[ 1 ] / sum( vAllocRatio ), 1 - vAllocRatio[ 1 ] / sum( vAllocRatio ) )
    vSampleSizeArmWise <- c( round( NumSub * dAllocFraction[ 1 ] ), NumSub - round( NumSub * dAllocFraction[ 1 ] ) )
    retval <- c( )
    u <- c( )

    for ( i in 1:NumSub ) {
        u[ i ] <- stats::runif( 1, 0, 1 ) # Generate a random number from U(0, 1)

        # Here 0 means subject is allotted to control arm, 1 means subject is allotted to treatment arm.
        # CDF of Uniform (0, 1) is given as F(x) = x. We make use of this CDF to allocate the subjects randomly on either arms.
        if ( u[ i ] > dAllocFraction[ 1 ] && sum( retval ) <= vSampleSizeArmWise[ 2 ] ) {
            retval[ i ] <- 1
        } else if ( u[ i ] <= dAllocFraction[ 1 ] && sum( retval == 0 ) <= vSampleSizeArmWise[ 1 ] ) {
            retval[ i ] <- 0
        } else {
            retval[ i ] <- 1
        }
    }

    # The following chunk of code is to make sure that allotment of patients is exactly the same as per the allocation ratio (expected patients on each arm) provided.

    if ( sum( retval ) != vSampleSizeArmWise[ 2 ] ) { # If observed allotment is not the same as expected allotment
        if ( sum( retval ) > vSampleSizeArmWise[ 2 ] ) { # if observed patients on treatment arm > expected patients on treatment arm
            diff <- sum( retval ) - vSampleSizeArmWise[ 2 ] # find the difference between No of observed patients and No of expected patients on treatment arm denoted by diff.
            k <- sample( which( retval == 1 ), diff ) # randomly choose the sample of "diff" indices from set of treatment indices
            retval[ k ] <- 0 # assign the retval = 0 for the corresponding sampled indices
        } else { # if observed patients on treatment arm < expected patients on treatment arm
            diff <- vSampleSizeArmWise[ 2 ] - sum( retval ) # find the difference between No of observed patients and No of expected patients on control arm denoted by diff
            k <- sample( which( retval == 0 ), diff ) # randomly choose the sample of "diff" indices from set of control indices
            retval[ k ] <- 1 # assign the retval = 1 for the corresponding sampled indices
        }
    }

    return( list( TreatmentID = as.integer( retval ), ErrorCode = as.integer( nErrorCode ) ) )
}
