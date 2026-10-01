######################################################################################################################## .
#' @name RandomizationSubjectsUsingSampleFunctionInR
#' @title Randomize Subjects to Two Arms Using R's sample() Function
#' @description The following function randomly allots the subjects on either of two arms (control and treatment).
#'   Steps:
#'
#' 1) Let p = Allocation fraction on Control arm and 1 - p = Allocation fraction on treatment arm. 2) Compute
#'   Expected Sample size (rounded) for Control and treatment arms using Allocation Fraction and Total sample size.
#'   3) Generate a Binary vector where nC = Control sample size and nT = Treatment sample size using sample()
#'   functionality available in R.
#' @author Shubham Lahoti, Gabriel Potvin, Anoop Singh Rawat
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArms Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#' @param AllocRatio Numeric vector of experimental-to-control allocation ratios, one element per experimental arm.
#'   The control allocation is 1, so a ratio of 2 assigns twice as many subjects to that experimental arm as to
#'   control.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
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

RandomizationSubjectsUsingSampleFunctionInR <- function( NumSub, NumArms, AllocRatio, UserParam = NULL ) {
    nErrorCode <- 0

    if ( NumArms != 2 ) {
        return( list( TreatmentID = rep( 0L, NumSub ), ErrorCode = -1L ) )
    }

    # Allocation ratio on control and treatment arm
    vAllocRatio <- c( 1, AllocRatio )

    # Convert the Allocation Ratio to Allocation Fraction for control and treatment arm
    vAllocFraction <- c( vAllocRatio[ 1 ] / sum( vAllocRatio ), 1 - vAllocRatio[ 1 ] / sum( vAllocRatio ) )
    vSampleSizeArmWise <- c( round( NumSub * vAllocFraction[ 1 ] ), NumSub - round( NumSub * vAllocFraction[ 1 ] ) )

    # Find the indices for Control and treatment arms
    vControlArmIndex <- sample( seq_len( NumSub ), size = vSampleSizeArmWise[ 1 ], replace = FALSE )
    vTreatmentArmIndex <- setdiff( seq_len( NumSub ), vControlArmIndex )

    # Generate a vector of zeroes of size NumSub and then replace the Treatment Indices with 1.
    vTreatmentIDs <- rep( 0, NumSub )
    vTreatmentIDs[ vTreatmentArmIndex ] <- 1

    return( list( TreatmentID = as.integer( vTreatmentIDs ), ErrorCode = as.integer( nErrorCode ) ) )
}
