######################################################################################################################## .
#' @name RandomizeSubjectsAcrossMultipleArms
#'
#' @title Randomize Subjects Across Multiple Arms
#'
#' @description The following function randomly allots the subjects on one of the arms
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

RandomizeSubjectsAcrossMultipleArms <- function( NumSub, NumArms, AllocRatio, UserParam = NULL ) {
    nErrorCode <- 0

    # Allocation ratio on control and treatment arm
    vAllocRatio <- c( 1, AllocRatio ) # First arm (control) has ratio 1

    # Convert the Allocation Ratio to Allocation Fraction for control and treatment arms
    vAllocFraction <- vAllocRatio / sum( vAllocRatio )

    # Calculate target sample sizes based on allocation ratio
    nTargetSampleSize <- floor( NumSub * vAllocFraction )

    # Calculate how many subjects are left to allocate
    nRemaining <- NumSub - sum( nTargetSampleSize )

    # Allocate remaining subjects based on the fractional parts of the ideal allocation
    if ( nRemaining > 0 ) {
        # Calculate fractional parts
        vFractionalParts <- ( NumSub * vAllocFraction ) - nTargetSampleSize

        # Sort arms by fractional parts (descending) to prioritize allocation
        vArmOrder <- order( vFractionalParts, decreasing = TRUE )

        # Allocate remaining subjects to arms with highest fractional parts
        for ( i in 1:nRemaining ) {
            nTargetSampleSize[ vArmOrder[ i ] ] <- nTargetSampleSize[ vArmOrder[ i ] ] + 1
        }
    }

    # Create a vector with the treatment IDs (0 to NumArms-1)
    vAllTreatmentIDs <- 0:( NumArms - 1 )

    # Create a vector with the correct number of each treatment ID
    vTreatmentIDs <- rep( vAllTreatmentIDs, times = nTargetSampleSize )

    # Randomly shuffle the treatment assignments
    vTreatmentIDs <- sample( vTreatmentIDs )

    return( list( TreatmentID = as.integer( vTreatmentIDs ), ErrorCode = as.integer( nErrorCode ) ) )
}
