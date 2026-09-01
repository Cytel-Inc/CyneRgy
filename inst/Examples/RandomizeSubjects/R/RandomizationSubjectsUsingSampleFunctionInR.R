######################################################################################################################## .
#' @name RandomizationSubjectsUsingSampleFunctionInR
#' @title Randomize Subjects to Two Arms Using R's sample() Function
#' @description
#' Randomly assigns subjects to control or treatment while matching the rounded
#' target sample sizes implied by the allocation ratio.
#'
#' Steps:
#'
#' 1) Let p = Allocation fraction on Control arm and 1 - p = Allocation fraction on treatment arm.
#' 2) Compute Expected Sample size (rounded) for Control and treatment arms using Allocation Fraction and Total sample size.
#' 3) Generate a Binary vector where nC = Control sample size and nT = Treatment sample size using sample() functionality available in R.
#'
#' @author Shubham Lahoti, Gabriel Potvin, Anoop Singh Rawat
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArms Integer number of arms in the trial, including placebo/control and experimental arms. This example supports exactly two arms.
#' @param AllocRatio Positive numeric scalar giving the experimental-to-control allocation ratio (`nt/nc`).
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{TreatmentID}{Integer vector. Treatment assignment for each subject (0 = Control, 1 = Treatment).}
#'     \item{ErrorCode}{Integer. Error code: 0 = No error;
#'                                          >0 = Non-fatal error (current simulation aborted, next simulations will run);
#'                                          <0 = Fatal error (no further simulation attempted).
#'     }
#'   }
######################################################################################################################## .

RandomizationSubjectsUsingSampleFunctionInR <- function( NumSub, NumArms, AllocRatio, UserParam = NULL )
{
    nError                          <- 0

    # Allocation ratio on control and treatment arm
    vAllocRatio                   <- c( 1, AllocRatio )

    # Convert the Allocation Ratio to Allocation Fraction for control and treatment arm
    dAllocFraction                <- c( vAllocRatio[ 1 ] / sum( vAllocRatio ), 1 - vAllocRatio[ 1 ] / sum( vAllocRatio ) )
    vSampleSizeArmWise            <- c( round( NumSub * dAllocFraction[ 1 ] ), NumSub - round( NumSub * dAllocFraction[ 1 ] ) )

    # Find the indices for Control and treatment arms
    vControlArmIndex              <- sample( 1:NumSub, size = vSampleSizeArmWise[ 1 ], replace = FALSE )
    vTreatmentArmIndex            <- c( 1:NumSub )[ -vControlArmIndex ]

    # Generate a vector of zeroes of size NumSub and then replace the Treatment Indices with 1.
    retval                        <- rep( 0, NumSub )
    retval[ vTreatmentArmIndex ]  <-  1

    return( list( TreatmentID = as.integer( retval ), ErrorCode = as.integer( nError ) ) )
}
