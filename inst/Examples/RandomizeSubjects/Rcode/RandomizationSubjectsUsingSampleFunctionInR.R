
# Function Template for Randomizing Subjects to Treatments.
#'@name RandomizationSubjects
#'@author Shubham Lahoti
#'@description : The following function randomly allots the subjects on either of two arms (control and treatment).
#'Steps : 
#' 
#' 1) Let p = Allocation fraction on Control arm and 1 - p = Allocation fraction on treatment arm.
#' 2) Compute Expected Sample size (rounded) for Control and treatment arms using Allocation Fraction and Total sample size.
#' 3) Generate a Binary vector where nC = Control sample size and nT = Treatment sample size using sample() functionality available in R.
#' 
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted.

#' @return retval : This is a binary vector defining the treatment ID where 0 = Subject alloted to Control arm, 
#'                                                                          1 = Subject alloted to treatment arm.

RandomizationSubjectsUsingSampleFunctionInR <- function(NumSub, NumArms, AllocRatio, UserParam = NULL)
{
    
    Error 	= 0
    
    dNumSub                       <- NumSub            # Total Sample size
    dNumArms                      <- 2                 # two arm designs
    
    # Allocation ratio on control and treatment arm
    vAllocRatio                   <- c( 1, AllocRatio )
    
    # Convert the Allocation Ratio to Allocation Fraction for control and treatment arm
    dAllocFraction                <- c( vAllocRatio[ 1 ]/sum( vAllocRatio ), 1 - vAllocRatio[ 1 ]/sum( vAllocRatio ) )
    vSampleSizeArmWise            <- c( round( dNumSub * dAllocFraction[ 1 ]), dNumSub - round( dNumSub * dAllocFraction[ 1 ] ) )
    
    # Find the indices for Control and treatment arms
    vControlArmIndex              <- sample( 1:dNumSub, size = vSampleSizeArmWise[ 1 ], replace = FALSE )
    vTreatmentArmIndex            <- c( 1:dNumSub )[ -vControlArmIndex ]
    
    # Generate a vector of zeroes of size NumSub and then replace the Treatment Indices with 1.
    
    retval                        <- rep( 0, NumSub )
    retval[ vTreatmentArmIndex ]  =  1
    
    return(list(TreatmentID = as.integer(retval), ErrorCode = as.integer(Error)))
}
