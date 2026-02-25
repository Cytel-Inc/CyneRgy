#' @name RandomizationSubjectsUsingUniformDistribution
#' @author Shubham Lahoti
#' @title Randomize Subjects to Two Arms Using Uniform Sampling
#' @description The following function randomly allots the subjects on either of two arms (control and treatment).
#' Steps: 
#' 1) We generate a random number from Uniform(0, 1). Save it as u.
#' 2) Let p = Allocation fraction on Control arm and 1 - p = Allocation fraction on treatment arm.
#' 3) If u <= p then allot the subject to Control arm else allot the subject to treatment arm.
#' 4) Make sure that Total sample size = Sample size on control + Sample size on treatment arm
#' 
#' @param NumSub Integer. The number of subjects that need to be simulated. The argument value is passed from Engine.
#' @param NumArms Integer. Number of trial arms. Only \code{NumArms == 2} is supported. The argument value is passed from Engine.
#' @param AllocRatio Numeric. The ratio of the experimental group sample size (nt) to control group sample size (nc) i.e. (nt/nc). The argument value is passed from Engine.
#' @param UserParam List. Optional user-defined parameters. Default is \code{NULL}.
#'  
#' @return A list with the following components:
#'   \describe{
#'     \item{TreatmentID}{Integer vector. Treatment assignment for each subject (0 = Control, 1 = Treatment).}
#'     \item{ErrorCode}{Integer. Error code: 0 = No error; 
#'                                          >0 = Non-fatal error (current simulation aborted, next simulations will run); 
#'                                          <0 = Fatal error (no further simulation attempted).
#'     }
#'   }

RandomizationSubjectsUsingUniformDistribution <- function(NumSub, NumArms, AllocRatio, UserParam = NULL)
{
    
    Error <- 0
    
    # Variable check
    if ( NumArms != 2 ) 
    {
        # Only two-arm designs are supported
        return( list( TreatmentID = rep( 0L, NumSub ), ErrorCode = as.integer( -1 )))
    }
    
    # Allocation ratio on control and treatment arm
    vAllocRatio         <- c( 1, AllocRatio )
    
    # Convert the Allocation Ratio to Allocation Fraction for control and treatment arm
    dAllocFraction      <- c( vAllocRatio[ 1 ] / sum( vAllocRatio ), 1 - vAllocRatio[ 1 ] / sum( vAllocRatio ) )
    vSampleSizeArmWise  <- c( round( NumSub * dAllocFraction[ 1 ]), NumSub - round( NumSub * dAllocFraction[ 1 ] ) )
    retval              <- c( )
    u                   <- c( )
    
    for( i in 1:NumSub )
    {   
        u[ i ]               <- runif( 1, 0, 1 )          #generate a random number from U(0, 1)
        
        # Here 0 means subject is allotted to control arm, 1 means subject is allotted to treatment arm.
        # CDF of Uniform (0, 1) is given as F(x) = x. We make use of this CDF to allocate the subjects randomly on either arms.
        if( u[ i ] > dAllocFraction[ 1 ] && sum( retval ) <= vSampleSizeArmWise[ 2 ] )
        {
            retval[ i ]      <- 1
        }
        else if( u[ i ] <= dAllocFraction[ 1 ] && sum( retval == 0 ) <= vSampleSizeArmWise[ 1 ]  )
        {
            retval[ i ]      <- 0
        }
        else
        {
            retval[ i ]      <- 1
        }
    }
    
    # The following chunk of code is to make sure that allotment of patients is exactly the same as per the allocation ratio (expected patients on each arm) provided.
    
    if( sum( retval ) != vSampleSizeArmWise[ 2 ] )               #if observed allotment is not the same as expected allotment 
    {
        if( sum( retval ) > vSampleSizeArmWise[ 2 ] )            # if observed patients on treatment arm > expected patients on treatment arm
        {
            diff         <- sum( retval ) - vSampleSizeArmWise[ 2 ]     # find the difference between No of observed patients and No of expected patients on treatment arm denoted by diff.
            k            <- sample( which( retval == 1 ), diff )        # randomly choose the sample of "diff" indices from set of treatment indices 
            retval[ k ]  <- 0                                           # assign the retval = 0 for the corresponding sampled indices
            
        } 
        else 
        {                                                         # if observed patients on treatment arm < expected patients on treatment arm
            diff         <- vSampleSizeArmWise[ 2 ] - sum( retval )      # find the difference between No of observed patients and No of expected patients on control arm denoted by diff
            k            <- sample( which( retval == 0 ), diff )         # randomly choose the sample of "diff" indices from set of control indices 
            retval[ k ]  <- 1                                            # assign the retval = 1 for the corresponding sampled indices
        } 
    }
    
    return( list( TreatmentID = as.integer( retval ), ErrorCode = as.integer( Error )))
}
