#  Function Template for Randomizing Subjects to Treatments.
#' @name RandomizationSubjects
#' @author Shubham Lahoti
#' @description The following function randomly allots the subjects on either of two arms (control and treatment).
#' Notations: 
#' 
# 1) b = No of blocks into which we want the total sample size to be divided.
# 2) bc = vector of length/ size of each block. So in this case the vector length will be b as there are b blocks.
# 3) r = No of simulations to generate. In this case r = 1
# 4) ratio = vector of allocation ratio specified in such way that every element in bc is divisible by sum(ratio) and N is also divisible by sum(ratio)
#     i.e. ratio is specified in such way that N/sum(bc) and bc[i]/sum(bc) is integer. bc[i] is ith element of vector bc.
#' 5) groups = character vector of labels for the different treatments.
#' 6) K = Number of treatment groups.
#'

#'Library Prerequisite : Installation of a library "randomizeR" is required to do the Block randomization in R.

# Description: 
# The permuted block technique randomizes patients between groups within a set of study participants, called a block. 
# Treatment assignments within blocks are determined so that they are random in order but that the desired allocation proportions are achieved exactly within each block.
#' @param NumSub The number of subjects that need to be simulated, integer value. The argument value is passed from Engine.
#' @param NumArm The number of arms in the trial including experimental and control, integer value. The argument value is passed from Engine.
#' @param AllocRatio The ratio of the experimental group sample size (nt) to control group sample size (nc) i.e. (nt/nc). The argument value is passed from Engine.
#' @param UserParam It is the list of Block lengths. So if a user wants randomization sampling to be done in "X" blocks, provide a list of X components such that each component represents the length of block.
#' The names of the objects must be BlockSize1, BlockSize2,...BlockSizeX. For example - for 2 blocks, 
#' UserParam <- list(BlockSize1 = 20, BlockSize2 = 10) where 20 is the length of first block and 10 is the length of second block.

#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted.

#' @return retval This is a binary vector defining the treatment ID where 0 = Subject allotted to Control arm, 
#'                                                                        1 = Subject allotted to Treatment arm.

BlockRandomizationSubjectsUsingRPackage <- function( NumSub, NumArms, AllocRatio, UserParam = NULL )
{
    Error 	                      <- 0
    
    dNumSub                       <- NumSub            # Total Sample size
    dNumArms                      <- 2                 # two arm designs
    
    # Allocation ratio on control and treatment arm
    vAllocRatio                   <- c( 1, AllocRatio )
    retval                        <- c(  )
    
    vBlockSize                    <- c()
    for( i in 1:length( UserParam ) )
    {
        vBlockSize                <- c( vBlockSize, UserParam[[ paste0( "BlockSize", i )]])
    }
    
    cPar                          <- pbrPar( bc = vBlockSize, K = 2, ratio = vAllocRatio, groups = c( "0", "1" ) ) 
    cObject                       <- genSeq( cPar, r = 1 )                             
    retval                        <- as.numeric( getRandList( cObject ) )
    
    return( list( TreatmentID = as.integer( retval ), ErrorCode = as.integer( Error ) ) )
}

