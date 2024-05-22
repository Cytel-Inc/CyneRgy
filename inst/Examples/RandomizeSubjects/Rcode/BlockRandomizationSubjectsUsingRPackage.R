
# Function Template for Randomizing Subjects to Treatments.
#'@name RandomizationSubjects
#'@author Shubham Lahoti
#'@description : The following function randomly allots the subjects on either of two arms (control and treatment).
#'Notations : 
#' 
# 1) b = No of blocks into which we want the total sample size to be divided.
# 2) bc = vector of length/ size of each block. So in this case the vector length will be b as there are b blocks.
# 3) r = No of simulations to generate. In this case r = 1
# 4) ratio = vector of allocation ratio specified in such way that every element in bc is divisible by sum(ratio) and N is also divisible by sum(ratio)
#     i.e. ratio is specified in such way that N/sum(bc) and bc[i]/sum(bc) is integer. bc[i] is ith element of vector bc.
#'5) groups = character vector of labels for the different treatments.
#'6) K = Number of treatment groups.
#'
# UserParam = It is the list of Block lengths. So if a user wants randomization sampling to be done in "b" blocks, provide a list of b components such that each component represnts the length of block.
# For example - for 2 blocks, UserParam <- list(x = 20, y = 10) where 20 is the length of first block and 10 is the length of second block.

#'Library Prerequisite : Installation of a library "randomizeR" is required to do the Block randomization in R.

# Description: 
# The permuted block technique randomizes patients between groups within a set of study participants, called a block. 
# Treatment assignments within blocks are determined so that they are random in order but that the desired allocation proportions are achieved exactly within each block.

#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted.

#' @return retval : This is a binary vector defining the treatment ID where 0 = Subject alloted to Control arm, 
#'                                                                          1 = Subject alloted to treatment arm.

BlockRandomizationSubjectsUsingRPackage <- function( NumSub, NumArms, AllocRatio, UserParam = NULL )
{
    library( randomizeR )
    Error 	= 0
    
    dNumSub                       <- NumSub            # Total Sample size
    dNumArms                      <- 2                 # two arm designs
    
    # Allocation ratio on control and treatment arm
    vAllocRatio                   <- c( 1, AllocRatio )
    
    retval                        <- c(  )
    
    #Block randomization function
    par                           <- pbrPar( bc = unlist( UserParam ), K = 2, ratio = vAllocRatio, groups = c( "0", "1" ) ) 
    R                             <- genSeq( par, r = 1 ) 
    retval                        <- as.numeric( getRandList( R ) )

    return( list( TreatmentID = as.integer( retval ), ErrorCode = as.integer( Error ) ) )
}

