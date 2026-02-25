#' @name BlockRandomizationSubjectsUsingRPackage
#' @title Permuted Block Randomization for Two-Armed Trials
#' @author Shubham Lahoti
#' @description Randomly assigns subjects to two arms (control and treatment) using the permuted block randomization technique. This ensures that within each block, the allocation ratio between arms is strictly maintained, minimizing imbalance throughout the enrollment process.
#' @details
#' This function implements permuted block randomization for two-arm clinical trial designs. It divides the total sample size into user-specified blocks, and within each block, subjects are randomized to control or treatment arms according to a specified allocation ratio. The function leverages the `randomizeR` package for generating the randomization sequence.
#' ## Notation:
#' - `b`: Number of blocks (length of `UserParam`).
#' - `bc`: Vector of block sizes (e.g., `c(20, 10)`).
#' - `r`: Number of randomization simulations (set to 1).
#' - `ratio`: Allocation ratio vector (e.g., `c(1, 1)`).
#' - `groups`: Treatment group labels (e.g., `c("0", "1")`).
#' - `K`: Number of treatment groups (must be 2).
#'
#' @section Library Prerequisite:
#' Requires the `randomizeR` package for block randomization.
#'
#' @param NumSub Integer. Total number of subjects to randomize. The value must be divisible by the sum of the allocation ratio (i.e., `sum(c(1, AllocRatio))`), as well as equal to the sum of the block sizes specified in `UserParam`.
#' @param NumArms Integer. Number of arms. Must be exactly 2 (only two-arm designs are supported).
#' @param AllocRatio Integer. Ratio of experimental to control group sample size (nt/nc). Must be a positive integer. The allocation ratio vector is constructed as `c(1, AllocRatio)` (e.g., `c(1, 1)` for equal allocation).
#' @param UserParam List. Named list of block sizes. Each element should be named `BlockSize1`, `BlockSize2`, ..., `BlockSizeX` for X blocks. The sum of all block sizes must equal `NumSub`. Each block size must be a positive integer and a multiple of the sum of the allocation ratio. Example: `UserParam <- list(BlockSize1 = 20, BlockSize2 = 10)`.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{TreatmentID}{Integer vector. Treatment assignment for each subject (0 = Control, 1 = Treatment).}
#'     \item{ErrorCode}{Optional integer value:
#'                      \describe{
#'                        \item{0}{No error.}
#'                        \item{> 0}{Non-fatal error; current simulation is aborted but subsequent simulations continue.}
#'                        \item{< 0}{Fatal error; no further simulations are attempted.}
#'                      }}
#'      }

BlockRandomizationSubjectsUsingRPackage <- function( NumSub, NumArms, AllocRatio, UserParam )
{
    Error 	    <- 0
    
    # Allocation Ratio may arrive from the UI as a double with tiny floating-point error
    # (e.g. 4 -> 4.000000000000000888...). Since the design requires an integer ratio,
    # we treat AllocRatio as an integer only if it is within a small tolerance of a whole number.
    dTolerance <- 1e-5  
    nAllocRatio <- as.integer(round(AllocRatio))
    
    # Variable check ####
    
    # 1. Allocation ratio must be an integer
    if ( abs( AllocRatio - nAllocRatio ) > dTolerance ) 
    {
        return( list( TreatmentID = as.integer( rep( 0, NumSub )), ErrorCode = as.integer( -1 )))
    }
    
    # 2. Only two-arm designs are supported
    if ( NumArms != 2 ) 
    {
        return( list( TreatmentID = as.integer( rep( 0, NumSub )), ErrorCode = as.integer( -2 )))
    }
    
    # 3. Block sizes must be provided
    if ( is.null( names( UserParam ))) 
    {
        return(list(TreatmentID = as.integer(rep(0, NumSub)), ErrorCode = as.integer(-3)))
    }
    
    # 4. Names must be exactly BlockSize1, BlockSize2, ..., BlockSizeX
    nBlocks       <- length( UserParam )
    expectedNames <- paste0( "BlockSize", seq_len( nBlocks ))
    
    if ( !identical( names( UserParam), expectedNames )) 
    {
        return( list( TreatmentID = as.integer( rep( 0, NumSub )), ErrorCode = as.integer( -4 )))
    }
    
    # 5. Block sizes must be integers
    if ( any( unlist( UserParam ) != as.integer( unlist( UserParam )))) 
    {
        return( list( TreatmentID = as.integer( rep( 0, NumSub )), ErrorCode = as.integer( -5 )))
    }
    
    # 6. The sum of block sizes must equal the total number of subjects
    if ( sum( unlist( UserParam )) != NumSub ) 
    {
        return( list( TreatmentID = as.integer( rep( 0, NumSub )), ErrorCode = as.integer( -6 )))
    }

    # 7. Each block size must be a multiple of the sum of the allocation ratio
    if (any( unlist( UserParam ) %% sum( c( 1, nAllocRatio )) != 0 )) 
    {
        return( list( TreatmentID = as.integer( rep( 0, NumSub )), ErrorCode = as.integer( -7 )))
    }
    
    # Allocation ratio on control and treatment arm
    vAllocRatio <- c( 1L, nAllocRatio )
    
    # Treatment ID assignment
    vBlockSize  <- as.integer(unlist(UserParam, use.names = FALSE))
    
    cPar        <- randomizeR::pbrPar( bc = as.integer( vBlockSize ), K = NumArms, ratio =  vAllocRatio, groups = c( "0", "1" ) ) 
    cObject     <- randomizeR::genSeq( cPar, r = 1 )                             
    vReturn     <- as.numeric( randomizeR::getRandList( cObject ) )
    
    return( list( TreatmentID = as.integer( vReturn ), ErrorCode = as.integer( Error ) ) )
}

