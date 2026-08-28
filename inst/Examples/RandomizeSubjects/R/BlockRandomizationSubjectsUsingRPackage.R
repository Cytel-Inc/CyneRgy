######################################################################################################################## .
#' @name BlockRandomizationSubjectsUsingRPackage
#' @title Permuted Block Randomization for Two-Armed Trials
#' @author Shubham Lahoti, Gabriel Potvin, Anoop Singh Rawat
#' @description
#' Randomly assigns subjects to two arms using permuted block randomization so
#' that the requested allocation ratio is maintained within each block.
#' @details
#' This function implements permuted block randomization for two-arm clinical trial designs. It divides the total sample size into user-specified blocks, and within each block, subjects are randomized to control or treatment arms according to a specified allocation ratio. The function leverages the `randomizeR` package for generating the randomization sequence.
#'
#' The allocation ratio is defined as \deqn{1 : AllocRatio} where `AllocRatio = n_t / n_c`.
#' Since the underlying `randomizeR` implementation requires integer allocation ratios, the provided numeric ratio is internally converted to the smallest integer ratio using a continued fraction approximation.
#' The allocation vector is always constructed as c(control, treatment)
#'
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
#' @param NumSub Integer. Total number of subjects to randomize. The value must be divisible by the sum of converted integer allocation ratio, as well as equal to the sum of the block sizes specified in `UserParam`.
#' @param NumArms Integer. Number of arms. Must be exactly 2 (only two-arm designs are supported).
#' @param AllocRatio Numeric. Ratio of experimental to control group sample size (nt/nc). Must be a positive value.
#' @param UserParam List. Named list of block sizes. Each element must be named `BlockSize1`, `BlockSize2`, ..., `BlockSizeX` for X blocks. The sum of all block sizes must equal `NumSub`. Each block size must be a positive integer and a multiple of the sum of the allocation ratio. Example: `UserParam <- list(BlockSize1 = 20, BlockSize2 = 10)`.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{TreatmentID}{Integer vector. Treatment assignment for each subject (0 = Control, 1 = Treatment).}
#'     \item{ErrorCode}{Integer status code:
#'                      \itemize{
#'                              \item 0  : Success
#'                              \item -1 : Invalid number of arms
#'                              \item -2 : Missing UserParam
#'                              \item -3 : Incorrect block naming format
#'                              \item -4 : Non-integer block size detected
#'                              \item -5 : Block sizes do not sum to NumSub
#'                              \item -6 : Block size incompatible with allocation ratio
#'     }}
#'  }
######################################################################################################################## .

BlockRandomizationSubjectsUsingRPackage <- function( NumSub, NumArms, AllocRatio, UserParam = NULL )
{
    nError        <- 0

    # Convert the allocation ratio
    vAllocRatio <- ConvertRatio( AllocRatio )

    # Variable check ####
    # 1. Only two-arm designs are supported
    if( NumArms != 2 )
    {
        return( list( TreatmentID = as.integer( rep( 0, NumSub ) ), ErrorCode = as.integer( -1 ) ) )
    }

    # 2. Block sizes must be provided
    if( is.null( names( UserParam ) ) )
    {
        return( list( TreatmentID = as.integer( rep( 0, NumSub ) ), ErrorCode = as.integer( -2 ) ) )
    }

    # 3. Names must be exactly BlockSize1, BlockSize2, ..., BlockSizeX
    nBlocks       <- length( UserParam )
    expectedNames <- paste0( "BlockSize", seq_len( nBlocks ) )

    if( !identical( names( UserParam ), expectedNames ) )
    {
        return( list( TreatmentID = as.integer( rep( 0, NumSub ) ), ErrorCode = as.integer( -3 ) ) )
    }

    # 4. Block sizes must be integers
    if( any( unlist( UserParam ) != as.integer( unlist( UserParam ) ) ) )
    {
        return( list( TreatmentID = as.integer( rep( 0, NumSub ) ), ErrorCode = as.integer( -4 ) ) )
    }

    # 5. The sum of block sizes must equal the total number of subjects
    if( sum( unlist( UserParam ) ) != NumSub )
    {
        return( list( TreatmentID = as.integer( rep( 0, NumSub ) ), ErrorCode = as.integer( -5 ) ) )
    }

    # 6. Each block size must be a multiple of the sum of the allocation ratio
    if( any( unlist( UserParam ) %% sum( vAllocRatio ) != 0 ) )
    {
        return( list( TreatmentID = as.integer( rep( 0, NumSub ) ), ErrorCode = as.integer( -6 ) ) )
    }

    # Treatment ID assignment
    vBlockSize  <- as.integer( unlist( UserParam, use.names = FALSE ) )

    cPar        <- randomizeR::pbrPar( bc = as.integer( vBlockSize ), K = NumArms, ratio =  vAllocRatio, groups = c( "0", "1" ) )
    cObject     <- randomizeR::genSeq( cPar, r = 1 )
    vReturn     <- as.numeric( randomizeR::getRandList( cObject ) )

    return( list( TreatmentID = as.integer( vReturn ), ErrorCode = as.integer( nError ) ) )
}

########## Auxilliary function ################

# Convert a numeric treatment-to-control ratio (n_t / n_c) into the smallest integer allocation vector:
# c(n_c, n_t)
ConvertRatio <- function( AllocRatio, Tolerance = 1e-8, MaxDenominator = 100 )
{
    dFraction    <- FractionApproximation( AllocRatio, Tolerance, MaxDenominator )
    nNumerator   <- unname( dFraction[ "nNumerator" ] )    # treatment
    nDenominator <- unname( dFraction[ "nDenominator" ] )  # control

    return ( c( as.integer( nDenominator ), as.integer( nNumerator ) ) )
}

# Approximate a positive real number x by a fraction p/q using continued fraction expansion
# The algorithm iteratively decomposes AllocRatio into: a0 + 1 / (a1 + 1 / (a2 + ...))
FractionApproximation <- function( AllocRatio, Tolerance = 1e-8, MaxDenominator = 100 )
{
    nRoundedDown <- floor( AllocRatio )

    if( abs( AllocRatio - nRoundedDown ) < Tolerance )
    {
        return( c( nNumerator = nRoundedDown, nDenominator = 1 ) )
    }

    nNumerator0   <- 1
    nDenominator0 <- 0
    nNumerator1   <- nRoundedDown
    nDenominator1 <- 1

    dValue <- AllocRatio

    while( TRUE ) {
        dFractionPart <- dValue - floor( dValue ) # decimal places
        if( dFractionPart == 0 ) break   # if integer -> break

        dValue <- 1 / dFractionPart #update as we are now dealing with the inverse of the fraction part (and will do for the other iterations of the while loop if needed)
        dRoundedDownValue <- floor ( dValue )

        nNumerator2   <- dRoundedDownValue * nNumerator1 + nNumerator0
        nDenominator2 <- dRoundedDownValue * nDenominator1 + nDenominator0

        # If denominator is above the set threshold -> return the values of the latest convergence
        if( nDenominator2 > MaxDenominator ) break

        # If the fraction is a close approximation of the AllocRatio -> return the values of the latest convergence
        if( abs( AllocRatio - ( nNumerator2 / nDenominator2 ) ) < Tolerance )
        {
            nNumerator1   <- nNumerator2
            nDenominator1 <- nDenominator2
            break
        }

        nNumerator0   <- nNumerator1
        nDenominator0 <- nDenominator1

        nNumerator1   <- nNumerator2
        nDenominator1 <- nDenominator2
    }

    return( c( nNumerator = nNumerator1, nDenominator = nDenominator1 ) )
}
