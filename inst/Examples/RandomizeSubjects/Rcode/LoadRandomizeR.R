# For Performing Block randomization in R, a package named "randomizeR" is required to be installed.

LoadRandomizeR <- function( Seed )
{   
    Error = 0
    set.seed( Seed )
    library( randomizeR )
    return( as.integer( Error ) )
    
}