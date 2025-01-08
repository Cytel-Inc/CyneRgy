# Template for Initialization function

Initialize <- function( Seed )
{
   
    nError <-  0
    set.seed(Seed)  # Note: Setting the seed here only impacts whatever is done in R.
    
    
    return(as.integer( nError ))
}

