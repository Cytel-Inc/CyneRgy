LoadRM <- function( Seed )
{   
    Error = 0
    set.seed( Seed )
    library( nlme )
    library( stats )
    library(rpact)
    return( as.integer( Error ) )
}