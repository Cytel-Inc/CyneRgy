## For example - 1 of 2-Arm Time To Event (TTE) analysis, computation of Hazard Ratio requires installation
# of package named "survival". Hence, we need to call this function via "Initialize R Environment"


Loadsurvival <- function( Seed )
{   
    Error = 0
    set.seed( Seed )
    library( survival )
    return( as.integer( Error ) )
    
}