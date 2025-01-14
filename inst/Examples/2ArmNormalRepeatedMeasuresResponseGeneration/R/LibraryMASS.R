# This library is used to run mvrnorm() function in R for generating Normal responses in Repeated measures.

LoadMass <- function( Seed )
{   
  Error = 0
  set.seed( Seed )
  library( MASS )
  return( as.integer( Error ) )
  
}