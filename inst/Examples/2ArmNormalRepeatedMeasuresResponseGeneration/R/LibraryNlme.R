# This library is used to run mvrnorm() function in R for generating Normal responses in Repeated measures.

LoadNlme <- function( Seed )
{   
  Error = 0
  set.seed( Seed )
  library( nlme )
  library( stats )
  return( as.integer( Error ) )
  
}