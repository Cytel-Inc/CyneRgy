# Template for Initialization function
Initialize <- function(Seed)
{
    # TO DO : Modify this function appropriately
    
    Error <- 0
    set.seed(Seed)
    library( survival )
    # User may use other options in set.seed like setting 
    # the Random Number Generator
    # User may also initialize Global Variables or set up 
    # the working directory etc. 
    # Do the error handling Modify Error appropriately 
    
    return(as.integer(Error))
}
