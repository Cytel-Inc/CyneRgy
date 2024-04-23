## For example -2 of 2-Arm Normal analysis, computation of Test statistic using Z.test requires installation
# of package named "BSDA". Hence, we need to call this function via "Initialize R Environment"


load_BSDA <- function(Seed)
{   
    Error = 0
    set.seed(Seed)
    library(BSDA)
    return(as.integer(Error))

}