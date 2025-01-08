

ReadExample5Phase2Output <- function(Seed, UserParam = NULL )
{
    library(survival)

    #setwd( "C:\\AssuranceNormal\\ExampleArgumentsFromEast\\Example5" )
    saveRDS( UserParam, "UserParam.Rds")
    
    Error <-  0
    
    bConditionalOnGo <- UserParam$bConditionalOnGo
    
    dfPost     <- read.csv( "c:/AssuranceNormal/EastOutput/Example5/Phase2.csv",sep = ",", header=TRUE)
    if( bConditionalOnGo == 1 )
        vPrior     <<- dfPost[ dfPost$BdryStopCode == 2,]$dTrueDelta
    else
        
        vPrior     <<- dfPost$dTrueDelta
    
    saveRDS( vPrior, "vPrior.Rds")
    nSimIndex  <<- 1
    
    return(as.integer(Error))
}
