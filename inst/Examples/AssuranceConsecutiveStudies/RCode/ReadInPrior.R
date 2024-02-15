

ReadExample4Phase2Output <- function(Seed, UserParam = NULL)
{
    library(survival)
    #library( dplyr )
    # TO DO : Modify this function appropriately
    #setwd( )
    Error <- 0
    
    dfPost     <- read.csv( "c:/AssuranceNormal/EastOutput/Example4/Phase2.csv",sep = ",", header=TRUE)
    vPrior     <<- dfPost[ dfPost$BdryStopCode == 2,]$dTrueDelta
    nSimIndex  <<- 1
   
    return(as.integer(Error))
}


ReadExample5Phase2Output <- function(Seed, UserParam = NULL )
{
    library(survival)
    #library( dplyr )
    # TO DO : Modify this function appropriately
    #setwd( )
    setwd( "C:\\AssuranceNormal\\ExampleArgumentsFromEast\\Example5" )
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

######################################################################################################################## .
# DO NOT NEED WHAT IS BELOW ####
######################################################################################################################## .

ReadInPrior <- function(Seed)
{
    #library( dplyr )
    # TO DO : Modify this function appropriately
    setwd( "C:\\Kyle\\Cytel\\Solara\\GSK\\Assurance\\Output\\")
    Error = 0
    
    mPost           <- read.table( "Ex4Ph2Post.csv",sep = ",")
    vUnif           <- runif( 100000, 0, 1 )
    vIndices        <- findInterval(vUnif, mPost[, 1], rightmost.closed = TRUE)
    vPh2PostSamples <- mPost[vIndices, 2]
    vPh3Prior <<- vPh2PostSamples
    write.table( vPh3Prior, "Ph3TrueDelta.csv", sep = ",")
    return(as.integer(Error))
}


ReadInPriorSamples <- function(Seed)
{
    #library( dplyr )
    # TO DO : Modify this function appropriately
    setwd( "C:\\Kyle\\Cytel\\Solara\\GSK\\Assurance\\Output\\")
    Error = 0
    
    mPost           <- read.table( "Ph2TrueDeltaGivenSuccess.csv",sep = ",")

    vPh3Prior <<- mPost[,1]
#    write.table( vPh3Prior, "Ph3TrueDelta.csv", sep = ",")
    return(as.integer(Error))
}


ReadInPriorSamplesForHR <- function(Seed)
{
    #library( dplyr )
    # TO DO : Modify this function appropriately
    setwd( "C:\\Kyle\\Cytel\\Solara\\GSK\\Assurance\\Output\\")
    Error = 0
    
    mPost           <- read.table( "TrueHRPriorForSolaraEx5.csv",sep = ",")
    
    vPh3PriorHR <<- mPost[,1]
    #    write.table( vPh3Prior, "Ph3TrueDelta.csv", sep = ",")
    return(as.integer(Error))
}