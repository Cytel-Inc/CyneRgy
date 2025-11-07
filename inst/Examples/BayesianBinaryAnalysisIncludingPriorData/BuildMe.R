source( "R/RunAnalysis.BetaBinom.R")
source( "R/RunAnalysis.HBayes.R")

# Example function call
SimData <- data.frame(Response = c( rbinom(150,1,0.2), rbinom(150,2,0.4)), TreatmentID = c(rep(0,150), rep(1,150)) )
SimData <- SimData[ sample( 1:150), ]

DesignParam <- list(MaxCompleters = 300, AllocInfo = c(0.5, 0.5))
LookInfo <- list(CurrLookIndex = 1, NumLooks = 2, CumCompleters = c(150, 300))
UserParam <- list(
    dAlphaCtrl = 0.2, dBetaCtrl = 0.9, dAlphaExp = 1, dBetaExp = 1,
    PU = 0.95, PUFinal = 0.975, PL = 0.1, nSimulations = 1000
)

PerformBayesianAnalysis(SimData, DesignParam, LookInfo, UserParam)

SimData <- data.frame(Response = c( rbinom(150,1,0.2), rbinom(150,2,0.4)), TreatmentID = c(rep(0,150), rep(1,150)) )
SimData <- SimData[ sample( 1:150), ]



source( "R/RunAnalysis.BetaBinom.R")
source( "R/RunAnalysis.HBayes.R")
SimData <- TestFailed1$SimData
UserParam <- TestFailed1$UserParam
LookInfo <- TestFailed1$LookInfo
DesignParam <- TestFailed1$DesignParam

DesignParam$SampleSize <- 210
DesignParam$MaxCompleters <- 210
LookInfo$CumCompleters <- c(105, 210)
UserParam$FutilityCheck <- 1

nSuccess <- 0
nEarly <- 0
nFutility <- 0
nFutilityEarly<-0

for( i in 1:5000 )
{
    SimData <- data.frame(Response = c( rbinom(105,1,0.2), rbinom(105,1,0.4)), TreatmentID = c(rep(0,105), rep(1,105)) )
    SimData <- SimData[ sample( 1:210), ]
    LookInfo$CurrLookIndex <-1
    lIA <- PerformBayesianAnalysis(SimData, DesignParam, LookInfo, UserParam)
    lIA <- RunAnalysis.HBayes( SimData, DesignParam, LookInfo, UserParam )
    if( lIA$strDecision == "Efficacy" )
    {
        nSuccess <- nSuccess + 1
        nEarly   <- nEarly + 1
    }
    else if(  lIA$strDecision == "Futility")
    {
        nFutility <- nFutility + 1
        nFutilityEarly <- nFutilityEarly+1
    }
    else if( lIA$Decision == 0 )
    {
        LookInfo$CurrLookIndex <-2
        lFA <- PerformBayesianAnalysis(SimData, DesignParam, LookInfo, UserParam)
        
        if( lFA$strDecision == "Efficacy" )
        {
            nSuccess <- nSuccess + 1
        }
        else
        {
            nFutility <- nFutility + 1
        }
    }
    
}
nSuccess
nEarly 
nFutility
nFutilityEarly

nSuccess/5000
