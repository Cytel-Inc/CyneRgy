# AnalyzeSurvivalDataUsingCoxPH <- function(SimData, DesignParam, LookInfo, UserParam = NULL )
# Function Template for computing Test Statistic for One Look Tests
AnalyzeSurvivalDataUsingCoxPH <- function(SimData, DesignParam, UserParam = NULL )
{
    # TO DO : Modify this account for drop-out - Ignoring it for the first pass
    setwd( "C:\\AssuranceNormal\\ExampleArgumentsFromEast\\Example5")
    # #setwd( "[ENTERED THE DESIRED LOCATION TO SAVE THE FILE]" )
    saveRDS( SimData, "SimData.Rds")
    saveRDS( DesignParam, "DesignParam.Rds" )
   # saveRDS( LookInfo, "LookInfo.Rds" )
    # saveRDS( SurvMethod, "SurvMethod.Rds" )
    # saveRDS( NumPrd, "NumPrd.Rds" )
    # saveRDS( SurvParam, "SurvParam.Rds" )
     saveRDS( UserParam, "UserParam.Rds" )
    
    nLookIndex           <- 1 #LookInfo$CurrLookIndex
    nQtyOfEvents         <- 300 #LookInfo$CumEvents[ nLookIndex ]
    
    SimData$TimeOfEvent  <- SimData$ArrivalTime + SimData$SurvivalTime    # This is the calendar time in the trial that the patients event is observed
    
    # compute the time of analysis 
    SimData              <- SimData[ order( SimData$TimeOfEvent), ]
    dTimeOfAnalysis      <- SimData[ nQtyOfEvents, ]$TimeOfEvent
    
    # Add the Observed Time variable 
    SimData              <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis ,]   # Exclude any patients that were not enrolled by the time of the analysis
    SimData$Event        <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )  # If the event is observed after the analysis it is not observed, eg censored 
    
    SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )
    
    
    # Fit a cox model 
    fitCox  <- coxph( Surv( ObservedTime, Event ) ~ as.factor( TreatmentID ), data = SimData )
    #fit     <- survdiff(Surv(ObservedTime, Event ) ~ as.factor(TreatmentID), data = SimData)
    #dPValue <- fit$pvalue #
    dPValue <- summary(fitCox)$coefficients[,"Pr(>|z|)"]
    dZVal   <- summary(fitCox)$coefficients[,"z"]
    dPValue <- pnorm( dZVal, lower.tail = TRUE)
    nDecision <- ifelse( dPValue <= 0.025, 1, 0 ) #LookInfo$EffBdryLower[ nLookIndex], 1, 0 )
    
    # if( !file.exists( paste0( "SimData", nLookIndex, ".Rds") ))
    # {
    #     saveRDS( SimData, paste0( "SimData", nLookIndex, ".Rds") )
    #     saveRDS( DesignParam, paste0( "DesignParam", nLookIndex, ".Rds") )
    #     saveRDS( LookInfo, paste0( "LookInfo", nLookIndex, ".Rds") )
    # }
   # Error <- ERROR1
    Error 	= 0
    #retval 	= 0
    
    
    return(list(TestStat = as.double(dZVal), ErrorCode = as.integer(Error), Decision = as.integer( nDecision ), TrueHR = as.double( SimData$TrueHR[ 1 ] ) ))
}
