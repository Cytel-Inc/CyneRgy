
AnalzyeTTEWithConditinalPowerFutility <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
  nError 	        <- 0
  nDecision 	    <- 0
  dTestStatistic  <- 0
  nLookIndex      <- 1 
  dEstimatedHR    <- 1
  #library(survMisc)
  
  if( !is.null( LookInfo ) )
  {
    nQtyOfLooks  <- LookInfo$NumLooks
    nLookIndex   <- LookInfo$CurrLookIndex
    CumEvents    <- LookInfo$InfoFrac * DesignParam$MaxEvents
    nQtyOfEvents <- CumEvents[nLookIndex]
  }
  else
  {
    nQtyOfLooks  <- 1
    nLookIndex   <- 1
    nQtyOfEvents <- DesignParam$MaxEvents 
  }
  
  SimData$TimeOfEvent  <- SimData$ArrivalTime + SimData$SurvivalTime    
  SimData              <- SimData[order(SimData$TimeOfEvent), ]
  dTimeOfAnalysis      <- SimData[nQtyOfEvents, ]$TimeOfEvent
  SimData              <- SimData[SimData$ArrivalTime <= dTimeOfAnalysis, ]
  SimData$Event        <- ifelse(SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1)
  SimData$ObservedTime <- ifelse(SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime)
  
  # Perform logrank test
  logrankTest <- survival::survdiff(survival::Surv(ObservedTime, Event) ~ TreatmentID, data = SimData)
  dTestStatistic <- sqrt(logrankTest$chisq) * sign(logrankTest$obs[2] - logrankTest$exp[2])
  dPValue <- 1 - pchisq(logrankTest$chisq, df = 1)
  
  # Retrieve user-defined parameters
  dTargetHR            <- UserParam$TargetHazardRatio
  dFutilityThreshold   <- UserParam$FutilityThreshold
  
  # UserParam$nComputationOption 
  # Options for computing conditional power:
  # 1: Compute CP using UserParam$TargetHazardRatio
  # 2: Compute CP using the observed HR
  # 3: Weighted: You must supply dWeightObserved, dWeightTarget and TargetHazardRatio
  
  library( gsDesign )
  library( CyneRgy )
  
  eff_bdry<-qnorm(0.025)
  
  nDecision <- 0 # Set a default value 
  if( nLookIndex < nQtyOfLooks )
  {
    if( UserParam$nComputationOption == 1 )
    {
      # Option 1: Compute CP using UserParam$TargetHazardRatio
      
      r <- DesignParam$AllocInfo / (1 + DesignParam$AllocInfo )
      se_HR <- 1 / sqrt( LookInfo$CumCompleters[1] * r * (1-r) )
      dConditionalPower <- pnorm(
                                  eff_bdry*sqrt(1+LookInfo$CumCompleters[1]/(LookInfo$CumCompleters[2]-LookInfo$CumCompleters[1]))-
                                  dTestStatistic*sqrt(LookInfo$CumCompleters[1]/(LookInfo$CumCompleters[2]-LookInfo$CumCompleters[1]))-
                                  log(dTargetHR)*sqrt(r*(1-r))*sqrt(LookInfo$CumCompleters[2]-LookInfo$CumCompleters[1])   
                                )
    }
    else if( UserParam$nComputationOption == 2 )
    {
      # Option 2: Compute CP using the observed HR
      
      r <- DesignParam$AllocInfo / (1 + DesignParam$AllocInfo )
      se_HR <- 1 / sqrt( LookInfo$CumCompleters[1] * r * (1-r) )
      dEstimatedHR <- exp( dTestStatistic*se_HR )
      dConditionalPower <- pnorm(
                                  eff_bdry*sqrt(1+LookInfo$CumCompleters[1]/(LookInfo$CumCompleters[2]-LookInfo$CumCompleters[1]))-
                                  dTestStatistic*sqrt(LookInfo$CumCompleters[1]/(LookInfo$CumCompleters[2]-LookInfo$CumCompleters[1]))-
                                  log(dEstimatedHR)*sqrt(r*(1-r))*sqrt(LookInfo$CumCompleters[2]-LookInfo$CumCompleters[1])   
                                )
    }
    else if( UserParam$nComputationOption == 3 )
    {
      
      dWeightEstimated     <- UserParam$WeightEstimatedHR
      dWeightTarget        <- UserParam$WeightTargetHR
      
      # Option 3: Weighted computation
      
      r <- DesignParam$AllocInfo / (1 + DesignParam$AllocInfo )
      se_HR <- 1 / sqrt( LookInfo$CumCompleters[1] * r * (1-r) )
      dEstimatedHR <- exp( dTestStatistic*se_HR )
      dWeightedHR <- (dWeightEstimated * dEstimatedHR) + (dWeightTarget * dTargetHR)
      dConditionalPower <- pnorm(
                                  eff_bdry*sqrt(1+LookInfo$CumCompleters[1]/(LookInfo$CumCompleters[2]-LookInfo$CumCompleters[1]))-
                                  dTestStatistic*sqrt(LookInfo$CumCompleters[1]/(LookInfo$CumCompleters[2]-LookInfo$CumCompleters[1]))-
                                  log(dTargetHR)*sqrt(r*(1-r))*sqrt(LookInfo$CumCompleters[2]-LookInfo$CumCompleters[1])   
                                ) 
    }
    else
    {
      throw( new( "InvalidParameterError", 
                  message = "Invalid computation option specified in UserParam$nComputationOption" ) )
    }
    
    
    
    # Make futility decision based on conditional power
    if (dConditionalPower < dFutilityThreshold)
    {
      nDecision <- 3  # Futility boundary crossed
    }
  }
  
  else if( nLookIndex == nQtyOfLooks)
  {
    # Check efficacy
    dConditionalPower  <- -1
    if (dTestStatistic <= eff_bdry) 
    {
      # Efficacy boundary crossed
      strDecision <- GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, bIAEfficacyCondition = FALSE, bIAFutilityCondition = FALSE, 
                                        bFAEfficacyCondition = TRUE, bFAFutilityCondition = FALSE )
    } 
    else 
    {
      # Final analysis: if efficacy not achieved, declare futility
      strDecision <- GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, bIAEfficacyCondition = FALSE, bIAFutilityCondition = FALSE, 
                                        bFAEfficacyCondition = FALSE, bFAFutilityCondition = TRUE )
    }
    nDecision <- GetDecision( strDecision, DesignParam, LookInfo )
  }
  
  lRet <- list(TestStat = as.double(dTestStatistic),
               HR = as.double(dEstimatedHR),
               Decision  = as.integer(nDecision), 
               ErrorCode = as.integer(nError),
               dConditionalPower = as.double(dConditionalPower))
  return(lRet)
}


