#' @name AnalzyeTTEWithConditinalPowerFutility
#' @title Time-To-Event Weighted Conditional Power Futility Analysis
#' 
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL.
#' \describe{
#'   \item{nComputationOption}{Specifies method for conditional power:  
#'       1 = Target hazard ratio (using UserParam$TargetHazardRatio),  
#'       2 = Estimated hazard ratio,  
#'       3 = Weighted hazard ratio (must supply UserParam$WeightEstimatedHR, UserParam$WeightTargetHR and UserParam$TargetHazardRatio).}
#'   \item{FutilityThreshold}{Threshold below which futility is declared.}
#'   \item{TargetHazardRatio}{User-specified hazard ratio (used in options 1 and 3).}
#'   \item{WeightEstimatedHR}{Weight assigned to the estimated hazard ratio (used in option 3).}
#'   \item{WeightTargetHR}{Weight assigned to the target hazard ratio (used in option 3).}
#' }
#' 
#' @description
#' This function performs a time-to-event (TTE) analysis with conditional power and futility boundaries, extending East Horizon: Design functionality 
#' via a custom R script for the Analysis integration point. The analysis is based on the logrank test and supports three modes of computing 
#' conditional power: using a target hazard ratio, using the estimated hazard ratio, or using a weighted combination of the two.
#'              
#' @return A list containing:
#'   \describe{
#'     \item{TestStat}{Numeric. The logrank test statistic ($Z$).}
#'     \item{HR}{Numeric. The estimated hazard ratio at the current analysis.}
#'     \item{Decision}{Integer. Analysis decision code:
#'       0 = No boundary crossed,  
#'       1 = Lower efficacy boundary crossed,  
#'       2 = Upper efficacy boundary crossed,  
#'       3 = Futility boundary crossed,  
#'       4 = Equivalence boundary crossed.}
#'     \item{ErrorCode}{Integer. Error status:  
#'       0 = No error,  
#'       >0 = Nonfatal error (current simulation aborted, others continue),  
#'       <0 = Fatal error (all simulations aborted).}
#'     \item{dConditionalPower}{Numeric. The conditional power at the current analysis (–1 if not computed).}
#'   }
#' @export
#' 
#######################################################################################################################################################################################################################

AnalzyeTTEWithConditinalPowerFutility <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
  nError 	        <- 0
  nDecision 	    <- 0
  dTestStatistic    <- 0
  nLookIndex        <- 1 
  dEstimatedHR      <- 1
  
  if( !is.null( LookInfo ) )
  {
    nQtyOfLooks  <- LookInfo$NumLooks
    nLookIndex   <- LookInfo$CurrLookIndex
    vCumEvents   <- LookInfo$InfoFrac * DesignParam$MaxEvents
    nQtyOfEvents <- vCumEvents[nLookIndex]
  }
  else
  {
    nQtyOfLooks  <- 1
    nLookIndex   <- 1
    nQtyOfEvents <- DesignParam$MaxEvents 
  }
  
  SimData$TimeOfEvent  <- SimData$ArrivalTime + SimData$SurvivalTime    
  SimData              <- SimData[ order( SimData$TimeOfEvent ), ]
  dTimeOfAnalysis      <- SimData[ nQtyOfEvents, ]$TimeOfEvent
  SimData              <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ]
  SimData$Event        <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )
  SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )
  
  # Perform logrank test
  logrankTest <- survival::survdiff( survival::Surv( ObservedTime, Event ) ~ TreatmentID, data = SimData )
  dTestStatistic <- sqrt( logrankTest$chisq ) * sign( logrankTest$obs[ 2 ] - logrankTest$exp[ 2 ] )
  dPValue <- 1 - pchisq( logrankTest$chisq, df = 1 )
  
  # Retrieve user-defined parameters
  dTargetHR            <- UserParam$TargetHazardRatio
  dFutilityThreshold   <- UserParam$FutilityThreshold
  
  library( gsDesign )
  library( CyneRgy )
  
  dEffBdry < -qnorm( 0.025 )
  
  nDecision <- 0 # Set a default value 
  if( nLookIndex < nQtyOfLooks )
  {
    if( UserParam$nComputationOption == 1 )
    {
      # Option 1: Compute CP using UserParam$TargetHazardRatio
      
      r <- DesignParam$AllocInfo / ( 1 + DesignParam$AllocInfo )
      dSEHR <- 1 / sqrt( LookInfo$CumCompleters[ 1 ] * r * ( 1-r ) )
      dConditionalPower <- pnorm(
                                  dEffBdry * sqrt( 1 + LookInfo$CumCompleters[ 1 ] / ( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] ) ) -
                                  dTestStatistic * sqrt( LookInfo$CumCompleters[ 1 ] / ( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] ) ) -
                                  log( dTargetHR ) * sqrt( r * ( 1-r ) ) * sqrt( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] )   
                           )
    }
    else if( UserParam$nComputationOption == 2 )
    {
      # Option 2: Compute CP using the observed HR
      
      r <- DesignParam$AllocInfo / (1 + DesignParam$AllocInfo )
      dSEHR <- 1 / sqrt( LookInfo$CumCompleters[ 1 ] * r * ( 1-r ) )
      dEstimatedHR <- exp( dTestStatistic * dSEHR )
      dConditionalPower <- pnorm(
                                  dEffBdry * sqrt( 1 + LookInfo$CumCompleters[ 1 ] / ( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] ) ) -
                                  dTestStatistic * sqrt( LookInfo$CumCompleters[ 1 ] / ( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] ) ) -
                                  log( dEstimatedHR ) * sqrt( r * ( 1-r ) ) * sqrt( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] )   
                           )
    }
    else if( UserParam$nComputationOption == 3 )
    {
      
      dWeightEstimated     <- UserParam$WeightEstimatedHR
      dWeightTarget        <- UserParam$WeightTargetHR
      
      # Option 3: Weighted computation
      
      r <- DesignParam$AllocInfo / (1 + DesignParam$AllocInfo )
      dSEHR <- 1 / sqrt( LookInfo$CumCompleters[1] * r * (1-r) )
      dEstimatedHR <- exp( dTestStatistic*dSEHR )
      dWeightedHR <- (dWeightEstimated * dEstimatedHR) + (dWeightTarget * dTargetHR)
      dConditionalPower <- pnorm(
                                  dEffBdry * sqrt( 1 + LookInfo$CumCompleters[ 1 ] / ( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] ) ) -
                                  dTestStatistic * sqrt( LookInfo$CumCompleters[ 1 ] / ( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] ) ) -
                                  log( dWeightedHR ) * sqrt( r * ( 1-r ) ) * sqrt( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] )   
                           )
    }
    else
    {
      throw( new( "InvalidParameterError", 
                  message = "Invalid computation option specified in UserParam$nComputationOption" ) )
    }
    
    
      
    # Make futility decision based on conditional power
    if ( dConditionalPower < dFutilityThreshold )
    {
      nDecision <- 3  # Futility boundary crossed
    }
  }
  
  else if( nLookIndex == nQtyOfLooks )
  {
    # Check efficacy
    dConditionalPower  <- -1
    if ( dTestStatistic <= dEffBdry ) 
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
  
  lRet <- list( TestStat = as.double( dTestStatistic ),
                HR = as.double( dEstimatedHR ),
                Decision  = as.integer( nDecision ), 
                ErrorCode = as.integer( nError ),
                dConditionalPower = as.double( dConditionalPower ) )
  return( lRet )
}
