######################################################################################################################## .
#  Last Modified Date: 05/03/2024
#' @param AnalyzeUsingSurvivalPackage
#' @title Compute the statistic using survival package
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL.
#' @description Use the survival package to compute the statistic. The purpose of this example is to demonstrate how the analysis and decision making can be modified in a simple approach.  
#'              The test statistic is compared to the lower boundary computed and sent by East as an input. This example does NOT include a futility rule. 
#' @return TestStat A double value of the computed test statistic
#' @return Decision An integer value: Decision = 0 --> No boundary crossed
#'                                    Decision = 1 --> Lower Efficacy Boundary Crossed
#'                                    Decision = 2 --> Upper Efficacy Boundary Crossed
#'                                    Decision = 3 --> Futility Boundary Crossed
#'                                    Decision = 4 --> Equivalence Boundary Crossed
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Nonfatal error, current simulation is aborted but the next simulations will run
#                                       ErrorCode < 0 --> Fatal error, no further simulation will be attempted

#'@note Helpful Hints:
#'       There is often info that East sends to R that are not shown in a given example.  It can be very helpful to save the input 
#'       objects and then load them into your R session and inspect them.  This can be done with the following R code in your function.
#'
#'       saveRDS( SimData,     "SimData.Rds")
#'       saveRDS( DesignParam, "DesignParam.Rds" )
#'       saveRDS( LookInfo,    "LookInfo.Rds" )
#'
#'       The above code will save each of the input objects to a file so they may be examined within R.
######################################################################################################################## .

AnalyzeUsingSurvivalPackage <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    library(CyneRgy)
    library(survival)

    # Step 1: Retrieve necessary information from the objects East sent. You may not need all the variables ####
    if( !is.null( LookInfo ) )
    {
        # Look info was provided so use it
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        CumEvents            <- LookInfo$InfoFrac*DesignParam$MaxEvents
        nQtyOfEvents         <- CumEvents[ nLookIndex ]
        dEffBdry             <- LookInfo$EffBdryLower[ nLookIndex ]
        RejType              <- LookInfo$RejType
        TailType             <- DesignParam$TailType
    }
    else
    {   # Look info is not provided for fixed sample designs so fetch the information appropriately
        nQtyOfLooks          <- 1
        nLookIndex           <- 1
        nQtyOfEvents         <- DesignParam$MaxEvents
        dEffBdry             <- DesignParam$CriticalPoint
        TailType             <- DesignParam$TailType
    }
    
    SimData$TimeOfEvent      <- SimData$ArrivalTime + SimData$SurvivalTime    # This is the calendar time in the trial that the patients event is observed
    
    # Compute the time of analysis 
    SimData                  <- SimData[ order( SimData$TimeOfEvent), ]
    dTimeOfAnalysis          <- SimData[ nQtyOfEvents, ]$TimeOfEvent
    
    # Add the Observed Time variable 
    SimData                  <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis ,]   # Exclude any patients that were not enrolled by the time of the analysis
    SimData$Event            <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )  # If the event is observed after the analysis it is not observed, eg censored 
    SimData$ObservedTime     <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )
    
    # Order the data by observed time for the remainder of the computations
    SimData                  <- SimData[ order( SimData$ObservedTime), ]
    
    # Compute Observed HR
    coxModel                 <- coxph(Surv(ObservedTime, Event) ~ TreatmentID, data = SimData)
    dTrueHR                  <- exp(coxModel$coefficients)
    
    # Compute the test statistic using survival package
    logrankTest              <- survdiff(Surv(ObservedTime, Event) ~ TreatmentID, SimData)
    
    # Compute the logrank test statistic
    dTS                      <- sqrt(logrankTest$chisq) * sign(logrankTest$obs[2] - logrankTest$exp[2])
    
    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, 
                                               bIAEfficacyCondition = dTS <  dEffBdry, 
                                               bFAEfficacyCondition = dTS <  dEffBdry)
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    
    Error                    <- 0
    
    lRet                     <- list(TestStat = as.double( dTS ),
                                     Decision  = as.integer( nDecision ),
                                     ErrorCode = as.integer( Error ),
                                     HazardRatio = as.double( dTrueHR ))
    return( lRet )
}