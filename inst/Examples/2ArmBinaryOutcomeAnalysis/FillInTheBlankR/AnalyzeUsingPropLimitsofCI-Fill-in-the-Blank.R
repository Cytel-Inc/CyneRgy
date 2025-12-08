######################################################################################################################## .
#' @param AnalyzeUsingPropLimitsOfCI
#' @title Analyze using the prop.test function in base R.
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East. The default must be NULL.
#'                  If UserParam is supplied, the list must contain the following named elements:
#'                  UserParam$dLowerLimit - A value (0,1) that specifes the lower limit for the confidence interval. 
#'                  UserParam$dUpperLimit - A value (0,1) that specifies the upper limit for the confidence interval.
#'                  UserParam$dConfLevel - A value (0,1) that specifies the confidence level for the prop.test function in base R.
#' @description This example utilizes the prop.test function in base R to perform the analysis. The p-value from prop.test is used to compute the Z statistic that is compared to the upper boundary computed and sent by East as an input.  
#'              This example does NOT include a futility rule. 
#'              
#' @return TestStat A double value of the computed test statistic
#' @return Decision An integer value: Decision = 0 --> No boundary crossed
#'                                    Decision = 1 --> Lower Efficacy Boundary Crossed
#'                                    Decision = 2 --> Upper Efficacy Boundary Crossed
#'                                    Decision = 3 --> Futility Boundary Crossed
#'                                    Decision = 4 --> Equivalence Boundary Crossed
#' @return AnalysisTime Optional Numeric value to be computed and returned by the user
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#                                       ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
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
AnalyzeUsingPropLimitsOfCI<- function(SimData, DesignParam, LookInfo, UserParam = NULL)
{
    
    
    # In this example, the majority of the code is provided.  The fill in the blank areas are noted by _____________________.
    # This is done to allow you to practice creating these examples. You will need to remove the ____________ and enter the correct code.
    # The fully worked examples are provided in the corresponding example R files. 
    
    if( is.null( UserParam ) )
    {
        UserParam <- list(UserParam$dLowerLimit = 0.1, UserParam$dConfLevel = 0.8, UserParam$dUpperLimit = 0.2)
    }
    
    # Retrieve necessary information from the objects East sent
    nQtyOfLooks          <- LookInfo$NumLooks
    nLookIndex           <- LookInfo$CurrLookIndex
    nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
    
    # Input objects can be saved through the following lines:
    
    #setwd( "[ENTER THE DIRECTORY WHERE YOU WANT TO SAVE DATA]")
    #saveRDS( SimData, "SimData.Rds")
    #saveRDS( DesignParam, "DesignParam.Rds" )
    #saveRDS( LookInfo, "LookInfo.Rds" )
    
    
    
    
    
    nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
    
    # Create the vector of simulated data for this IA - East sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Create vectors of data for each treatment 
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]
    
    # Perform the desired analysis, then determine if the lower limit of the confidence interval is greater than the user-specified value
    mData                <- cbind(table(_______), table(vOutcomesE))
    lAnalysisResult      <- prop.test(mData, alternative = "two.sided", correct = FALSE, conf.level = UserParam$dConfLevel)
    dLowerLimitCI        <- lAnalysisResult$conf.int[ 1 ]
    # A decision of 2 means success, 0 means continue the trial
    nDecision            <- ifelse( dLowerLimitCI > ______, 2, 0 )  
    
    if( nDecision == 0 )
    {
        # Check futility 
        _______        <- lAnalysisResult$conf.int[ 2 ]
        
        # Did not hit a Go decision, so check No Go
        # We are at the FA, efficacy decision was not made yet so the decision is futility
        if( nLookIndex == nQtyOfLooks ) 
        {
            # The final analysis was reached and a Go decision could not be made, thus a No Go decision is made
            nDecision <- 3 # East code for futility 
        }
        # At the IA check the No Go since a Go decision was not made
        else if( dUpperLimitCI < UserParam$dUpperLimit )  
            _______ <- 3 # East code for futility 
        
    }
    
    Error 	= 0
    
    
    
    return(list(TestStat = as.double(dLowerLimitCI), ErrorCode = as.integer(Error), Decision = as.integer( nDecision ) ))
}

