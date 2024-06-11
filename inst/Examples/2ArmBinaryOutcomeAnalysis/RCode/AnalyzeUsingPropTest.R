######################################################################################################################## .
#' @param AnalyzeUsingPropTest
#' @title Analyze using the prop.test function in base R.
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East. The default must be NULL.
#' @description This example utilizes the prop.test function in base R to perform the analysis. The p-value from prop.test is used to compute the Z statistic that is compared to the upper boundary computed and sent by East as an input.  
#'              This example does NOT include a futility rule. 
#'              
#' @return TestStat A double value of the computed test statistic
#' @return Decision An integer value: Decision = 0 --> No boundary crossed
#'                                    Decision = 1 --> Lower Efficacy Boundary Crossed
#'                                    Decision = 2 --> Upper Efficacy Boundary Crossed
#'                                    Decision = 3 --> Futility Boundary Crossed
#'                                    Decision = 4 --> Equivalence Boundary Crossed
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
AnalyzeUsingPropTest<- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL)
{

    # Step 1: Retrieve necessary information from the objects East sent ####
    if(  !is.null( LookInfo )  )
    {
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
    }
    else
    {
        nQtyOfLooks          <- 1
        nLookIndex           <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
    }
    
    # Create the vector of simulated data for this IA - East sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Create vectors of data for each treatment 
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]
    
    # Perform the desired analysis 
    mData                <- cbind(table(vOutcomesS), table(vOutcomesE))
    lAnalysisResult      <- prop.test(mData, alternative = "greater", correct = FALSE)
    dPValue              <- lAnalysisResult$p.value
    dZValue              <- qnorm( 1 - dPValue )
    nDecision            <- ifelse( dZValue > LookInfo$EffBdryUpper[ nLookIndex], 2, 0 )  # A decision of 2 means success, 0 means continue the trial
    
    if( nDecision == 0 )
    {
        # if needed, check futility
        
        
        
    }
    
    Error 	= 0
    
    
    return(list(TestStat = as.double(dZValue), ErrorCode = as.integer(Error), Decision = as.integer( nDecision ) ))
}


