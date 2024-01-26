
######################################################################################################################## .
#' @param AnalyzeUsingEastManualFormula
#' @title Compute the statistic using formula 28.2 in the East manual.
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East. The default must be NULL. For this example, user defined parameters are not included. 
#' @description Use the formula 28.2 in the East manual to compute the statistic.  The purpose of this example is to demonstrate how the analysis and decision making can be modified in a simple approach.  
#'              The test statistic is compared to the upper boundary computed and sent by East as an input. This example does NOT include a futility rule. 
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
#' @export
######################################################################################################################## .

AnalyzeUsingEastManualFormula<- function(SimData, DesignParam, LookInfo, UserParam = NULL)
{
    
    # Input objects can be saved through the following lines:
    
    #setwd( "[ENTER THE DIRECTORY WHERE YOU WANT TO SAVE DATA]")
    #saveRDS( SimData, "SimData.Rds")
    #saveRDS( DesignParam, "DesignParam.Rds" )
    #saveRDS( LookInfo, "LookInfo.Rds" )
    
    
    # Retrieve necessary information from the objects East sent
    nLookIndex           <- LookInfo$CurrLookIndex
    nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
    nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
    
    # Create the vector of simulated data for this IA - East sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Create vectors of data for each treatment - E is Experimental and S is Standard of Care 
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]
    
    nQtyOfResponsesOnE   <- sum( vOutcomesE )
    nQtyOfPatsOnE        <- length( vOutcomesE )
    
    nQtyOfResponsesOnS   <- sum( vOutcomesS )
    nQtyOfPatsOnS        <- length( vOutcomesS )
    
    # Compute the estimates in equation 28.2 from the East user manual
    dPiHatExperimental   <- nQtyOfResponsesOnE/nQtyOfPatsOnE
    dPiHatControl        <- nQtyOfResponsesOnS/nQtyOfPatsOnS
    
    dPiHatj              <- ( nQtyOfResponsesOnE +  nQtyOfResponsesOnS )/( nQtyOfPatsOnE + nQtyOfPatsOnS )
    
    # Equation 28.2 in East manual
    dZj                  <- ( dPiHatExperimental - dPiHatControl )/sqrt( dPiHatj*( 1- dPiHatj ) * ( 1/nQtyOfPatsOnE + 1/nQtyOfPatsOnS)  ) 
    
    # A decision of 2 means success, 0 means continue the trial
    nDecision            <- ifelse( dZj > LookInfo$EffBdryUpper[ nLookIndex], 2, 0 )  
    
    if( nDecision == 0 )
    {
        # For this example, there is NO futility check but this is left for consistency with other examples 
        
    }
    
    Error <-  0
    
    
    return(list(TestStat = as.double(dZj), ErrorCode = as.integer(Error), Decision = as.integer( nDecision ) ))
}
