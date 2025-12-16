######################################################################################################################## .
#' @param AnalyzeUsingEastManualFormula
#' @title Compute the statistic using formula 28.2 in the East manual.
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo A list containing input parameters related to multiple looks, which the user may need to compute 
#'                 test statistics and perform tests. Users should access the variables using their names 
#'                 (e.g., `LookInfo$NumLooks`) rather than by their order. Important variables in group sequential designs include:
#'                 
#'                 - `LookInfo$NumLooks`: An integer representing the number of looks in the study.
#'                 - `LookInfo$CurrLookIndex`: An integer representing the current index look, starting from 1.
#'                 - `LookInfo$CumEvents`: A vector of length `LookInfo$NumLooks`, containing the cumulative number of events at each look.
#'                 - `LookInfo$RejType`: A code representing rejection types. Possible values are:
#'                   - **Efficacy Only:**
#'                     - `0`: 1-Sided Efficacy Upper.
#'                     - `2`: 1-Sided Efficacy Lower.
#'                   - **Futility Only:**
#'                     - `1`: 1-Sided Futility Upper.
#'                     - `3`: 1-Sided Futility Lower.
#'                   - **Efficacy and Futility:**
#'                     - `4`: 1-Sided Efficacy Upper and Futility Lower.
#'                     - `5`: 1-Sided Efficacy Lower and Futility Upper.
#' @param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL. For this example, user defined parameters are not included. 
#' @description Use the formula 28.2 in the East manual to compute the statistic. The purpose of this example is to demonstrate how the analysis and decision making can be modified in a simple approach.  
#'              The test statistic is compared to the upper boundary computed and sent by East as an input. This example does NOT include a futility rule. 
#' @return TestStat A double value of the computed test statistic
#' @return Decision An integer value: Decision = 0 --> No boundary crossed
#'                                    Decision = 1 --> Lower Efficacy Boundary Crossed
#'                                    Decision = 2 --> Upper Efficacy Boundary Crossed
#'                                    Decision = 3 --> Futility Boundary Crossed
#'                                    Decision = 4 --> Equivalence Boundary Crossed
#' @return AnalysisTime Optional Numeric value to be computed and returned by the user
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

AnalyzeUsingEastManualFormula<- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL)
{
    library(CyneRgy)
    
    # Step 1: Retrieve necessary information from the objects East sent. You may not need all the variables ####
    if(  !is.null( LookInfo )  )
    {
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfLooks          <- LookInfo$NumLooks
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        RejType              <- LookInfo$RejType
        TailType             <- DesignParam$TailType
    }
    else
    {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
        TailType             <- DesignParam$TailType
    }
    
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
    dBoundary            <- ifelse( is.null( LookInfo ), DesignParam$CriticalPoint, LookInfo$EffBdryUpper[ nLookIndex])
    
    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, 
                                               bIAEfficacyCondition = dZj > dBoundary, 
                                               bFAEfficacyCondition = dZj > dBoundary)
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    
    Error <-  0
    
    
    return(list(TestStat = as.double(dZj), ErrorCode = as.integer(Error), Decision = as.integer( nDecision ), Delta = as.double( dPiHatExperimental - dPiHatControl ) ))
}
