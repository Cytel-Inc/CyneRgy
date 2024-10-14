#  Last Modified Date: 04/18/2024
#' @name AnalyzeUsingEastManualFormula_Normal
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East. The default must be NULL.
#' 
#' @description Use the formula Q.3.3 in the East manual to compute the statistic.  The purpose of this example is to demonstrate how the analysis and decision making can be modified in a simple approach.  
#'              The test statistic is compared to the upper boundary computed and sent by East as an input. This example does NOT include a futility rule. 
#'              Two sample Z test for Normal distribution. Number of Looks > 1.
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
#' @export
#' 
#######################################################################################################################################################################################################################


AnalyzeUsingEastManualFormulaNormal <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    library(CyneRgy)
    
    # Retrieve necessary information from the objects East sent
    if(  !is.null( LookInfo )  )
    {
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfLooks          <- LookInfo$NumLooks
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
    }
    else
    {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
    }
    
    # Create the vector of simulated data for this IA - East sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Create vectors of data for each treatment - E is Experimental and S is Standard of Care 
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]
    
    # compute the estimates for mean and Std. Dev for (E and S)
    dMeanOfResponsesOnE   <- mean( vOutcomesE )
    dStdDevOfResponsesOnE <- sd( vOutcomesE)
    nQtyOfPatsOnE        <- length( vOutcomesE )
    
    dMeanOfResponsesOnS   <- mean( vOutcomesS )
    dStdDevOfResponsesOnS <- sd( vOutcomesS)
    nQtyOfPatsOnS        <- length( vOutcomesS )
    
    # Equation from Appendix Q - 3.3 in East manual for the estimate of Pooled Std. Deviation
    dStdDevPooled        <- sqrt( ( ( nQtyOfPatsOnE - 1) * dStdDevOfResponsesOnE ^ 2 + ( nQtyOfPatsOnS - 1 ) * dStdDevOfResponsesOnS ^ 2 )/( nQtyOfPatsOnE + nQtyOfPatsOnS - 2 ) )
    
    # Equation from Appendix Q - 3.3 in East manual
    dZj                  <- ( dMeanOfResponsesOnE - dMeanOfResponsesOnS )/( dStdDevPooled * sqrt( 1/nQtyOfPatsOnE + 1/nQtyOfPatsOnS ))
    dBoundary            <- ifelse( is.null( LookInfo ), DesignParam$CriticalPoint, LookInfo$EffBdryUpper[ nLookIndex])
    
    # Set look decision logic
    if( nLookIndex < nQtyOfLooks ) # Interim Analysis
    {
        if( dZj > dBoundary )
        {
            strDecision <- "Efficacy"
        }
        else
        {
            strDecision <- "Continue"
        }
    }
    else # Final Analysis
    {
        if( dZj > dBoundary )
        {
            strDecision <- "Efficacy"
        }
        else
        {
            strDecision <- "Futility"
        }
    }
    
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    
    
    Error <-  0
    
    
    return(list(TestStat = as.double( dZj ), ErrorCode = as.integer( Error ), Decision = as.integer( nDecision ) ))
}

