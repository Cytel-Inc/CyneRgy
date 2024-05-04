#  Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East. The default must be NULL.
{{FUNCTION_NAME}} <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    nError 	        <- 0
    nDecision 	    <- 0
    dTestStatistic  <- 0
    
    # Input objects can be saved through the following lines
    # Saving is not available in Solara
    #setwd( "[ENTER THE DIRECTORY WHERE YOU WANT TO SAVE DATA]")
    #saveRDS( SimData, "SimData.Rds")
    #saveRDS( DesignParam, "DesignParam.Rds" )
    #saveRDS( LookInfo, "LookInfo.Rds" )
    

    lRet <- list(TestStat = as.double(dTestStatistic),
                 Decision  = as.integer(nDecision), 
                 ErrorCode = as.integer(nError))
    return( lRet )
}

