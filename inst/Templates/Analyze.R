
# Function Template for Performing Test for Multi Look Tests
#  Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template for analysis function with multiple looks
{{FUNCTION_NAME}} <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    nError 	        <- 0
    nDecision 	    <- 0
    dTestStatistic  <- 0
    
    # Write the actual code here.
    # Compute test statistic value and store the decision
    # value (appropriate code) in retval
    # Use appropriate error handling and modify the
    # Error appropriately
    
    return(list(Decision  = as.integer(nDecision), 
                ErrorCode = as.integer(nError),
                TestStat  = as.double(dTestStatistic),))
}

