######################################################################################################################## .
# THIS CODE IS JUST AN EXAMPLE FOR REPEATED MEASURE AND MAY NOT WORK, JUST AN IDEA, HENCE IN THE SANDBOX ####
######################################################################################################################## .

#' Analyze multivariate normals,
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East. The default must be NULL.
#' If UseParam is supplied, the list must contain the following named elements:
#' \describe{
#'    \item{UserParam$nQtyOfTimePoints}{The number of subtypes, or outcomes, to generate.}
#'    }
#' @export
AnalyzeMultiVariateNormal <- function(SimData, DesignParam, LookInfo, UserParam = NULL)
{
    library()
    
    # Step 1 - Determine how many types there are in the dataset ####
    
    dDelta         <- 0 
    strAlternative <- "two.sided"
    
    if( is.null( UserParam$dDelta ) == FALSE )
    {
        dDelta         <- UserParam$dDelta 
        strAlternative <- "less"
    }
    
    nQtyOfSuccessfulTypes <- 0 
    
    vTreatment <- SimData$TreatmentID 
    for( nType in 1:UserParam$nQtyOfTypes )
    {
        # Just as a POC, analyze using a t-test for decision making 
        # Get the data from SimData
        strTypeName <- paste0( "Type", nType )
        vOutcome <- SimData[[ strTypeName ]]
        
        testResults <- t.test( vOutcome ~ vTreatment, mu = dDelta, alternative = strAlternative )
        
        if( testResults$p.value < 0.025 )
            nQtyOfSuccessfulTypes <- nQtyOfSuccessfulTypes + 1
    }
    
    # If we have at least UserParam$nQtyMinimumTypes  that are significant then the trial is successful
    if( nQtyOfSuccessfulTypes >= UserParam$nQtyMinimumTypes )  
    {
        nDecision <- 2  # Success
        
    }
    else
    {
        nDecision <- 3 #Futility 
    }
    

    
    # Note: the SimData$vTrueDelta vector was added to the SimData via the return in the SimulatePateintOutcomeNormalAssurance
    
    lReturn <- list(TestStat = as.double(0), 
                    Decision = as.integer(nDecision), 
                    ErrorCode = as.integer(0))
    
    return( lReturn )
}

