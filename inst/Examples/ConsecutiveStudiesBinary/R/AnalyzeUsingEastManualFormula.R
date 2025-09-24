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
#' @return Decision An integer value: Call GetDecision with Efficacy, Futility or Continue 
#' @return ErrorCode An integer value:  ErrorCode = 0 --> No Error
#'                                      ErrorCode > 0 --> Non fatal error, current simulation is aborted but the next simulations will run
#'                                      ErrorCode < 0 --> Fatal error, no further simulation will be attempted
######################################################################################################################## .

AnalyzeUsingEastManualFormula<- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL)
{
    
    # Step 1 -Retrieve necessary information from the objects East sent####

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
    
    # Step 2: Create the vector of simulated data for this IA - East sends all of the simulated data ####
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    
    # Step 3: Create vectors of data for each treatment - E is Experimental and S is Standard of Care #### 
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]
    
    nQtyOfResponsesOnE   <- sum( vOutcomesE )
    nQtyOfPatsOnE        <- length( vOutcomesE )
    
    nQtyOfResponsesOnS   <- sum( vOutcomesS )
    nQtyOfPatsOnS        <- length( vOutcomesS )
    
    # Step 4: Compute the estimates in equation 28.2 from the East user manual####
    dPiHatExperimental   <- nQtyOfResponsesOnE/nQtyOfPatsOnE
    dPiHatControl        <- nQtyOfResponsesOnS/nQtyOfPatsOnS
    
    dPiHatj              <- ( nQtyOfResponsesOnE +  nQtyOfResponsesOnS )/( nQtyOfPatsOnE + nQtyOfPatsOnS )
    
    # Equation 28.2 in East manual
    dZj                  <- ( dPiHatExperimental - dPiHatControl )/sqrt( dPiHatj*( 1- dPiHatj ) * ( 1/nQtyOfPatsOnE + 1/nQtyOfPatsOnS)  ) 
    
    # Step 5: Determine the decision ####
    if(  !is.null( LookInfo )  )
    {
        strDecision <- ifelse( dZj < LookInfo$EffBdryLower[ nLookIndex], "Efficacy", "Continue" )  
    }
    else
    {
        strDecision <- ifelse( dZj < DesignParam$CriticalPoint,  "Efficacy", "Futility"  )    
    }
    
    
    if( strDecision != "Efficacy" )
    {
        # Did not hit efficacy, so check futility 
        # We are at the FA, efficacy decision was not made yet so the decision is futility
        if( nLookIndex == nQtyOfLooks ) 
        {
            strDecision <- "Futility" # Code for futility 
        }
    }
    nDecision <- GetDecision( strDecision, DesignParam, LookInfo ) 
    
    Error <-  0
    
    
    return(list(TestStat  = as.double( dZj ), 
                ErrorCode = as.integer( Error ), 
                Decision  = as.integer( nDecision ),
                Delta     = as.double( dPiHatExperimental - dPiHatControl),
                TrueDelta = as.double( SimData$TrueProbabilityExperimental[ 1 ] - SimData$TrueProbabilityControl[ 1 ] ),
                TrueProbabilityControl = as.double(SimData$TrueProbabilityControl[ 1 ] ),
                TrueProbabilityExperimental = as.double(SimData$TrueProbabilityExperimental[ 1 ])))
}


#' @description { Description: This function takes a string for the desired decision, design and look info and return the correct decision value.  
#' If LookInfo is not Null then looking at LookInfo$RejType can help determine the design type
#'   LookInfo$RejType Codes:
#'     Efficacy Only:
#'       1 Sided Efficacy Upper = 0
#'       1 Sided Efficacy Lower = 2
#'     
#'     Futility Only:
#'       1 Sided Futility Upper = 1
#'       1 Sided Futility Lower = 3
#'     
#'     Efficacy Futility:
#'       1 Sided Efficacy Upper Futility Lower = 4 
#'       1 Sided Efficacy Lower Futility Upper = 5
#'     
#'     Not in East Horizon Explore Yet:
#'       2 Sided Efficacy Only = 6
#'       2 Sided Futility Only = 7
#'       2 Sided Efficacy Futility = 8
#'       Equivalence = 9
#'
#'   Then using DesignParam$TailType
#'       0: Left Tailed
#'       1: Right Tailed
#'
#' 
#' 
#' }
#' @param strDecision is a  string with either "Efficacy", "Futility" or "Continue"
#' @param DesignParam This is the DesignParam sent from East Horizon Explore to the R integration for analysis.
#' @param LookInfo The LookInfo parameter sent from East Horizon Explore to the R integration for analysis.
GetDecision <- function(  strDecision, DesignParam, LookInfo )
{
    nReturnDecision <- -1   # This is an error 
    strDesignType   <- NA
    strDirection    <- ""
    
    # Step 1 - Determine the  direction ####
    if( DesignParam$TailType == 0 )
    {
        strDirection <- "Left"
    }
    else if(  DesignParam$TailType == 1 )
    {
        strDirection <- "Right"
    }
    
    # Step 2 - determine the design type so we know what decision to return ####
    
    if( is.null( LookInfo ) )
    {
        # No LookInfo so this is a fixed design --> EfficacyOnly
        strDesignType    <- "EfficacyOnly"
        bInterimAnalysis <- FALSE
        
    }
    else if( LookInfo$RejType == 1 | LookInfo$RejType == 3 )
    {
        # There is a futility boundary but no efficacy boundary
        strDesignType    <- "FutilityOnly"
        bInterimAnalysis <- LookInfo$CurrLookIndex < LookInfo$NumLooks 
    }
    else if( LookInfo$RejType == 0 | LookInfo$RejType == 2 )
    {
        # There is an efficacy boundary but no futility boundary
        strDesignType    <- "EfficacyOnly"
        bInterimAnalysis <- LookInfo$CurrLookIndex < LookInfo$NumLooks 
    }
    else if( LookInfo$RejType == 4 | LookInfo$RejType == 5 )
    {
        # There is an efficacy boundary and futility boundary
        strDesignType    <- "EfficacyFutility"
        bInterimAnalysis <- LookInfo$CurrLookIndex < LookInfo$NumLooks 
    }
    
    
    # Can we use the DesignParam and LookInfo to define the design type and IA/FA?  -  Assuming we can suppose strDesignType = "FutilityOnly", "EfficacyOnly" or "EfficacyFutiity"
    
    if( strDirection == "Right" )
    {
        if( strDesignType == "FutilityOnly" )
        {
            if( bInterimAnalysis )
            {
                if( strDecision == "Futility" )
                { 
                    nReturnDecision <- 3
                }
                else if( strDecision == "Continue" )
                {
                    nReturnDecision <- 0
                }
                
            }
            else # It is a futility only design at the final analysis
            {
                if( strDecision == "Futility" )
                { 
                    nReturnDecision <- 3
                }
                else if( strDecision == "Efficacy" )
                {
                    nReturnDecision <- 0
                }
                
            }
            
        }
        else if( strDesignType == "EfficacyOnly" )
        {
            if( bInterimAnalysis )
            {
                if( strDecision == "Efficacy" )
                {
                    nReturnDecision <- 2
                }
                else if(strDecision == "Continue"  )
                {
                    nReturnDecision <- 0
                }
            }
            else
            {
                if( strDecision == "Efficacy" )
                {
                    nReturnDecision <- 2
                }
                else if(strDecision == "Futility"  )
                {
                    nReturnDecision <- 3  # By definition it should be 0
                }
                
            }
        }    
        else if( strDesignType == "EfficacyFutility" )
        {
            
            if( bInterimAnalysis )
            {
                if( strDecision == "Efficacy" )
                {
                    nReturnDecision <- 2
                }
                else if(strDecision == "Futility"  )
                {
                    nReturnDecision <- 3
                }
                else if(strDecision == "Continue"  )
                {
                    nReturnDecision <- 0
                }
            }
            else
            {
                if( strDecision == "Efficacy" )
                {
                    nReturnDecision <- 2
                }
                else if(strDecision == "Futility"  )
                {
                    nReturnDecision <- 3
                }
                
            }
        }
    }
    else if( strDirection == "Left" )
    {
        
        if( strDesignType == "FutilityOnly" )
        {
            if( bInterimAnalysis )
            {
                if( strDecision == "Futility" )
                { 
                    nReturnDecision <- 3
                }
                else if( strDecision == "Continue" )
                {
                    nReturnDecision <- 0
                }
                
            }
            else # It is a futility only design at the final analysis
            {
                if( strDecision == "Futility" )
                { 
                    nReturnDecision <- 3
                }
                else if( strDecision == "Efficacy" )
                {
                    nReturnDecision <- 0
                }
                
            }
            
        }
        else if( strDesignType == "EfficacyOnly" )
        {
            if( bInterimAnalysis )
            {
                if( strDecision == "Efficacy" )
                {
                    nReturnDecision <- 1
                }
                else if(strDecision == "Continue"  )
                {
                    nReturnDecision <- 0
                }
            }
            else
            {
                if( strDecision == "Efficacy" )
                {
                    nReturnDecision <- 1
                }
                else if(strDecision == "Futility"  )
                {
                    nReturnDecision <- 0
                }
                
            }
        }    
        else if( strDesignType == "EfficacyFutility" )
        {
            
            if( bInterimAnalysis )
            {
                if( strDecision == "Efficacy" )
                {
                    nReturnDecision <- 1
                }
                else if(strDecision == "Futility"  )
                {
                    nReturnDecision <- 3
                }
                else if(strDecision == "Continue"  )
                {
                    nReturnDecision <- 0
                }
            }
            else
            {
                if( strDecision == "Efficacy" )
                {
                    nReturnDecision <- 1
                }
                else if(strDecision == "Futility"  )
                {
                    nReturnDecision <- 3
                }
                
            }
        }
        
    }
    
    
    return( as.integer(nReturnDecision) )
}