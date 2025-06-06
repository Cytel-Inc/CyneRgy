#################################################################################################### .
#   Program/Function Name: GetDecision
#   Author: J. Kyle Wathen
#   Description: This function takes a string for the desired decision, design and look info and return the correct decision value. 
#   Change History:
#   Last Modified Date: 08/21/2024
#################################################################################################### .
#' @name GetDecision
#' @title Determine Decision Based on Decision String, Design and Look Information
#' 
#' @description This function takes a string indicating the desired decision ("Efficacy", "Futility", or "Continue"), design parameters, and look information, and returns the appropriate decision value. 
#' If `LookInfo` is not NULL, the function uses `LookInfo$RejType` to help determine the design type:
#'
#' - **LookInfo$RejType Codes**:
#'   - *Efficacy Only*:
#'     - 1-Sided Efficacy Upper = 0
#'     - 1-Sided Efficacy Lower = 2
#'   - *Futility Only*:
#'     - 1-Sided Futility Upper = 1
#'     - 1-Sided Futility Lower = 3
#'   - *Efficacy and Futility*:
#'     - 1-Sided Efficacy Upper & Futility Lower = 4
#'     - 1-Sided Efficacy Lower & Futility Upper = 5
#'   - *Additional Scenarios Not in East Horizon Explore*:
#'     - 2-Sided Efficacy Only = 6
#'     - 2-Sided Futility Only = 7
#'     - 2-Sided Efficacy & Futility = 8
#'     - Equivalence = 9
#'
#' The function also uses `DesignParam$TailType` to determine tail direction:
#' - 0: Left-tailed
#' - 1: Right-tailed
#'
#' Based on the design type and tail direction, the function evaluates the decision and returns the corresponding integer decision value. Errors are raised for invalid input combinations.
#'
#' @param strDecision A string indicating the desired decision: "Efficacy", "Futility", or "Continue".
#' @param DesignParam A list containing design parameters sent from East Horizon Explore to the R integration for analysis.
#' @param LookInfo A list containing look information sent from East Horizon Explore to the R integration for analysis.
#' @export
#################################################################################################### .
GetDecision <- function( strDecision, DesignParam, LookInfo )
{
    nReturnDecision <- -1   # This is an error 
    strDesignType   <- NA
    strDirection    <- ""
    
    # Step 1 - Determine the  direction
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
                else if( strDecision == "Efficacy" )
                {
                    stop( "CyneRgy::GetDecision - Efficacy check is not enabled at this look. Therefore, 'EFficacy' is not a valid value for strDecision at this look. Please use either 'Continue' or 'Futility'." )
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
                else if( strDecision == "Continue" )
                {
                    stop( "CyneRgy::GetDecision - 'Continue' is not a valid value for strDecision at the last look. Please use either 'Efficacy' or 'Futility'." )
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
                else if( strDecision == "Continue"  )
                {
                    nReturnDecision <- 0
                }
                else if( strDecision == "Futility" )
                {
                    stop( "CyneRgy::GetDecision - Futility check is not enabled at this look. Therefore, 'Futility' is not a valid value for strDecision at this look. Please use either 'Continue' or 'Efficacy'." ) 
                }
            }
            else
            {
                if( strDecision == "Efficacy" )
                {
                    nReturnDecision <- 2
                }
                else if( strDecision == "Futility" )
                {
                    nReturnDecision <- 0
                }
                else if( strDecision == "Continue" )
                {
                    stop( "CyneRgy::GetDecision - 'Continue' is not a valid value for strDecision at the last look. Please use either 'Efficacy' or 'Futility'." )
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
                else if( strDecision == "Futility" )
                {
                    nReturnDecision <- 3
                }
                else if( strDecision == "Continue" )
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
                else if( strDecision == "Futility" )
                {
                    nReturnDecision <- 3
                }
                else if( strDecision == "Continue")
                {
                    stop( "CyneRgy::GetDecision - 'Continue' is not a valid value for strDecision at the last look. Please use either 'Efficacy' or 'Futility'." )
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
                else if( strDecision == "Efficacy" )
                {
                    stop( "CyneRgy::GetDecision - EFficacy check is not enabled at this look. Therefore, 'Efficacy' is not a valid value for strDecision at this look. Please use either 'Continue' or 'Futility'." )
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
                else if( strDecision == "Continue" )
                {
                    stop( "CyneRgy::GetDecision - 'Continue' is not a valid value for strDecision at the last look. Please use either 'Efficacy' or 'Futility'." )
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
                else if( strDecision == "Continue" )
                {
                    nReturnDecision <- 0
                }
                else if( strDecision == "Futility" )
                {
                    stop( "CyneRgy::GetDecision - Futility check is not enabled at this look. Therefore, 'Futility' is not a valid value for strDecision at this look. Please use either 'Continue' or 'Efficacy'." )
                }
            }
            else
            {
                if( strDecision == "Efficacy" )
                {
                    nReturnDecision <- 1
                }
                else if( strDecision == "Futility" )
                {
                    nReturnDecision <- 0
                }
                else if( strDecision == "Continue" )
                {
                    stop( "CyneRgy::GetDecision - 'Continue' is not a valid value for strDecision at the last look. Please use either 'Efficacy' or 'Futility'." )
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
                else if( strDecision == "Futility" )
                {
                    nReturnDecision <- 3
                }
                else if( strDecision == "Continue" )
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
                else if( strDecision == "Futility" )
                {
                    nReturnDecision <- 3
                }
                else if( strDecision == "Continue" )
                {
                    stop( "CyneRgy::GetDecision - 'Continue' is not a valid value for strDecision at the last look. Please use either 'Efficacy' or 'Futility'." )
                }
                
            }
        }
        
    }
    
    
    return( as.integer( nReturnDecision ) )
}
