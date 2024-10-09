#################################################################################################### .
#   Program/Function Name:
#   Author: Anoop Singh Rawat
#   Description: This function takes look information and efficacy and futility conditions to be checked and returns the strDecision needed for the GetDecision function.
#   Change History:
#   Last Modified Date: 10/09/2024
#################################################################################################### .
#' @name GetDecisionString
#' @title GetDecisionString
#' @description { Description: This function takes look information and efficacy and futility conditions to be checked and returns the strDecision needed for the GetDecision function.  
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
#' 
#' }
#' @param LookInfo The LookInfo parameter sent from East Horizon Explore to the R integration for analysis.
#' @param nLookIndex is an integer indicating the current look. This is created by the user in the analysis code.
#' @param nQtyOfLooks is an integer indicating the total number of looks in the study. This is created by the user in the analysis code.
#' @param bIAEfficacyCondition is a condition that renders to a boolean. If available and applicable, this condition is checked to declare efficacy at an interim look.
#' @param bIAfutilityCondition is a condition that renders to a boolean. If available and applicable, this condition is checked to declare futility at an interim look.
#' @param bFAefficacyCondition is a condition that renders to a boolean. If available and applicable, this condition is checked to declare efficacy at the final look.
#' @param bFAfutilityCondition is a condition that renders to a boolean. If available and applicable, this condition is checked to declare futility at the final look.
#' @export
GetDecisionString <- function( LookInfo, nLookIndex, nQtyOfLooks, bIAEfficacyCondition = FALSE, bIAFutilityCondition = FALSE, 
                                bFAEfficacyCondition = FALSE, bFAFutilityCondition = FALSE )
{
    if( nLookIndex < nQtyOfLooks )  # Interim Analysis
    {
        if( bIAEfficacyCondition & LookInfo$RejType %in% c(0, 2, 4, 5) )
        {
            strDecision <- "Efficacy"
        }
        else if( bIAFutilityCondition & LookInfo$RejType %in% c(1, 3, 4, 5) )
        {
            strDecision <- "Futility"
        }
        else
        {
            strDecision <- "Continue"
        }
    }
    else # Final Analysis
    {
        if( bFAEfficacyCondition )
        {
            strDecision <- "Efficacy"
        }
        else if( bFAFutilityCondition )
        {
            strDecision <- "Futility"
        }
        else
        {
            strDecision <- "Futility"
        }
    }
    return( strDecision )
}