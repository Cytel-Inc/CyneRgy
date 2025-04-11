#################################################################################################### .
#   Program/Function Name: GetDecisionString
#   Author: Anoop Singh Rawat
#   Description: This function takes look information and efficacy and futility conditions to be checked and returns the strDecision needed for the GetDecision function.
#   Change History:
#   Last Modified Date: 10/09/2024
#################################################################################################### .
#' @name GetDecisionString
#' @title Generate Decision String Based on Interim and Final Analysis Conditions
#' 
#' @description This function evaluates look information, efficacy conditions, and futility conditions to generate the decision string (`strDecision`) required for the `GetDecision` function.
#' If `LookInfo` is not `NULL`, the `LookInfo$RejType` parameter can be used to determine the design type.
#' 
#' `LookInfo$RejType` Codes:
#' - **Efficacy Only**:
#'     - 1-Sided Efficacy Upper = 0
#'     - 1-Sided Efficacy Lower = 2
#' - **Futility Only**:
#'     - 1-Sided Futility Upper = 1
#'     - 1-Sided Futility Lower = 3
#' - **Efficacy and Futility**:
#'     - 1-Sided Efficacy Upper and Futility Lower = 4 
#'     - 1-Sided Efficacy Lower and Futility Upper = 5
#' - **Not in East Horizon Explore Yet**:
#'     - 2-Sided Efficacy Only = 6
#'     - 2-Sided Futility Only = 7
#'     - 2-Sided Efficacy and Futility = 8
#'     - Equivalence = 9
#' 
#' @param LookInfo List containing look information passed from East Horizon Explore to the R integration for analysis.
#' @param nLookIndex Integer indicating the current look index, created by the user in the analysis code.
#' @param nQtyOfLooks Integer indicating the total number of looks in the study, created by the user in the analysis code.
#' @param bIAEfficacyCondition Logical condition evaluated to determine interim efficacy at a look (defaults to `FALSE`).
#' @param bIAFutilityCondition Logical condition evaluated to determine interim futility at a look (defaults to `FALSE`).
#' @param bFAEfficacyCondition Logical condition evaluated to determine final efficacy at the last look (defaults to `FALSE`).
#' @param bFAFutilityCondition Logical condition evaluated to determine final futility at the last look (defaults to `FALSE`).
#' @export
#################################################################################################### .

GetDecisionString <- function( LookInfo, nLookIndex, nQtyOfLooks, bIAEfficacyCondition = FALSE, bIAFutilityCondition = FALSE, 
                                bFAEfficacyCondition = FALSE, bFAFutilityCondition = FALSE )
{
    if( nLookIndex < nQtyOfLooks )  # Interim Analysis
    {
        if( bIAEfficacyCondition & LookInfo$RejType %in% c( 0, 2, 4, 5 ) )
        {
            strDecision <- "Efficacy"
        }
        else if( bIAFutilityCondition & LookInfo$RejType %in% c( 1, 3, 4, 5 ) )
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