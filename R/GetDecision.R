#################################################################################################### .
#   Program/Function Name: GetDecision
#   Author: J. Kyle Wathen
#   Description: Convert a decision label into the value expected by the analysis integration point.
#################################################################################################### .
#' @name GetDecision
#' @title Convert a Decision Label to an Integration Decision Value
#'
#' @description Converts `"Efficacy"`, `"Futility"`, or `"Continue"` into the integer decision value expected by the analysis
#' integration point. The result depends on the tail direction, the boundaries enabled by `LookInfo$RejType`, and whether the current
#' look is interim or final.
#'
#' Supported `LookInfo$RejType` values are:
#'
#' - `0` or `2`: efficacy boundary only.
#' - `1` or `3`: futility boundary only.
#' - `4` or `5`: efficacy and futility boundaries.
#'
#' A fixed design is represented by `LookInfo = NULL` and is treated as an efficacy-only final analysis.
#'
#' @param strDecision Character string equal to `"Efficacy"`, `"Futility"`, or `"Continue"`.
#' @param DesignParam List containing `TailType`, where `0` is left-tailed and `1` is right-tailed.
#' @param LookInfo Optional list containing `RejType`, `CurrLookIndex`, and `NumLooks`.
#'
#' @return Integer decision value: `0` for no boundary crossed, `1` for lower efficacy, `2` for upper efficacy, or `3` for futility.
#'
#' @export
#################################################################################################### .

GetDecision <- function( strDecision, DesignParam, LookInfo = NULL )
{
    vValidDecisions <- c( "Efficacy", "Futility", "Continue" )
    if( length( strDecision ) != 1 || !strDecision %in% vValidDecisions )
        stop( "strDecision must be 'Efficacy', 'Futility', or 'Continue'.", call. = FALSE )

    if( is.null( DesignParam$TailType ) || length( DesignParam$TailType ) != 1 || !DesignParam$TailType %in% c( 0, 1 ) )
        stop( "DesignParam$TailType must be 0 for left-tailed or 1 for right-tailed.", call. = FALSE )

    nEfficacyDecision <- if( DesignParam$TailType == 0 ) 1 else 2

    if( is.null( LookInfo ) )
    {
        strDesignType    <- "EfficacyOnly"
        bInterimAnalysis <- FALSE
    }
    else
    {
        vRequiredLookInfo <- c( "RejType", "CurrLookIndex", "NumLooks" )
        vMissingLookInfo  <- vRequiredLookInfo[ !vRequiredLookInfo %in% names( LookInfo ) ]
        if( length( vMissingLookInfo ) > 0 )
            stop( "LookInfo is missing: ", paste( vMissingLookInfo, collapse = ", " ), call. = FALSE )
        if( length( LookInfo$RejType ) != 1 || !LookInfo$RejType %in% 0:5 )
            stop( "LookInfo$RejType must be an integer from 0 through 5.", call. = FALSE )
        if( length( LookInfo$CurrLookIndex ) != 1 || length( LookInfo$NumLooks ) != 1 ||
            LookInfo$CurrLookIndex < 1 || LookInfo$NumLooks < 1 || LookInfo$CurrLookIndex > LookInfo$NumLooks )
            stop( "LookInfo$CurrLookIndex must be between 1 and LookInfo$NumLooks.", call. = FALSE )

        if( LookInfo$RejType %in% c( 0, 2 ) )
            strDesignType <- "EfficacyOnly"
        else if( LookInfo$RejType %in% c( 1, 3 ) )
            strDesignType <- "FutilityOnly"
        else
            strDesignType <- "EfficacyFutility"

        bInterimAnalysis <- LookInfo$CurrLookIndex < LookInfo$NumLooks
    }

    if( bInterimAnalysis && strDesignType == "EfficacyOnly" && strDecision == "Futility" )
        stop( "Futility is not enabled at this interim look. Use 'Continue' or 'Efficacy'.", call. = FALSE )
    if( bInterimAnalysis && strDesignType == "FutilityOnly" && strDecision == "Efficacy" )
        stop( "Efficacy is not enabled at this interim look. Use 'Continue' or 'Futility'.", call. = FALSE )
    if( !bInterimAnalysis && strDecision == "Continue" )
        stop( "Continue is not valid at the final look. Use 'Efficacy' or 'Futility'.", call. = FALSE )

    if( strDecision == "Continue" )
        nReturnDecision <- 0
    else if( strDecision == "Futility" && strDesignType != "EfficacyOnly" )
        nReturnDecision <- 3
    else if( strDecision == "Efficacy" && strDesignType != "FutilityOnly" )
        nReturnDecision <- nEfficacyDecision
    else
        nReturnDecision <- 0

    return( as.integer( nReturnDecision ) )
}
