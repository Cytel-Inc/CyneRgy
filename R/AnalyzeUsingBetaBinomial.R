#################################################################################################### .
#   Program/Function Name: AnalyzeUsingBetaBinomial
#   Description: Analyze two-arm binary data using a beta-binomial model.
#################################################################################################### .
#' @name AnalyzeUsingBetaBinomial
#' @title Analyze Binary Data Using a Beta-Binomial Model
#'
#' @description Computes the posterior probability that the experimental response rate is greater than the control response rate.
#' At an interim look, efficacy and futility are determined by `dUpperCutoffEfficacy` and `dLowerCutoffForFutility`. At the final
#' look, efficacy is declared when the posterior probability exceeds the efficacy cutoff.
#'
#' @param SimData Data frame containing `Response` and `TreatmentID`, where treatment `0` is control and treatment `1` is experimental.
#' @param DesignParam List containing `TailType`. For compatibility with fixed-design inputs, it may also contain `MaxCompleters`.
#' @param LookInfo Optional list describing the current look. When supplied, it must contain `CurrLookIndex`, `NumLooks`,
#' `CumCompleters`, and `RejType`.
#' @param UserParam List containing `dAlphaCtrl`, `dBetaCtrl`, `dAlphaExp`, `dBetaExp`, `dUpperCutoffEfficacy`, and
#' `dLowerCutoffForFutility`.
#'
#' @return A list containing posterior probability `TestStat`, integer `ErrorCode`, integer `Decision`, and posterior mean
#' difference `Delta`. Missing `UserParam` values produce fatal `ErrorCode = -1`.
#'
#' @export
#################################################################################################### .

AnalyzeUsingBetaBinomial <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    if( is.null( LookInfo ) )
    {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
    }
    else
    {
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfLooks          <- LookInfo$NumLooks
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
    }

    vRequiredParams <- c( "dAlphaCtrl", "dBetaCtrl", "dAlphaExp", "dBetaExp",
                          "dUpperCutoffEfficacy", "dLowerCutoffForFutility" )
    vMissingParams  <- vRequiredParams[ !vRequiredParams %in% names( UserParam ) ]

    if( is.null( UserParam ) || length( vMissingParams ) > 0 )
    {
        return( list( TestStat  = as.double( 0 ),
                      ErrorCode = as.integer( -1 ),
                      Decision  = as.integer( 0 ),
                      Delta     = as.double( 0 ) ) )
    }

    if( !all( c( "Response", "TreatmentID" ) %in% names( SimData ) ) ||
        nQtyOfPatsInAnalysis < 1 || nQtyOfPatsInAnalysis > nrow( SimData ) )
        stop( "SimData must contain Response and TreatmentID for every subject in the analysis.", call. = FALSE )

    vPatientOutcome   <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]
    vOutcomesCtrl     <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesExp      <- vPatientOutcome[ vPatientTreatment == 1 ]

    if( length( vOutcomesCtrl ) == 0 || length( vOutcomesExp ) == 0 ||
        any( !vPatientOutcome %in% c( 0, 1 ) ) || any( !vPatientTreatment %in% c( 0, 1 ) ) )
        stop( "SimData must contain binary outcomes and subjects from control and experimental arms.", call. = FALSE )

    lRet <- ProbExpGreaterCtrlBeta( vOutcomesCtrl, vOutcomesExp,
                                    UserParam$dAlphaCtrl, UserParam$dBetaCtrl,
                                    UserParam$dAlphaExp, UserParam$dBetaExp )

    strDecision <- GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                      bIAEfficacyCondition = lRet$dPostProb > UserParam$dUpperCutoffEfficacy,
                                      bIAFutilityCondition = lRet$dPostProb < UserParam$dLowerCutoffForFutility,
                                      bFAEfficacyCondition = lRet$dPostProb > UserParam$dUpperCutoffEfficacy )
    nDecision <- GetDecision( strDecision, DesignParam, LookInfo )
    Error     <- 0

    return( list( TestStat  = as.double( lRet$dPostProb ),
                  ErrorCode = as.integer( Error ),
                  Decision  = as.integer( nDecision ),
                  Delta     = as.double( lRet$dDelta ) ) )
}


ProbExpGreaterCtrlBeta <- function( vOutcomesCtrl, vOutcomesExp, dAlphaCtrl, dBetaCtrl, dAlphaExp, dBetaExp )
{
    dAlphaCtrl <- dAlphaCtrl + sum( vOutcomesCtrl )
    dBetaCtrl  <- dBetaCtrl + length( vOutcomesCtrl ) - sum( vOutcomesCtrl )
    dAlphaExp  <- dAlphaExp + sum( vOutcomesExp )
    dBetaExp   <- dBetaExp + length( vOutcomesExp ) - sum( vOutcomesExp )

    if( any( c( dAlphaCtrl, dBetaCtrl, dAlphaExp, dBetaExp ) <= 0 ) )
        stop( "All posterior beta-distribution parameters must be positive.", call. = FALSE )

    vPiCtrl   <- stats::rbeta( 10000, dAlphaCtrl, dBetaCtrl )
    vPiExp    <- stats::rbeta( 10000, dAlphaExp, dBetaExp )
    dPostProb <- mean( vPiExp > vPiCtrl )
    dDelta    <- dAlphaExp / ( dAlphaExp + dBetaExp ) - dAlphaCtrl / ( dAlphaCtrl + dBetaCtrl )

    return( list( dPostProb = dPostProb, dDelta = dDelta ) )
}
