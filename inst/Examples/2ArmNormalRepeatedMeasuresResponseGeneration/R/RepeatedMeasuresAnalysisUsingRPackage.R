######################################################################################################################## .
#' @name MMRMAna
#' @title Analyze Repeated Measures Using nlme
#' @description Reshapes simulated repeated-measures responses, fits a generalized least-squares model, and extracts the treatment test statistic.
#' @author Shubham Lahoti
#' @param SimData Data frame containing treatment assignments and visit-specific response columns.
#' @param DesignParam List containing `SampleSize` and `NumVisit` for the simulated design.
#' @param UserParam Optional list of user-defined parameters; unused by this example.
#' @return A named list containing `TestStat`, `PrimDelta`, `SecDelta`, and `ErrorCode`.
######################################################################################################################## .

MMRMAna <- function( SimData, DesignParam, UserParam = NULL )
{
    nError                  <- 0
    dPrimaryDeltaEstimate   <- 0
    dSecondaryDeltaEstimate <- 0
    dStandardError          <- 1

    # SimData <- read.csv( "C:\\Users\\shubham.lahoti\\Downloads\\MMRM codes and data\\MMRMSimData.csv" )
    SimData$id <- seq_len( DesignParam$SampleSize )
    nNumVisit  <- DesignParam$NumVisit

    dfLongData <- stats::reshape( SimData,
                                  varying   = paste0( "Response", seq_len( nNumVisit ) ),
                                  direction = "long",
                                  sep       = "",
                                  idvar     = "id" )
    dfLongData <- dfLongData[ order( dfLongData$TreatmentID, dfLongData$id, dfLongData$time ), ]

    vOutcome         <- dfLongData[ dfLongData$time != 1, ]$Response
    vBaselineOutcome <- dfLongData[ dfLongData$time == 1, ]$Response
    vBaselineOutcome <- rep( vBaselineOutcome, each = nNumVisit - 1 )
    vTreatment       <- dfLongData[ dfLongData$time != 5, ]$TreatmentID

    fitMMRM <- nlme::gls( vOutcome ~ vBaselineOutcome + vTreatment,
                          na.action  = stats::na.omit,
                          data       = dfLongData,
                          correlation = nlme::corSymm( form = ~ time | id ),
                          weights     = nlme::varIdent( form = ~ 1 | time ) )
    # fitMMRM <- nlme::gls( vOutcome ~ vBaselineOutcome * factor( time ) + TreatmentID * factor( time ),
    #                       na.action = stats::na.omit, data = dfLongData,
    #                       correlation = nlme::corSymm( form = ~ time | id ),
    #                       weights = nlme::varIdent( form = ~ 1 | time ) )
    summary( fitMMRM )

    if( summary( fitMMRM )$tTable[ "vTreatment", "p-value" ] <= 0.025 )
        nDecision <- 2
    else
        nDecision <- 0
    dTestStatistic <- summary( fitMMRM )$tTable[ "vTreatment", "t-value" ]

    return( list( TestStat   = as.double( dTestStatistic ),
                  PrimDelta  = as.double( dPrimaryDeltaEstimate ),
                  SecDelta   = as.double( dSecondaryDeltaEstimate ),
                  ErrorCode  = as.integer( nError ) ) )

    # return( list( Decision = as.integer( nDecision ), ErrorCode = as.integer( nError ),
    #               TestStat = as.double( dTestStatistic ),
    #               PVal = as.double( summary( fitMMRM )$tTable[ "vTreatment", "p-value" ] ) ) )
}
