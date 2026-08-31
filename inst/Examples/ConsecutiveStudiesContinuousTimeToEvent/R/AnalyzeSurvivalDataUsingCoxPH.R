######################################################################################################################## .
#' @name AnalyzeSurvivalDataUsingCoxPH
#' @title Analyze simulated survival data using a Cox proportional hazards model
#' @description Administratively censor simulated survival data at the current analysis time, fit a Cox model, and
#' return the test statistic, decision, p-value, and configured true hazard-ratio output.
#' @author J. Kyle Wathen, Laurent Spiess, Gabriel Potvin
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East Horizon. The default must be NULL.
#' @return A named list containing `TestStat`, `Decision`, `ErrorCode`, `dPValue`, `HazardRatio`, and `TrueHR`.
######################################################################################################################## .
AnalyzeSurvivalDataUsingCoxPH <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    nError <- 0
    nLookIndex           <- 1

    if( !is.null( LookInfo ) )
    {
        # Look info was provided so use it
        nLookIndex   <- LookInfo$CurrLookIndex
        nQtyOfEvents <- LookInfo$CumEvents[ nLookIndex ]
    }
    else
    {
        nQtyOfEvents         <- DesignParam$MaxEvents
    }

    if( is.null( UserParam ) )
    {
        UserParam <- list( bReturnLogTrueHazard = FALSE, bReturnNAForNoGoTrials = FALSE )
    }

    SimData$TimeOfEvent  <- SimData$ArrivalTime + SimData$SurvivalTime    # This is the calendar time in the trial that the patients event is observed

    # Compute the time of analysis
    SimData              <- SimData[ order( SimData$TimeOfEvent ), ]
    dTimeOfAnalysis      <- SimData[ nQtyOfEvents, ]$TimeOfEvent

    # Add the Observed Time variable
    SimData              <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis , ]   # Exclude any patients that were not enrolled by the time of the analysis
    SimData$Event        <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )  # If the event is observed after the analysis it is not observed, eg censored
    SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )

    # Fit a cox model
    fitCox    <- survival::coxph( survival::Surv( ObservedTime, Event ) ~ as.factor( TreatmentID ), data = SimData )
    dPValue   <- summary( fitCox )$coefficients[ , "Pr(>|z|)" ]
    dZVal     <- summary( fitCox )$coefficients[ , "z" ]
    dPValue   <- pnorm( dZVal, lower.tail = TRUE )
    nDecision <- ifelse( dPValue <= DesignParam$Alpha, 2, 3 )

    #
    # Decision Code
    # 0  No Boundary Crossed
    # 1  Lower Efficacy Boundary Crossed
    # 2  Upper Efficacy Boundary Crossed
    # 3  Futility Boundary Crossed
    # 4  Equivalence Boundary Crossed
    #

    dTrueHR <- as.double( SimData$TrueHR[ 1 ] )

    if( UserParam$bReturnLogTrueHazard )
    {
        dTrueHR <- log( dTrueHR )
    }

    if( UserParam$bReturnNAForNoGoTrials & nDecision != 2 )
    {
        dTrueHR <- NA
    }
    lRet <- list( TestStat = as.double( dZVal ),
                  Decision  = as.integer( nDecision ),
                  ErrorCode = as.integer( nError ),
                  dPValue   = as.double( dPValue ),
                  HazardRatio = as.double( dTrueHR ),
                  TrueHR    = as.double( dTrueHR ) )

    return( lRet )
}
