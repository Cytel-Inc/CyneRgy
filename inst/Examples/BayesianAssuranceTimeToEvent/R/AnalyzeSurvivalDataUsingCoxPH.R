######################################################################################################################## .
#' @name AnalyzeSurvivalDataUsingCoxPH
#' @title Analyze Simulated Survival Data Using a Cox Proportional Hazards Model
#' @description Fit a Cox proportional hazards model to administratively censored simulated survival data and return
#' the test statistic, decision, p-value, and configured true hazard-ratio output.
#' @author J. Kyle Wathen and Laurent Spiess
#' @details Performs a Cox proportional hazards regression analysis on simulated
#' time-to-event data. The function determines the analysis time based on the specified
#' interim or final look, applies administrative censoring, fits a Cox proportional hazards
#' model comparing treatment groups, and returns the resulting test statistic, p-value,
#' decision code, and hazard ratio information.
#'
#' @param SimData Data frame which consists of data generated in current simulation.
#' @param DesignParam List of Design and Simulation Parameters required to perform analysis.
#' @param LookInfo List containing Design and Simulation Parameters, which might be required to perform analysis.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' The list may contain the following named elements:
#'        \describe{
#'         \item{UserParam$bReturnLogTrueHazard}{Logical indicating whether the returned hazard ratio should be transformed using
#'               the natural logarithm. Default is \code{FALSE}.}
#'         \item{UserParam$bReturnNAForNoGoTrials}{Logical indicating whether the hazard ratio should be returned as \code{NA} when
#'               the trial does not meet the efficacy criterion. Default is \code{FALSE}.}
#'        }
#' @return A named list containing elements as described below.
#'        \describe{
#'          \item{TestStat}{Z statistic from the Cox proportional hazards model}
#'          \item{Decision}{Required value. Integer Value with the following meaning:
#'                          \describe{
#'                             \item{Decision = 0}{No boundary crossed}
#'                             \item{Decision = 1}{Lower Efficacy Boundary Crossed}
#'                             \item{Decision = 2}{Upper Efficacy Boundary Crossed}
#'                             \item{Decision = 3}{Futility Boundary Crossed}
#'                             \item{Decision = 4}{Equivalence Boundary Crossed}
#'                           }
#'                           }
#'          \item{ErrorCode}{Decision codes:
#'                          \describe{
#'                             \item{ErrorCode = 0}{No Error}
#'                             \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                             \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                           }
#'                           }
#'          \item{dPValue}{One-sided p-value derived from the z-statistic}
#'          \item{HazardRatio}{Returned true hazard ratio (or log hazard ratio if requested)}
#'          \item{TrueHR}{Same value as \code{HazardRatio}}
#'        }
#'
######################################################################################################################## .

AnalyzeSurvivalDataUsingCoxPH <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    nError      <- 0
    nLookIndex <- 1

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

    dTrueHR <- as.double( SimData$TrueHR[ 1 ] )

    if( as.logical( UserParam$bReturnLogTrueHazard ) )
    {
        dTrueHR <- log( dTrueHR )
    }

    if( as.logical( UserParam$bReturnNAForNoGoTrials ) & nDecision != 2 )
    {
        dTrueHR <- NA
    }

    lRet <- list( TestStat  = as.double( dZVal ),
                  Decision  = as.integer( nDecision ),
                  ErrorCode = as.integer( nError ),
                  dPValue   = as.double( dPValue ),
                  HazardRatio = as.double( dTrueHR ),
                  TrueHR    = as.double( dTrueHR ) )

    return( lRet )
}
