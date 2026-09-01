######################################################################################################################## .
#' @name AnalyzeSurvivalDataUsingCoxPH
#' @title Analyze simulated survival data using a Cox proportional hazards model
#' @description Administratively censor simulated survival data at the current analysis time, fit a Cox model, and
#' return the test statistic, decision, p-value, and configured true hazard-ratio output.
#' @author J. Kyle Wathen, Laurent Spiess, Gabriel Potvin
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#'        \describe{
#'          \item{ArrivalTime}{ A numeric value with the time the patient arrived in the trial}
#'          \item{TreatmentID}{An integer value where 0 indicates control treatment and 1 experimental treatment.}
#'          \item{SurvivalTime}{Numeric value for the survival time or time-to-event for the patient, note this is not the time in the trial
#'                               that the patient experiences the event.}
#'          \item{DropOutTime}{Numeric value for the dropout time for the patient in a time to event trial.}
#'        }
#' @param DesignParam List of design and simulation parameters needed to compute test statistics and perform testing. Access elements by name, for example `DesignParam$Alpha`, rather than by position.
#'      \describe{
#'          \item{SampleSize}{Sample size of the trial}
#'          \item{Alpha}{Type I Error}
#'          \item{TestType}{Values are One side: 0; Two Sided: 1, Two Sided, Asymmetric: 2}
#'          \item{TailType}{Values are Left Tailed: 0, Right Tailed: 1}
#'          \item{LowerAlpha}{Lower Type I error. Present for Left Tailed and Two Sided Asymmetric Tests }
#'          \item{UpperAlpha}{Upper Type I error. Present for Right Tailed and Two Sided Asymmetric Tests }
#'          \item{CriticalPoint}{Critical Value. Present in Fixed Sample designs only }
#'          \item{UpperCriticalPoint}{Upper Critical Value. Present in Right Tail Fixed Sample designs only }
#'          \item{LowerCriticalPoint}{Lower Critical Value. Present in Left Tail Fixed Sample designs only }
#'          \item{MaxEvents}{Maximum Events in a time to event based trial}
#'          \item{FollowUpType}{For survival tests, Follow Up Type. Possible values are: Until End of Study: 0, For fixed period: 1}
#'          \item{AllocInfo}{Vector of ratios of treatment sample sizes to control sample size. Length = Number of treatment arms }
#'      }
#' @param LookInfo List of parameters for the current analysis look. It is `NULL` for fixed-sample designs. Access elements by name, for example `LookInfo$NumLooks`, rather than by position.
#'                 \describe{
#'                      \item{NumLooks}{An integer value with the number of looks in the study}
#'                      \item{CurrLookIndex}{An integer value with the current index look, starting from 1}
#'                      \item{CumEvents}{Vector containing the cumulative number of events for each look.}
#'                      \item{InfoFrac}{Information fraction}
#'                      \item{LookTime}{Look time on the calendar scale.}
#'                      \item{RejType}{Rejection type identifying the enabled efficacy and futility boundaries.}
#'                      \item{CumAlpha}{Cumulative alpha spent. Present in one sided tests only }
#'                      \item{CumAlphaUpper}{Upper cum. alpha spent. Present in right tailed and two sided tests only }
#'                      \item{CumAlphaLower}{Lower cum. alpha spent. Present in left tailed and two sided tests only }
#'                      \item{EffBdryScale}{Efficacy boundary scale. Possible values are: Z Scale: 0, p-Value Scale: 1}
#'                      \item{EffBdry}{Vector of efficacy boundaries. Present in one sided tests only }
#'                      \item{EffBdryUpper}{Vector of upper efficacy boundaries. Present in right tailed and two sided tests only }
#'                      \item{EffBdryLower}{Vector of lower efficacy boundary. Present in left tailed and two sided tests only }
#'                      \item{FutBdryScale}{Futility boundary scale: Z scale = 0, p-value scale = 1, Delta scale = 2, conditional-power scale = 3, or hazard-ratio scale = 6.}
#'                      \item{FutBdry}{Vector of futility boundaries. Present in one sided tests only }
#'                      \item{FutBdryUpper}{Vector of upper futility boundaries. Present in left tailed and two sided tests only }
#'                      \item{FutBdryLower}{Vector of lower futility boundaries. Present in right tailed and two sided tests only }
#'                      \item{CPDeltaOption}{Conditional-power treatment-effect option: 0 for design Delta or 1 for estimated Delta.}
#'                      \item{BindingType}{Futility binding type: 0 for non-binding or 1 for binding.}
#'                 }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'   \describe{
#'     \item{UserParam$bReturnLogTrueHazard}{Logical indicating whether to return the natural logarithm of `TrueHR`. Defaults to `FALSE`.}
#'     \item{UserParam$bReturnNAForNoGoTrials}{Logical indicating whether to return `NA` for trials without an efficacy decision. Defaults to `FALSE`.}
#'   }
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
