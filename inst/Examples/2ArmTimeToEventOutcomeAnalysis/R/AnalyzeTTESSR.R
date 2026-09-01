######################################################################################################################## .
#' @name AnalyzeTTESSR
#' @title Analyze a Time-to-Event Outcome with Event Re-Estimation
#' @author Anoop Singh Rawat, Shubham Lahoti, and Gabriel Potvin
#'
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
#' @param AdaptInfo List containing event-count and sample-size re-estimation parameters:
#'   \describe{
#'     \item{SSRFuncScale}{Rule type: 0 for a continuous rule or 1 for a step-function rule.}
#'     \item{PromZoneMin}{Lower bound of the promising zone for continuous SSR.}
#'     \item{PromZoneMax}{Upper bound of the promising zone.}
#'     \item{MaxEventsMult}{Maximum event-count multiplier.}
#'     \item{MaxSSMult}{Maximum sample-size multiplier.}
#'     \item{MaxSSMultInp}{List containing `From`, `To`, `MaxEventsMult`, and `MaxSSMult` values for step-function rules.}
#'   }
#'
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' A list of user-defined parameters in East Horizon. Default = NULL.
#'
#' @description
#' Implements time-to-event (TTE) analysis with conditional power–based event re-estimation (SSR).
#' The function:
#' \enumerate{
#'   \item Prepares observed data up to the current analysis time
#'   \item Computes the log-rank–style test statistic and hazard ratio (HR)
#'   \item Computes conditional power using the design boundary
#'   \item Determines re-estimated events using a continuous or step-function SSR rule
#'   \item Generates a decision at the current look (efficacy, continue, or futility at final look)
#' }
#'
#' This function assumes proportional hazards and uses the Cox model for hazard ratio estimation.
#'
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#' \describe{
#'   \item{Decision}{**Required.** Integer value indicating the outcome of the analysis.
#'     \itemize{
#'       \item{Decision = 0}{when No boundary, futility or efficacy is  crossed}
#'       \item{Decision = 1}{when the Lower Efficacy Boundary Crossed}
#'       \item{Decision = 2}{when the Upper Efficacy Boundary Crossed}
#'       \item{Decision = 3}{when the Futility Boundary Crossed}
#'       \item{Decision = 4}{when the Equivalence Boundary Crossed}
#'     }}
#'   \item{TestStat}{**Optional.** A numeric (double) value representing the teststatistic}
#'   \item{ReEstEvents}{**Required.** Integer value of the **re-estimated events** based on the Sample Size Re-estimation (SSR) rule.}
#'   \item{HR}{**Optional.** Numeric value representing the observed **hazard ratio**:
#'     \deqn{HR = \frac{\text{hazard(Treatment)}}{\text{hazard(Control)}}}
#'     Estimated using a Cox proportional hazards model for time-to-event data.}
#'   \item{AnalysisTime}{**Optional.** Numeric value. Estimate of Analysis time. Same as look time for interims. Same as study duration for the final analysis. To be computed and returned by the user.}
#'   \item{ErrorCode}{**Optional.** Integer code representing execution status:
#'     \itemize{
#'       \item{0}{— No error}
#'       \item{>0}{— Non-fatal error (current iteration aborted)}
#'       \item{<0}{— Fatal error (simulation terminated)}
#'     }}
#' }
#'
######################################################################################################################## .

AnalyzeTTESSR <- function( SimData, DesignParam, LookInfo = NULL, AdaptInfo = NULL, UserParam = NULL )
{
    nError         <- 0
    nDecision      <- 0
    dTestStatistic <- 0
    dAnalysisTime  <- 0

    ###########################################################
    ## Step 1 — Data Preparation and Analysis Time Computation
    ###########################################################

    if( !is.null( LookInfo ) )
    {
        nQtyOfLooks   <- LookInfo$NumLooks
        nLookIndex    <- LookInfo$CurrLookIndex
        vCumEvents    <- LookInfo$InfoFrac * DesignParam$MaxEvents
        nQtyOfEvents  <- vCumEvents[ nLookIndex ]
    }
    else
    {
        nQtyOfLooks   <- 1
        nLookIndex    <- 1
        nQtyOfEvents  <- DesignParam$MaxEvents
    }

    SimData$TimeOfEvent <- SimData$ArrivalTime + SimData$SurvivalTime
    SimData <- SimData[ order( SimData$TimeOfEvent ), ]
    dAnalysisTime <- SimData[ nQtyOfEvents, ]$TimeOfEvent

    SimData <- SimData[ SimData$ArrivalTime <= dAnalysisTime, ]
    SimData$Event <- ifelse( SimData$TimeOfEvent > dAnalysisTime, 0, 1 )
    SimData$ObservedTime <- ifelse(
        SimData$TimeOfEvent > dAnalysisTime,
        dAnalysisTime - SimData$ArrivalTime,
        SimData$TimeOfEvent - SimData$ArrivalTime
    )

    SimData <- SimData[ order( SimData$ObservedTime ), ]
    SimDataCurrLook <- subset( SimData, SimData$ArrivalTime <= dAnalysisTime + 1e-4 )

    ###########################################################
    ## Step 2 — Test Statistic And HR Computation
    ###########################################################

    nEventsTreatment <- sum( SimDataCurrLook$Event[ SimDataCurrLook$TreatmentID == 1 ] )
    nEventsControl   <- sum( SimDataCurrLook$Event[ SimDataCurrLook$TreatmentID == 0 ] )
    nTotalEvents     <- nEventsTreatment + nEventsControl

    nAtRiskTreatment <- sum( SimDataCurrLook$TreatmentID == 1 )
    nAtRiskControl   <- sum( SimDataCurrLook$TreatmentID == 0 )
    nTotalAtRisk     <- nAtRiskTreatment + nAtRiskControl

    dExpectedTreatment <- nAtRiskTreatment * nTotalEvents / nTotalAtRisk
    dExpectedControl   <- nAtRiskControl * nTotalEvents / nTotalAtRisk

    dVarianceTreatment <- ( nAtRiskTreatment * nAtRiskControl * nTotalEvents * ( nTotalAtRisk - nTotalEvents ) ) /
                        ( nTotalAtRisk ^ 2 * ( nTotalAtRisk - 1 ) )

    dTestStatistic <- ( nEventsTreatment - dExpectedTreatment ) / sqrt( dVarianceTreatment )

    coxModel <- survival::coxph( survival::Surv( ObservedTime, Event ) ~ TreatmentID, data = SimData )
    dHR <- exp( coef( coxModel ) )

    ###########################################################
    ## Step 3 — Conditional Power Computation
    ###########################################################
    dOrigCp <- NA

    if( !is.na( dTestStatistic ) )
    {
        if( !is.null( LookInfo ) && !is.null( LookInfo$EffBdry ) )
        {
            dZCrit <- LookInfo$EffBdry[ nLookIndex ]
        }
        if( !is.null( LookInfo ) )
        {
            dTau <- LookInfo$InfoFrac[ nLookIndex ]
        }
        dOrigCp <- 1 - pnorm( ( dZCrit - dTestStatistic * sqrt( dTau ) ) / sqrt( 1 - dTau + 1e-12 ) )
    }

    ###########################################################
    ## Step 4 — Re-estimated Events Computation
    ###########################################################
    if( AdaptInfo$SSRFuncScale == 0 )
    {
        if( is.na( dOrigCp ) )
        {
            nReEstEvents <- DesignParam$MaxEvents
        }
        else if( dOrigCp > AdaptInfo$PromZoneMin && dOrigCp < AdaptInfo$PromZoneMax )
        {
            nReEstEvents <- DesignParam$MaxEvents * AdaptInfo$MaxSSMultInp$MaxEventsMult
        }
        else
        {
            nReEstEvents <- DesignParam$MaxEvents
        }
    }
    else if( AdaptInfo$SSRFuncScale == 1 )
    {
        if( is.na( dOrigCp ) )
        {
            nReEstEvents <- DesignParam$MaxEvents
        }
        else
        {
            vStepLowerBound <- AdaptInfo$MaxSSMultInp$From
            vStepUpperBound <- AdaptInfo$MaxSSMultInp$To
            vStepMultiplier <- AdaptInfo$MaxSSMultInp$MaxEventsMult
            nIdx <- which( dOrigCp > vStepLowerBound & dOrigCp <= vStepUpperBound )
            if( length( nIdx ) == 0 )
            {
                nReEstEvents <- DesignParam$MaxEvents
            }
            else
            {
                nReEstEvents <- DesignParam$MaxEvents * vStepMultiplier[ nIdx ]
            }
        }
    }

    ###########################################################
    ## Step 5 — Decision Computation
    ###########################################################
    if( !is.na( dTestStatistic ) )
    {
        if( !is.null( LookInfo ) )
        {
            if( !is.null( LookInfo$EffBdry ) )
            {
                dEffBdry <- LookInfo$EffBdry[ nLookIndex ]
                nDecision <- ifelse( is.nan( dEffBdry ) | is.na( dEffBdry ), 0,
                                    ifelse( dTestStatistic > dEffBdry, 2, 0 ) )
            }
        }
        else
        {
            if( !is.null( DesignParam$CriticalPoint ) )
            {
                nDecision <- ifelse( dTestStatistic > DesignParam$CriticalPoint, 2, 0 )
            }
        }
        if( nDecision == 0 && nLookIndex == nQtyOfLooks )
        {
            nDecision <- 3
        }
    }

    ###########################################################
    ## Step 6 — Return Output
    ###########################################################
    return( list(
        Decision       = as.integer( nDecision ),
        TestStat       = as.double( dTestStatistic ),
        ReEstEvents    = as.integer( nReEstEvents ),
        HR             = as.double( dHR ),
        AnalysisTime   = as.double( dAnalysisTime ),
        ErrorCode      = as.integer( nError )
    ) )
}
