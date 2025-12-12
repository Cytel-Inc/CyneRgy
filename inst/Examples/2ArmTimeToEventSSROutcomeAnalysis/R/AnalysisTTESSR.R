#' @name AnalysisTTESSR
#'
#' @param SimData 
#' A data frame containing the simulated patient-level data for the current simulation iteration.  
#' Must include at least the following variables:
#' \itemize{
#'   \item{ArrivalTime}{— The calendar time at which the subject entered the trial}
#'   \item{SurvivalTime}{— The simulated survival time for each subject}
#'   \item{TreatmentID}{— 0 = Control, 1 = Treatment}
#' }
#'
#' @param DesignParam 
#' A list containing the design and simulation parameters required for analysis.  
#' Must include:
#' \itemize{
#'   \item{MaxEvents}{— Maximum number of events for the study}
#'   \item{CriticalPoint}{— Single-look efficacy boundary (if LookInfo = NULL)}
#' }
#'
#' @param LookInfo 
#' A list containing group sequential design information for multi-look trials.  
#' This list is optional (default = NULL).  
#' If provided, it must include:
#' \itemize{
#'   \item{NumLooks}{— Total number of interim analyses}
#'   \item{CurrLookIndex}{— Current look index}
#'   \item{InfoFrac}{— Information fraction at each look}
#'   \item{EffBdry}{— Efficacy boundary at each look}
#' }
#'
#' @param AdaptInfo 
#' A list containing sample-size re-estimation parameters, including:
#' \itemize{
#'   \item{SSRFuncScale}{— 0 = continuous rule, 1 = step function}
#'   \item{PromZoneMin}{— Lower bound of promising zone (for continuous SSR)}
#'   \item{PromZoneMax}{— Upper bound of promising zone}
#'   \item{MaxSSMultInp}{— List containing \code{From}, \code{To}, \code{MaxEventsMult} for step rules}
#' }
#'
#' @param UserParam 
#' A list of user-defined parameters in East or East Horizon.  
#' Default = NULL.
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
#' @return Decision  
#' **Required.** Integer value indicating the outcome of the analysis.  
#' \itemize{ 
#'   \item{0}{— Continue (no boundary crossed)}
#'   \item{2}{— Efficacy boundary crossed}
#'   \item{3}{— Futility at the final look (if no efficacy signal)}
#' }
#'
#' @return TestStat  
#' **Optional.** A numeric (double) value representing the teststatistic
#'
#' @return ReEstEvents  
#' **Required.** Integer value of the **re-estimated events**   
#' based on the Sample Size Re-estimation (SSR) rule.
#'
#' @return HR  
#' **Optional.** Numeric value representing the observed **hazard ratio**:  
#' \deqn{HR = \frac{\text{hazard(Treatment)}}{\text{hazard(Control)}}}  
#' Estimated using a Cox proportional hazards model for time-to-event data.
#'
#' @return AnalysisTime  
#' **Optional.** A double value representing the calendar time at which the analysis was conducted.
#'
#' @return ErrorCode  
#' **Optional.** Integer code representing execution status:
#' \itemize{
#'   \item{0}{— No error}
#'   \item{>0}{— Non-fatal error (current iteration aborted)}
#'   \item{<0}{— Fatal error (simulation terminated)}
#' }
#'
#' @note
#' Helpful Hints:  
#' It is often very useful to save the input objects to inspect them manually:
#'
#' \preformatted{
#' saveRDS(SimData,     "SimData.Rds")
#' saveRDS(DesignParam, "DesignParam.Rds")
#' saveRDS(LookInfo,    "LookInfo.Rds")
#' saveRDS(AdaptInfo,   "AdaptInfo.Rds")
#' }
#'
#' These can then be loaded into an R session for detailed debugging.
#'
#' @export


library(survival)

AnalysisTTESSR <- function(SimData, DesignParam, LookInfo = NULL, AdaptInfo = NULL, UserParam = NULL )
{
    nError         <- 0
    nDecision      <- 0
    dTestStatistic <- 0
    dAnalysisTime  <- 0
    
    ###########################################################
    ## Step 1 — Data Preparation and Analysis Time Computation
    ###########################################################
    
    if (!is.null(LookInfo)) {
        nQtyOfLooks   <- LookInfo$NumLooks
        nLookIndex    <- LookInfo$CurrLookIndex
        vCumEvents    <- LookInfo$InfoFrac * DesignParam$MaxEvents
        nQtyOfEvents  <- vCumEvents[nLookIndex]
    } else {
        nQtyOfLooks   <- 1
        nLookIndex    <- 1
        nQtyOfEvents  <- DesignParam$MaxEvents
    }
  
    SimData$TimeOfEvent <- SimData$ArrivalTime + SimData$SurvivalTime
    SimData <- SimData[order(SimData$TimeOfEvent), ]
    dAnalysisTime <- SimData[nQtyOfEvents, ]$TimeOfEvent
    
    SimData <- SimData[SimData$ArrivalTime <= dAnalysisTime, ]
    SimData$Event <- ifelse(SimData$TimeOfEvent > dAnalysisTime, 0, 1)
    SimData$ObservedTime <- ifelse(
        SimData$TimeOfEvent > dAnalysisTime, 
        dAnalysisTime - SimData$ArrivalTime, 
        SimData$TimeOfEvent - SimData$ArrivalTime
    )
    
    SimData <- SimData[order(SimData$ObservedTime), ]
    SimDataCurrLook <- subset(SimData, SimData$ArrivalTime <= dAnalysisTime + 1e-4)

    ###########################################################
    ## Step 2 — Test Statistic And HR Computation
    ###########################################################
    
    nEventsTreatment <- sum(SimDataCurrLook$Event[SimDataCurrLook$TreatmentID == 1])
    nEventsControl   <- sum(SimDataCurrLook$Event[SimDataCurrLook$TreatmentID == 0])
    nTotalEvents     <- nEventsTreatment + nEventsControl

    nAtRiskTreatment <- sum(SimDataCurrLook$TreatmentID == 1)
    nAtRiskControl   <- sum(SimDataCurrLook$TreatmentID == 0)
    nTotalAtRisk     <- nAtRiskTreatment + nAtRiskControl

    dExpectedTreatment <- nAtRiskTreatment * nTotalEvents / nTotalAtRisk
    dExpectedControl   <- nAtRiskControl * nTotalEvents / nTotalAtRisk

    dVarianceTreatment <- (nAtRiskTreatment * nAtRiskControl * nTotalEvents * (nTotalAtRisk - nTotalEvents)) /
                          (nTotalAtRisk^2 * (nTotalAtRisk - 1))

    dTestStatistic <- (nEventsTreatment - dExpectedTreatment) / sqrt(dVarianceTreatment)

    coxModel <- survival::coxph(survival::Surv(ObservedTime, Event) ~ TreatmentID, data = SimData)
    dHR <- exp(coef(coxModel))

    ###########################################################
    ## Step 3 — Conditional Power Computation
    ###########################################################
    dOrigCp <- NA
    
    if (!is.na(dTestStatistic)) {
        if (!is.null(LookInfo) && !is.null(LookInfo$EffBdry)) {
            dZCrit <- LookInfo$EffBdry[nLookIndex]
        }
        if (!is.null(LookInfo)) {
            dTau <- LookInfo$InfoFrac[nLookIndex]
        }
        dOrigCp <- 1 - pnorm((dZCrit - dTestStatistic * sqrt(dTau)) / sqrt(1 - dTau + 1e-12))
    }
    
    ###########################################################
    ## Step 4 — Re-estimated Events Computation
    ###########################################################
    if (AdaptInfo$SSRFuncScale == 0) {
        if (is.na(dOrigCp)) {
            nReEstEvents <- DesignParam$MaxEvents
        } else if (dOrigCp > AdaptInfo$PromZoneMin && dOrigCp < AdaptInfo$PromZoneMax) {
            nReEstEvents <- DesignParam$MaxEvents * AdaptInfo$MaxSSMultInp$MaxEventsMult
        } else {
            nReEstEvents <- DesignParam$MaxEvents
        }
    } else if (AdaptInfo$SSRFuncScale == 1) {
        if (is.na(dOrigCp)) {
            nReEstEvents <- DesignParam$MaxEvents
        } else {
            vStepLowerBound <- AdaptInfo$MaxSSMultInp$From
            vStepUpperBound <- AdaptInfo$MaxSSMultInp$To
            vStepMultiplier <- AdaptInfo$MaxSSMultInp$MaxEventsMult
            nIdx <- which(dOrigCp > vStepLowerBound & dOrigCp <= vStepUpperBound)
            if (length(nIdx) == 0) {
                nReEstEvents <- DesignParam$MaxEvents
            } else {
                nReEstEvents <- DesignParam$MaxEvents * vStepMultiplier[nIdx]
            }
        }
    }

    ###########################################################
    ## Step 5 — Decision Computation
    ###########################################################
    if (!is.na(dTestStatistic)) {
        if (!is.null(LookInfo)) {
            if (!is.null(LookInfo$EffBdry)) {
                dEffBdry <- LookInfo$EffBdry[nLookIndex]
                nDecision <- ifelse(is.nan(dEffBdry) | is.na(dEffBdry), 0,
                                    ifelse(dTestStatistic > dEffBdry, 2, 0))
            }
        } else {
            if (!is.null(DesignParam$CriticalPoint)) {
                nDecision <- ifelse(dTestStatistic > DesignParam$CriticalPoint, 2, 0)
            }
        }
        if (nDecision == 0 && nLookIndex == nQtyOfLooks) {
            nDecision <- 3
        }
    }

    ###########################################################
    ## Step 6 — Return Output
    ###########################################################
    return(list(
        Decision       = as.integer(nDecision),
        TestStat       = as.double(dTestStatistic),
        ReEstEvents    = as.integer(nReEstEvents),
        HR             = as.double(dHR),
        AnalysisTime   = as.double(dAnalysisTime),
        ErrorCode      = as.integer(nError)
    ))
}
