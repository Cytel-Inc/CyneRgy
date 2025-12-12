#' @name AnalyseBinomialSSR
#'
#' @param SimData 
#' A data frame containing the simulated patient-level data for the current simulation iteration.  
#' Must include at least the following variables:
#' \itemize{
#'   \item{ArrivalTime}{— The calendar time at which the subject entered the trial}
#'   \item{Response}{— The observed endpoint for binary outcome}
#'   \item{TreatmentID}{— 0 = Control, 1 = Treatment}
#' }
#'
#' @param DesignParam 
#' A list containing the design and simulation parameters required for analysis.  
#' Must include:
#' \itemize{
#'   \item{MaxCompleters}{— Maximum number of completers for the study}
#'   \item{RespLag}{— Response lag from arrival time to measurement}
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
#'   \item{MaxSSMultInp}{— List containing \code{From}, \code{To}, \code{MaxSSMult} for step rules}
#' }
#'
#' @param UserParam 
#' A list of user-defined parameters in East or East Horizon.  
#' Default = NULL.
#'
#' @description
#' Implements binary-outcome analysis with conditional power–based sample size re-estimation (SSR).  
#' The function:
#' \enumerate{
#'   \item Prepares observed data up to the interim analysis time
#'   \item Computes the standardized test statistic
#'   \item Computes conditional power using the design boundary
#'   \item Determines re-estimated completers using a continuous or step-function SSR rule
#'   \item Generates a decision at the current look (efficacy, continue, or futility at final look)
#' }
#'
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
#' @return ReEstCompleters  
#' **Required.** Integer value of the **re-estimated total completers**   
#' based on the Sample Size Re-estimation (SSR) rule.
#'
#' @return Delta  
#' **Optional.** Numeric value representing the observed **proportion difference**:  
#' \deqn{\Delta = p_{\text{Exp}} - p_{\text{Ctrl}}}  
#' where \(p_{\text{Exp}}\) is the observed proportion of responders in the experimental group,  
#' and \(p_{\text{Ctrl}}\) is the observed proportion of responders in the control group.
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

AnalyseBinomialSSR <- function(SimData, DesignParam, AdaptInfo = NULL, LookInfo = NULL, UserParam = NULL )
{
    nError         <- 0
    nDecision      <- 0
    dTestStatistic <- NA
    dAnalysisTime  <- NA

    ###########################################################
    ## Step 1 — Data Preparation and Analysis Time Computation
    ###########################################################
    
    if (!is.null(LookInfo)) {
        nQtyOfLooks       <- LookInfo$NumLooks
        nLookIndex        <- LookInfo$CurrLookIndex
        vCumCompleters    <- LookInfo$InfoFrac * DesignParam$MaxCompleters
        nQtyOfCompleters  <- vCumCompleters[nLookIndex]
    } else {
        nQtyOfLooks      <- 1
        nLookIndex       <- 1
        nQtyOfCompleters <- DesignParam$MaxCompleters
    }
    
    SimData$CalendarResponseTime <- SimData$ArrivalTime + DesignParam$RespLag
    SimData <- SimData[order(SimData$CalendarResponseTime), ]
    dAnalysisTime <- SimData[nQtyOfCompleters, ]$CalendarResponseTime

    SimData <- SimData[SimData$ArrivalTime <= dAnalysisTime, ]
    SimData$Completers <- ifelse(SimData$CalendarResponseTime > dAnalysisTime, 0, 1)
    SimData$ObservedTime <- ifelse(
        SimData$CalendarResponseTime > dAnalysisTime, 
        dAnalysisTime - SimData$ArrivalTime, 
        SimData$CalendarResponseTime - SimData$ArrivalTime
    )

    SimData <- SimData[order(SimData$ObservedTime), ]
    
    # Include patients arriving exactly at analysis time
    SimDataCurrLook <- subset(SimData, SimData$ArrivalTime <= dAnalysisTime + 1e-4)

    ###########################################################
    ## Step 2 — Test Statistic And Delta Computation
    ###########################################################
    
    vPatientOutcome   <- SimDataCurrLook$Response
    vPatientTreatment <- SimDataCurrLook$TreatmentID

    vOutcomesCtrl <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesExp  <- vPatientOutcome[ vPatientTreatment == 1 ]

    nCtrl     <- length(vOutcomesCtrl)
    nExp      <- length(vOutcomesExp)
    nCtrlResp <- sum(vOutcomesCtrl)
    nExpResp  <- sum(vOutcomesExp)

    dCtrlPi <- ifelse(nCtrl > 0, nCtrlResp / nCtrl, NA)
    dExpPi  <- ifelse(nExp > 0, nExpResp / nExp, NA)
    dDelta  <- dExpPi - dCtrlPi

    nTotal     <- nCtrl + nExp
    nTotalResp <- nCtrlResp + nExpResp
    dPooledPi  <- ifelse(nTotal > 0, nTotalResp / nTotal, NA)

    dSE <- sqrt(dPooledPi * (1 - dPooledPi) * (1/nCtrl + 1/nExp))

    if (!is.na(dSE) && dSE > 0 && !is.na(dDelta)) {
        dTestStatistic <- dDelta / dSE
    } else {
        dTestStatistic <- NA
        nError <- 1
    }

    ###########################################################
    ## Step 3 — Conditional Power Computation
    ###########################################################
    
    dOrigCp <- NA
    
    if (!is.na(dTestStatistic)) {
      
        # Z-critical
        if (!is.null(LookInfo) && !is.null(LookInfo$EffBdry)) {
            dZCrit <- LookInfo$EffBdry[nLookIndex]
        }
      
        # Information fraction
        if (!is.null(LookInfo)) {
            dTau <- LookInfo$InfoFrac[nLookIndex]
        }
      
        # Conditional power
        dOrigCp <- 1 - pnorm((dZCrit - dTestStatistic * sqrt(dTau)) /
                              sqrt(1 - dTau + 1e-12))
    }
    
    ###########################################################
    ## Step 4 — Re-estimated Completers Computation
    ###########################################################
    
    if (AdaptInfo$SSRFuncScale == 0) {

        if (is.na(dOrigCp)) {
            nReEstCompleters <- DesignParam$MaxCompleters

        } else if (dOrigCp > AdaptInfo$PromZoneMin &&
                   dOrigCp < AdaptInfo$PromZoneMax) {

            nReEstCompleters <- DesignParam$MaxCompleters *
                                AdaptInfo$MaxSSMultInp$MaxSSMult

        } else {
            nReEstCompleters <- DesignParam$MaxCompleters
        }
      
    } else if (AdaptInfo$SSRFuncScale == 1) {

        if (is.na(dOrigCp)) {
            nReEstCompleters <- DesignParam$MaxCompleters

        } else {
            vStepLowerBound <- AdaptInfo$MaxSSMultInp$From
            vStepUpperBound <- AdaptInfo$MaxSSMultInp$To
            vStepMultiplier <- AdaptInfo$MaxSSMultInp$MaxSSMult
        
            nIdx <- which(dOrigCp > vStepLowerBound &
                          dOrigCp <= vStepUpperBound)
        
            if (length(nIdx) == 0) {
                nReEstCompleters <- DesignParam$MaxCompleters
            } else {
                nReEstCompleters <- DesignParam$MaxCompleters *
                                    vStepMultiplier[nIdx]
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

                nDecision <- ifelse(
                    is.nan(dEffBdry) | is.na(dEffBdry),
                    0,
                    ifelse(dTestStatistic > dEffBdry, 2, 0)
                )
            }

        } else {
            if (!is.null(DesignParam$CriticalPoint)) {
                nDecision <- ifelse(
                    dTestStatistic > DesignParam$CriticalPoint,
                    2, 0
                )
            }
        }

        # Futility rule at final look
        if (nDecision == 0 && nLookIndex == nQtyOfLooks) {
            nDecision <- 3
        }
    }
    
    ###########################################################
    ## Step 6 — Return Output
    ###########################################################

    return(list(
        Decision        = as.integer(nDecision),
        TestStat        = as.double(dTestStatistic),
        ReEstCompleters = as.integer(nReEstCompleters),
        Delta           = as.double(dDelta),
        AnalysisTime    = as.double(dAnalysisTime),
        ErrorCode       = as.integer(nError)
    ))
}
