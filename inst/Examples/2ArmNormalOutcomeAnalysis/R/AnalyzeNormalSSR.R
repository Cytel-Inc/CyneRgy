######################################################################################################################## .
#' @name AnalyzeNormalSSR
#' @title Analyze a Continuous Outcome with Sample Size Re-Estimation
#' @author Shubham Lahoti, J. Kyle Wathen, and Gabriel Potvin
#'
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#'        \describe{
#'          \item{ArrivalTime}{ A numeric value with the time the patient arrived in the trial}
#'          \item{TreatmentID}{An integer value where 0 indicates control treatment and 1 experimental treatment.}
#'          \item{Response}{A numeric value indicating the response.}
#'          \item{CensorIndOrg}{An integer value indicating whether the subject was censored or not.}
#'        }
#' @param DesignParam List of design and simulation parameters needed to compute test statistics and perform testing. Access elements by name, for example `DesignParam$Alpha`, rather than by position.
#'      \describe{
#'          \item{SampleSize}{Sample size of the trial}
#'          \item{Alpha}{Type I Error}
#'          \item{TestType}{Values are One side: 0; Two Sided: 1, Two Sided, Asymmetric: 2}
#'          \item{TailType}{Values are Left Tailed: 0, Right Tailed: 1}
#'          \item{LowerAlpha}{Lower Type I error. Present for Left Tailed and Two Sided Asymmetric Tests }
#'          \item{UpperAlpha}{Upper Type I error. Present for Right Tailed and Two Sided Asymmetric Tests }
#'          \item{MaxCompleters}{Maximum Number of Completers}
#'          \item{FollowUpType}{Follow-up type: 0 for until the end of the study, or 1 for a fixed period.}
#'          \item{AllocInfo}{Vector of ratios of treatment sample sizes to control sample size. Length = Number of treatment arms. }
#'          \item{CriticalPoint}{Critical Value. Present in Fixed Sample designs only }
#'          \item{UpperCriticalPoint}{Upper Critical Value. Present in Right Tail Fixed Sample designs only }
#'          \item{LowerCriticalPoint}{Lower Critical Value. Present in Left Tail Fixed Sample designs only }
#'          \item{FollowUpDur}{Follow up duration}
#'          \item{TrtEffNull}{Treatment Effect under Null on natural scale. Applicable for Non-inferiority trials.}
#'
#'      }
#' @param LookInfo List of parameters for the current analysis look. It is `NULL` for fixed-sample designs. Access elements by name, for example `LookInfo$NumLooks`, rather than by position.
#'                 \describe{
#'                      \item{NumLooks}{An integer value with the number of looks in the study}
#'                      \item{CurrLookIndex}{An integer value with the current index look, starting from 1}
#'                      \item{CumCompleters}{Cumulative number of completer for all non time-to-event studies.}
#'                      \item{InfoFrac}{Information fraction}
#'                      \item{RejType}{Rejection type identifying the enabled efficacy and futility boundaries.}
#'                      \item{CumAlpha}{Cumulative alpha spent. Present in one sided tests only }
#'                      \item{CumAlphaUpper}{Upper cum. alpha spent. Present in right tailed and two sided tests only }
#'                      \item{CumAlphaLower}{Lower cum. alpha spent. Present in left tailed and two sided tests only }
#'                      \item{EffBdryScale}{Efficacy boundary scale.  Possible values are: Z Scale: 0, p-Value Scale: 1}
#'                      \item{EffBdry}{Vector of efficacy boundaries. Present in one sided tests only }
#'                      \item{EffBdryUpper}{Vector of upper efficacy boundaries. Present in right tailed and two sided tests only }
#'                      \item{EffBdryLower}{Vector of lower efficacy boundary. Present in left tailed and two sided tests only }
#'                      \item{FutBdryScale}{Futility boundary scale. Possible value are:  Z Scale: 0, p-Value Scale: 1, Delta Scale: 2, Conditional Power Scale: 3}
#'                      \item{FutBdry}{Vector of futility boundaries. Present in one sided tests only }
#'                      \item{FutBdryUpper}{Vector of upper futility boundaries. Present in left tailed and two sided tests only }
#'                      \item{FutBdryLower}{Vector of lower futility boundaries. Present in right tailed and two sided tests only }
#'                      \item{CPDeltaOption}{Conditional-power treatment-effect option: 0 for design Delta or 1 for estimated Delta.}
#'                      \item{BindingType}{Futility binding type: 0 for non-binding or 1 for binding.}
#'                 }
#' @param AdaptInfo List containing sample-size re-estimation parameters:
#'   \describe{
#'     \item{SSRFuncScale}{Rule type: 0 for a continuous rule or 1 for a step-function rule.}
#'     \item{PromZoneMin}{Lower bound of the promising zone for continuous SSR.}
#'     \item{PromZoneMax}{Upper bound of the promising zone.}
#'     \item{MaxSSMult}{Maximum sample-size multiplier.}
#'     \item{MaxSSMultInp}{List containing `From`, `To`, and `MaxSSMult` values for step-function rules.}
#'   }
#'
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' A list of user-defined parameters in East Horizon. Default = NULL.
#'
#' @description
#' Implements continuous-outcome analysis with conditional power–based sample size re-estimation (SSR).
#' The function:
#' \enumerate{
#'   \item Prepares observed data up to the interim analysis time
#'   \item Computes the standardized test statistic
#'   \item Computes conditional power using the design boundary
#'   \item Determines re-estimated completers using a continuous or step-function SSR rule
#'   \item Generates a decision at the current look (efficacy, continue, or futility at final look)
#' }
#'
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#' \describe{
#'   \item{Decision}{**Required.** Integer value indicating the outcome of the analysis.
#'     \itemize{
#'       \item{Decision = 0}{when No boundary, futility or efficacy is crossed}
#'       \item{Decision = 1}{when the Lower Efficacy Boundary Crossed}
#'       \item{Decision = 2}{when the Upper Efficacy Boundary Crossed}
#'       \item{Decision = 3}{when the Futility Boundary Crossed}
#'       \item{Decision = 4}{when the Equivalence Boundary Crossed}
#'     }}
#'   \item{TestStat}{**Optional.** A numeric (double) value representing the teststatistic}
#'   \item{ReEstCompleters}{**Required.** Integer value of the **re-estimated total completers** based on the Sample Size Re-estimation (SSR) rule.}
#'   \item{Delta}{**Optional.** Numeric value representing the observed **mean difference**:
#'     \deqn{\Delta = \text{mean(Treatment)} - \text{mean(Control)}}}
#'   \item{AnalysisTime}{**Optional.** Numeric value. Estimate of Analysis time. Same as look time for interims. Same as study duration for the final analysis. To be computed and returned by the user.}
#'   \item{ErrorCode}{**Optional.** Integer code representing execution status:
#'     \itemize{
#'       \item{0}{— No error}
#'       \item{>0}{— Non-fatal error (current iteration aborted)}
#'       \item{<0}{— Fatal error (simulation terminated)}
#'     }}
#' }
######################################################################################################################## .

AnalyzeNormalSSR <- function( SimData, DesignParam, LookInfo = NULL, AdaptInfo = NULL, UserParam = NULL )
{
    nError <- 0
    nDecision <- 0
    dTestStatistic <- 0
    dDelta <- NA
    dSE <- NA
    dAnalysisTime <- 0

    ###########################################################
    ## Step 1 — Data Preparation and Analysis Time Computation
    ###########################################################
    if( !is.null( LookInfo ) )
    {
        nQtyOfLooks      <- LookInfo$NumLooks
        nLookIndex       <- LookInfo$CurrLookIndex
        vCumCompleters   <- LookInfo$InfoFrac * DesignParam$MaxCompleters
        nQtyOfCompleters <- vCumCompleters[ nLookIndex ]
    }
    else
    {
        nQtyOfLooks      <- 1
        nLookIndex       <- 1
        nQtyOfCompleters <- DesignParam$MaxCompleters
    }

    SimData$CalendarResponseTime <- SimData$ArrivalTime + DesignParam$RespLag
    SimData <- SimData[ order( SimData$CalendarResponseTime ), ]

    dAnalysisTime <- SimData[ nQtyOfCompleters, ]$CalendarResponseTime

    SimData <- SimData[ SimData$ArrivalTime <= dAnalysisTime, ]

    SimData$Completers <- ifelse( SimData$CalendarResponseTime > dAnalysisTime, 0, 1 )

    SimData$ObservedTime <- ifelse(
        SimData$CalendarResponseTime > dAnalysisTime,
        dAnalysisTime - SimData$ArrivalTime,
        SimData$CalendarResponseTime - SimData$ArrivalTime
    )

    SimData <- SimData[ order( SimData$ObservedTime ), ]
    SimDataCurrLook <- subset( SimData, SimData$ArrivalTime <= dAnalysisTime + 1e-4 )

    ###########################################################
    ## Step 2 — Test Statistic And Delta Computation
    ###########################################################
    vOutcome <- SimDataCurrLook$Response
    vTreat   <- SimDataCurrLook$TreatmentID

    vCtrl <- vOutcome[ vTreat == 0 ]
    vTrt  <- vOutcome[ vTreat == 1 ]

    dDelta <- mean( vTr ) - mean( vCtrl )
    dSE    <- sqrt( var( vTr ) / length( vTr ) + var( vCtrl ) / length( vCtrl ) )

    if( !is.na( dDelta ) && !is.na( dSE ) && dSE > 0 )
    {
        dTestStatistic <- dDelta / dSE
    }
    else
    {
        dTestStatistic <- NA
    }

    ###########################################################
    ## Step 3 — Conditional Power Computation
    ###########################################################
    dOrigCp <- NA

    if( !is.na( dTestStatistic ) )
    {

        # Z-crit
        if( !is.null( LookInfo ) && !is.null( LookInfo$EffBdry ) )
        {
            dZcrit <- LookInfo$EffBdry[ nLookIndex ]
        }

        # Info fraction
        if( !is.null( LookInfo ) )
        {
            dTau <- LookInfo$InfoFrac[ nLookIndex ]
        }

        # Conditional power
        dOrigCp <- 1 - pnorm( ( dZcrit - dTestStatistic * sqrt( dTau ) ) /
                             sqrt( 1 - dTau + 1e-12 ) )
    }

    ###########################################################
    ## Step 4 — Re-estimated Completers Computation
    ###########################################################
    if( AdaptInfo$SSRFuncScale == 0 )
    {
        ### Continuous
        if( is.na( dOrigCp ) )
        {
            nReEstCompleters <- DesignParam$MaxCompleters
        }
        else if( dOrigCp > AdaptInfo$PromZoneMin && dOrigCp < AdaptInfo$PromZoneMax )
        {
            nReEstCompleters <- DesignParam$MaxCompleters * AdaptInfo$MaxSSMultInp$MaxSSMult
        }
        else
        {
            nReEstCompleters <- DesignParam$MaxCompleters
        }

    }
    else if( AdaptInfo$SSRFuncScale == 1 )
    {
        ### Step Function
        if( is.na( dOrigCp ) )
        {
            nReEstCompleters <- DesignParam$MaxCompleters
        }
        else
        {

            vStepLowerBound <- AdaptInfo$MaxSSMultInp$From
            vStepUpperBound <- AdaptInfo$MaxSSMultInp$To
            vStepMultiplier <- AdaptInfo$MaxSSMultInp$MaxSSMult

            ## Find which interval dOrigCp falls into
            vIdx <- which( dOrigCp > vStepLowerBound & dOrigCp <= vStepUpperBound )

            if( length( vIdx ) == 0 )
            {
                nReEstCompleters <- DesignParam$MaxCompleters
            }
            else
            {
                nReEstCompleters <- DesignParam$MaxCompleters * vStepMultiplier[ vIdx ]
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
        # If no efficacy, check for futility at final look
        if( nDecision == 0 && nLookIndex == nQtyOfLooks )
        {
            nDecision <- 3
        }
    }

    ###########################################################
    ## Step 6 — Return Output
    ###########################################################
    return( list(
        Decision         = as.integer( nDecision ),
        TestStat         = as.double( dTestStatistic ),
        ReEstCompleters  = as.integer( nReEstCompleters ),
        Delta            = as.double( dDelta ),
        AnalysisTime     = as.double( dAnalysisTime ),
        ErrorCode        = as.integer( nError )
    ) )
}
