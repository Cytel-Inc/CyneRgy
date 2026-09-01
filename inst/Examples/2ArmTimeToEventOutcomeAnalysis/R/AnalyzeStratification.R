######################################################################################################################## .
#' @name AnalyzeStratification
#' @title Analyze a Stratified Time-to-Event Outcome
#' @author Anoop Singh Rawat, Shubham Lahoti, and Gabriel Potvin
#'
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#' \itemize{
#'   \item{ArrivalTime}{Calendar time at which the subject entered the trial.}
#'   \item{SurvivalTime}{Time-to-event outcome measured from subject arrival.}
#'   \item{TreatmentID}{Treatment assignment, where 0 is control and 1 is experimental treatment.}
#' }
#'
#' @param DesignParam List of design and simulation parameters needed to compute test statistics and perform testing. Access elements by name, for example `DesignParam$Alpha`, rather than by position.
#' \itemize{
#'   \item{MaxEvents}{Maximum number of events for a fixed-sample analysis.}
#'   \item{CriticalPoint}{Single-look efficacy boundary when `LookInfo` is `NULL`.}
#'
#'   Stratification parameters:
#'   \item{NumStratFactors}{— Number of stratification factors used in the analysis}
#'   \item{TestStratFactors}{— Subset of stratification factors to be used specifically for testing (may include \code{NA})}
#'   \item{StratFactors}{— A list of stratification factor levels, where each element corresponds
#'         to a stratification variable.
#'         For example:
#'         \itemize{
#'            \item{\code{Var1}}{— Levels for stratification variable 1 (e.g., \code{c("1","2")})}
#'            \item{\code{Var2}}{— Levels for stratification variable 2 (e.g., \code{c("1","2")})}
#'         }}
#' }
#'
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
#' A list of user-defined parameters in East Horizon. Default = NULL.
#'
#' @description
#' Computes a stratified log-rank test, hazard ratio, analysis time and decision
#' at a given interim analysis.
#'
#' \enumerate{
#'   \item Prepares observed data up to the interim analysis time
#'   \item Computes the test statistic
#'   \item Computes the HR
#'   \item Generates a decision at the current look (efficacy, continue, or futility at final look)
#' }
#'
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#' \describe{
#'   \item{Decision}{An integer value indicating the outcome of the analysis:
#'     \itemize{
#'       \item{Decision = 0}{when No boundary, futility or efficacy is crossed}
#'       \item{Decision = 1}{when the Lower Efficacy Boundary Crossed}
#'       \item{Decision = 2}{when the Upper Efficacy Boundary Crossed}
#'       \item{Decision = 3}{when the Futility Boundary Crossed}
#'       \item{Decision = 4}{when the Equivalence Boundary Crossed}
#'     }}
#'   \item{TestStat}{**Optional.** A numeric (double) value representing the teststatistic}
#'   \item{HR}{**Optional.** A double value containing the computed HR.}
#'   \item{AnalysisTime}{**Optional.** Numeric value. Estimate of Analysis time. Same as look time for interims. Same as study duration for the final analysis. To be computed and returned by the user.}
#'   \item{ErrorCode}{**Optional.** An integer value:
#'     \itemize{
#'       \item{0}{— No error}
#'       \item{>0}{— Non-fatal error (current iteration aborted)}
#'       \item{<0}{— Fatal error (simulation terminated)}
#'     }}
#' }
######################################################################################################################## .

## AnalyzeStratification() : Returning Test Stat, HR and Analysis Time and Decision
AnalyzeStratification<- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{

    nError <- 0
    nDecision <- 0
    dTestStatistic <- 0
    dTimeOfAnalysis <- 0

    # Determine number of events for analysis
    if( !is.null( LookInfo ) )
    {
        nQtyOfLooks  <- LookInfo$NumLooks
        nLookIndex   <- LookInfo$CurrLookIndex
        vCumEvents    <- LookInfo$CumEvents
        nQtyOfEvents <- vCumEvents[ nLookIndex ]
    }
    else
    {
        nQtyOfLooks  <- 1
        nLookIndex   <- 1
        nQtyOfEvents <- DesignParam$MaxEvents
    }

    # Prepare analysis dataset
    SimData$TimeOfEvent  <- SimData$ArrivalTime + SimData$SurvivalTime
    SimData              <- SimData[ order( SimData$TimeOfEvent ), ]
    dTimeOfAnalysis      <- SimData[ nQtyOfEvents, ]$TimeOfEvent
    SimData              <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ]
    SimData$Event        <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )
    SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )

    vObservedTime <- SimData$ObservedTime
    vEvent        <- SimData$Event
    vTreatmentID  <- SimData$TreatmentID

    # Determine which stratification factors to use
    if( !all( is.na( DesignParam$TestStratFactors ) ) )
    {
        vStratFactors <- DesignParam$TestStratFactors
    }
    else
    {
        # For Design, as TestStratFactors is NA
        vStratFactors <- names( DesignParam$StratFactors )
    }

    # Convert each stratification column to a factor
    for( strFactor in vStratFactors )
    {
        SimData[[ strFactor ] ] <- factor( SimData[[ strFactor ] ],
                                           levels = unique( SimData[[ strFactor ] ] ) )
    }

    # Construct the formula for strata dynamically
    fStrataFormula <- stats::as.formula(
        paste0(
            "survival::Surv(vObservedTime, vEvent) ~ vTreatmentID + ",
            paste0( "survival::strata(`", vStratFactors, "`)", collapse = " + " )
        )
    )

    # Perform stratified log-rank test
    cSurvDiff      <- survival::survdiff( fStrataFormula, data = SimData )
    dTestStatistic <- sqrt( cSurvDiff$chisq )

    # Compute Hazard Ratio (HR)
    cCoxFit <- survival::coxph( fStrataFormula, data = SimData )
    dHR     <- exp( coef( cCoxFit ) )

    dTestStatistic <- ifelse( unname( dHR ) < 1, dTestStatistic * -1, dTestStatistic )

    # Decision logic based on boundaries
    if( !is.na( dTestStatistic ) )
    {
        if( !is.null( LookInfo ) )
        {
            # Use efficacy boundary from LookInfo if available
            if( !is.null( LookInfo$EffBdry ) )
            {
                dEffBdry  <- LookInfo$EffBdry[ nLookIndex ]
                nDecision <- ifelse( is.nan( dEffBdry ) | is.na( dEffBdry ), 0,
                                     ifelse( dTestStatistic > dEffBdry, 2, 0 ) )
            }
        }
        else
        {
            # Use fixed design boundary
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

    lRet <- list( TestStat     = as.double( dTestStatistic ),
                 AnalysisTime = as.double( dTimeOfAnalysis ),
                 HR           = as.double( dHR ),
                 Decision     = as.integer( nDecision ),
                 ErrorCode    = as.integer( nError ) )
    return( lRet )
}
