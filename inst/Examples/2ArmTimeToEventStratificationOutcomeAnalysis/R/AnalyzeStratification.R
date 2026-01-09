#' @name AnalyzeStratification
#'
#' @param SimData 
#' A data frame containing the simulated patient-level data for the current simulation iteration.  
#' INcludes at least the following variables:
#' \itemize{
#'   \item{ArrivalTime}{— The calendar time at which the subject entered the trial}
#'   \item{Response}{— The observed endpoint for continuous outcome}
#'   \item{TreatmentID}{— 0 = Control, 1 = Treatment}
#' }
#'
#' @param DesignParam 
#' A list containing the design and simulation parameters required for analysis. Includes:
#' \itemize{
#'   \item{MaxCompleters}{— Maximum number of completers for the study}
#'   \item{RespLag}{— Response lag from arrival time to measurement}
#'   \item{CriticalPoint}{— Single-look efficacy boundary (if LookInfo = NULL)}
#'
#'   %% Stratification parameters
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
#' @param LookInfo 
#' A list containing group sequential design information for multi-look trials.  
#' For group sequential designs, it includes:
#' \itemize{
#'   \item{NumLooks}{— Total number of interim analyses}
#'   \item{CurrLookIndex}{— Current look index}
#'   \item{InfoFrac}{— Information fraction at each look}
#'   \item{EffBdry}{— Efficacy boundary at each look}
#' }
#'
#' @param UserParam 
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
#' @export
library(survival)

## AnalyzeStratification() : Returning Test Stat, HR and Analysis Time and Decision
AnalyzeStratification<- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{

    nError <- 0
    nDecision <- 0
    dTestStatistic <- 0
    dTimeOfAnalysis <- 0

    # Step 1: Determine number of events for analysis
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
    
    # Step 2: Prepare analysis dataset
    SimData$TimeOfEvent  <- SimData$ArrivalTime + SimData$SurvivalTime    
    SimData              <- SimData[ order( SimData$TimeOfEvent), ]
    dTimeOfAnalysis      <- SimData[ nQtyOfEvents, ]$TimeOfEvent
    SimData              <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis ,]
    SimData$Event        <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )
    SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )
    
    dtime <- SimData$ObservedTime
    nstatus <- SimData$Event
    ntreatment <- SimData$TreatmentID

    # Determine which stratification factors to use
    if (!all(is.na(DesignParam$TestStratFactors))) {
      strat_factors <- DesignParam$TestStratFactors
    } else {
      # For Design, as TestStratFactors is NA
      strat_factors <- names(DesignParam$StratFactors)
    }

# Convert each stratification column to factor
for (fac in strat_factors) {
  SimData[[fac]] <- factor(SimData[[fac]], levels = unique(SimData[[fac]]))
}

  # Construct the formula for strata dynamically
    strata_formula <- as.formula(
      paste0(
        "Surv(dtime, nstatus) ~ ntreatment + ",
        paste0("strata(`", strat_factors, "`)", collapse = " + ")
      )
    )
 
    # Perform stratified log-rank test
    dfit <- survdiff(strata_formula, data = SimData)
    dTestStatistic <- sqrt(dfit$chisq)
    
    
    # Compute Hazard Ratio (HR)
    dcox_fit <- coxph(strata_formula, data = SimData)
    dhr <- exp(coef(dcox_fit))

    dTestStatistic <- ifelse(unname(dhr) < 1, dTestStatistic * -1, dTestStatistic)
    
    # Step 4: Decision rule
    # Step 7 - Decision logic based on boundaries
    if(!is.na(dTestStatistic)) {
      if(!is.null(LookInfo)) {
        # Use efficacy boundary from LookInfo if available
        if(!is.null(LookInfo$EffBdry)) {
          dEffBdry <- LookInfo$EffBdry[nLookIndex]
          nDecision <- ifelse(is.nan(dEffBdry) | is.na(dEffBdry), 0,
                              ifelse(dTestStatistic > dEffBdry, 2, 0))
        } 
      } else {
        # Use fixed design boundary
        if(!is.null(DesignParam$CriticalPoint)) {
          nDecision <- ifelse(dTestStatistic > DesignParam$CriticalPoint, 2, 0)
        }
      }
      # If no efficacy, check for futility at final look
      if(nDecision == 0 && nLookIndex == nQtyOfLooks) {
        nDecision <- 3
      }
    }
       
    lRet <- list(TestStat = as.double(dTestStatistic),
                 AnalysisTime = as.double(dTimeOfAnalysis),
                 HR = as.double(dhr),
                 Decision  = as.integer(nDecision), 
                 ErrorCode = as.integer(nError))
    return( lRet )
}

