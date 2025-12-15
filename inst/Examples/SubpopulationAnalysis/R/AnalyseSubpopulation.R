#' @name AnalyseSubpopulation
#' 
#' ASSUMPTION
#' The look positioning is based on target events on Full Population
#'
#' @param SimData 
#' A data frame containing the simulated patient-level data for the current simulation iteration.  
#' Must include at least the following variables:
#' \itemize{
#'   \item{ArrivalTime}{— The calendar time at which the subject entered the trial}
#'   \item{Response}{— The observed endpoint for continuous outcome}
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
#' %% Subpopulation Analysis Parameters
#'   \item{NumSubPops}{— Number of predefined subpopulations included in the analysis}
#'
#'   \item{SubpopName}{— A vector of subpopulation names or identifiers
#'         (e.g., \code{c("SP1","SP2","SP3")})}
#'
#'   \item{WinCond}{— A list specifying the win conditions for each subpopulation.
#'         Each element corresponds to a subpopulation and defines the criteria
#'         used to determine whether a treatment arm “wins” within that group.
#'         For example:
#'         \itemize{
#'            \item{\code{SP1}}{— Win condition settings for Subpopulation 1}
#'            \item{\code{SP2}}{— Win condition settings for Subpopulation 2}
#'            \item{\code{SP3}}{— Win condition settings for Subpopulation 3}
#'         }}
#' 
#'   \item{PlanEndTrial}{— A logical flag or condition vector indicating whether
#'         the trial should be considered complete for each subpopulation at
#'         the planned analysis points (e.g., \code{TRUE} / \code{FALSE})}
#'
#'   \item{TransitionMatrix}{— A transition matrix or list of matrices defining
#'         how probabilities or subjects transition between states or
#'         subpopulations (if applicable).
#'         For example:
#'         \itemize{
#'            \item{\code{SP1}}{— Transition matrix for Subpopulation 1}
#'            \item{\code{SP2}}{— Transition matrix for Subpopulation 2}
#'            \item{\code{SP3}}{— Transition matrix for Subpopulation 3}
#'         }}
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
#' @param UserParam 
#' A list of user-defined parameters in East or East Horizon.  
#' Default = NULL.
#'
#' @description
#' Computes teststat, hazard ratio, analysis time and decision   
#' at a given interim analysis while having multiple subpopulations.  
#'
#' This function:
#' \enumerate{
#'   \item Determines the number of events required at the current interim look
#'         (based on \code{LookInfo} if provided; otherwise on \code{DesignParam}).
#'
#'   \item Prepares the analysis dataset by:
#'         \itemize{
#'           \item Computing event times and observed follow-up
#'           \item Ordering subjects by event time to determine analysis cutoff
#'           \item Censoring subjects whose events occur after the analysis time
#'           \item Restricting subjects to those enrolled before the cutoff
#'         }
#'
#'   \item Reads all pre-specified population definitions:
#'         \itemize{
#'           \item Full population
#'           \item Subpopulations in \code{DesignParam$SubPops}
#'           \item Alpha allocation weights for GMCP
#'         }
#'
#'   \item Constructs logical filters that identify subjects belonging to the
#'         full population and each subpopulation.
#'
#'   \item Identifies all stratification factors used by any subpopulation and
#'         ensures they are appropriately factorized.
#'
#'   \item For each population (full population + all subpops), it:
#'         \itemize{
#'           \item Selects the applicable stratification factors
#'           \item Constructs a dynamic stratified log-rank formula
#'           \item Computes the standardized test statistic (sqrt of chi-square) 
#'           \item Fits a stratified Cox model and extracts the hazard ratio (HR)
#'         }
#'
#'   \item Collects population-specific test statistics and applies the graphical
#'         multiple testing procedure via \code{compute_gMCPDecisions()}.
#'
#'   \item Converts GMCP rejection flags into population-specific decision codes:
#'         \itemize{
#'           \item \code{2} = reject null hypothesis (efficacy)
#'           \item \code{0} = continue at interim look
#'           \item \code{3} = no rejection at final look (futility)
#'         }
#'
#'   \item Returns all computed outputs for each population:
#'         \itemize{
#'           \item Test statistic
#'           \item Hazard ratio
#'           \item GMCP decision code
#'         }
#'
#'   \item Returns the overall analysis time at which the interim or final
#'         evaluation was conducted, along with an error flag.
#' }


#'
#'
#' @return Decision  
#' An integer value indicating the outcome of the analysis:
#' \itemize{
#'   \item{0}{— Continue (no boundary crossed)}
#'   \item{2}{— Efficacy boundary crossed}
#'   \item{3}{— Futility at the final look (if no efficacy signal)}
#' }
#'
#' @return TestStat  
#' **Optional.**A double value containing the computed Test Stat.
#' @return HR  
#' **Optional.**A double value containing the computed HR.
#'
#' @return AnalysisTime  
#' **Optional.**A double value representing the calendar time at which the analysis was conducted.
#
#' @return ErrorCode  
#' **Optional.** An integer value:
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
#' }
#'
#' These can then be loaded into an R session for detailed debugging.
#'
#' @export

library(survival)
AnalyseSubpopulation <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL) 
{
  nError <- 0
  dTimeOfAnalysis <- 0
  
  # Step 1: Determine number of events for analysis
  if (!is.null(LookInfo)) {
    nQtyOfLooks  <- LookInfo$NumLooks
    nLookIndex   <- LookInfo$CurrLookIndex
    vCumEvents    <- LookInfo$CumEvents
    nQtyOfEvents <- vCumEvents[nLookIndex]
  } else {
    nQtyOfLooks  <- 1
    nLookIndex   <- 1
    nQtyOfEvents <- DesignParam$MaxEvents 
  }
  
  # Step 2: Prepare analysis dataset
  SimData$TimeOfEvent  <- SimData$ArrivalTime + SimData$SurvivalTime    
  SimData              <- SimData[order(SimData$TimeOfEvent), ]
  dTimeOfAnalysis      <- SimData[nQtyOfEvents, ]$TimeOfEvent
  SimData              <- SimData[SimData$ArrivalTime <= dTimeOfAnalysis, ]
  SimData$Event        <- ifelse(SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1)
  SimData$ObservedTime <- ifelse(SimData$TimeOfEvent > dTimeOfAnalysis, 
                                 dTimeOfAnalysis - SimData$ArrivalTime, 
                                 SimData$TimeOfEvent - SimData$ArrivalTime)
  
  # Step 3: Reading populations Inputs 
  nNumSubPops <- DesignParam$NumSubPops
  PopNames   <- DesignParam$SubpopName
  SubPops    <- DesignParam$SubPops
  dPropAlpha  <- DesignParam$PropAlpha
  
  # Step 4: Create population filters
  PopFilters <- list()
  PopFilters[["Full Population"]] <- rep(TRUE, nrow(SimData))
  
  if (nNumSubPops > 0) {
    for (subpop_name in names(SubPops)) {
      subpop_filter <- rep(TRUE, nrow(SimData))
      for (factor_name in names(SubPops[[subpop_name]])) {
        allowed_values <- SubPops[[subpop_name]][[factor_name]]
        subpop_filter <- subpop_filter & (SimData[[factor_name]] %in% allowed_values)
      }
      PopFilters[[subpop_name]] <- subpop_filter
    }
  }
  
  # Step 5: Determine all possible factors across all subpops
  all_factors <- unique(unlist(lapply(SubPops, names)))
  for (fac in all_factors) {
    if (fac %in% names(SimData)) {
      SimData[[fac]] <- factor(SimData[[fac]], levels = unique(SimData[[fac]]))
    }
  }
  
  # Step 6: Initialize output lists
  dTestStatistic <- list()
  dHR <- list()
  nDecision <- list()
  
  vtestStats_vec <- c()
  vpopOrder <- c()
  
  # Step 7: Compute test stats and collect populations
  for (pop_name in names(PopFilters)) {
    
    subset_data <- SimData[PopFilters[[pop_name]], ]
    
    if (nrow(subset_data) > 0) {
      
      # Identify strat factors
      if (pop_name == "Full Population") {
        current_strat_factors <- all_factors
      } else {
        current_strat_factors <- names(SubPops[[pop_name]])
      }
      current_strat_factors <- current_strat_factors[current_strat_factors %in% names(subset_data)]
      
      # Build survival formula
       if (length(current_strat_factors) > 0) {
      strata_formula <- as.formula(
  paste0(
    "Surv(ObservedTime, Event) ~ TreatmentID + ",
    paste0("strata(`", current_strat_factors, "`)", collapse = " + ")
  )
)

      } else {
        strata_formula <- Surv(ObservedTime, Event) ~ TreatmentID
      }
      
      
      # HR
      dcox_fit <- coxph(strata_formula, data = subset_data)
      dhr <- exp(coef(dcox_fit))
      
      # Log-rank test
      dfit <- survdiff(strata_formula, data = subset_data)
      test_stat <- sqrt(dfit$chisq)
      test_stat <- ifelse(unname(dhr) < 1, test_stat * -1, test_stat)
      
      
      # Store outputs in named lists
      dTestStatistic[[pop_name]] <- as.double(test_stat)
      dHR[[pop_name]] <- as.double(dhr)
      
      # Store test stats for GMCP
      vtestStats_vec <- c(vtestStats_vec, test_stat)
      vpopOrder <- c(vpopOrder, pop_name)
      
    } else {
      # Missing population
      dTestStatistic[[pop_name]] <- NA
      dHR[[pop_name]] <- NA
      vtestStats_vec <- c(vtestStats_vec, NA)
      vpopOrder <- c(vpopOrder, pop_name)
    }
  }
  
  # Step 8: Compute GMCP decisions
  gmcp_result <- compute_gMCPDecisions(
    testStats = vtestStats_vec,
    tailType  = DesignParam$TailType,
    alpha     = DesignParam$Alpha,
    weights   = DesignParam$PropAlpha,
    tpm       = DesignParam$TransitionMatrix
  )
  
  # Step 9: Map GMCP decisions to population and store in named list
  for (i in seq_along(vpopOrder)) {
    
    pop_name <- vpopOrder[i]
    
    gmcp_flag <- gmcp_result$decisionFlag[i]   # 0 = no reject, 1 = reject
    
    if (gmcp_flag == 1) {
      final_decision <- 2
    } else {
      final_decision <- if (nLookIndex == nQtyOfLooks) 3 else 0
    }
    
    nDecision[[pop_name]] <- as.integer(final_decision)
  }
  
  # Step 10: Return results
  # The return line must remain unchanged
  lRet <- list(
    Decision = as.list(nDecision),
    TestStat = as.list(dTestStatistic),
    dHR = as.list(dHR),
    AnalysisTime = as.double(dTimeOfAnalysis),
    ErrorCode = as.integer(nError)
  )
  
  return(lRet)
}

library(survival)
compute_gMCPDecisions <- function(testStats, tailType, alpha, weights, tpm) {
  require(gMCPLite) 
  
  #browser()
  
  isTestStatMissing <- is.nan(testStats) | is.na(testStats);
  if(any(isTestStatMissing)) {
    testStats[which(isTestStatMissing == TRUE)] <- ifelse(tailType == "Left-Tail", Inf, -Inf)
  } 
  
  #Computing raw p-values
  if(tailType == 0) {
    raw.p.values <- pnorm(q=testStats, lower.tail=TRUE)
  } else {
    raw.p.values <- pnorm(q=testStats, lower.tail=FALSE)
  }
  
  #Creating graph object
  graph <- matrix2graph(m=tpm, weights = weights)
  
  #Applying gMCP procedure
  #As per the current scope for Sub-Init RoADMAP-22, we are using weighted-bonferroni as a local test.
  output <- gMCP(graph=graph, pvalues = raw.p.values, test="Bonferroni", alpha = alpha)
  
  return(list(raw.p.values=raw.p.values, adj.p.values=output@adjPValues, 
              decisionFlag= as.numeric(output@rejected)) ) #While returning output, converting logical decison flags to numeric
}
