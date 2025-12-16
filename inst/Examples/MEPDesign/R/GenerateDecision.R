#' User Design Function for Trial Analysis
#'
#' @name GenerateDecision
#' @description
#' This function allows users to implement custom decision-making logic for the MEP engine.
#' It is called at each analysis look to make decisions about endpoint efficacy, futility
#' and trial continuation based on user-defined criteria.
#'
#' @param SimData Data frame containing the full simulation data for all patients
#' @param AnalysisData Data frame containing the subset of patient data available at the current analysis look
#' @param DataSummary List containing summary statistics for each endpoint, including:
#'   \itemize{
#'     \item AvgFollowupTime: Mean follow-up time
#'     \item MedianFollowupTime: Median follow-up time
#'     \item Dropouts: Total number of dropouts
#'     \item Dropouts0/1: Number of dropouts in control/treatment groups
#'     \item Censored: Total number of censored observations
#'     \item Censored0/1: Number of censored in control/treatment groups
#'     \item Pendings: Total number of pending events/completers
#'     \item Pendings0/1: Number of pending in control/treatment groups
#'     \item Events/Completers: Total number of events (TTE) or completers (non-TTE)
#'     \item Events0/1/Completers0/1: Events/completers by treatment group
#'   }
#' @param LookInfo List containing information about the current analysis look:
#'   \itemize{
#'     \item AnalysisTime: Current analysis time
#'     \item LookNum: Current look number
#'     \item TestStatisticsOutputs: Test statistics results from MEP native engine
#'     \item InfoFrac: Information fraction
#'     \item LastLookDecision: Decisions from previous look
#'     \item EfficacyBoundaryPScale: Efficacy boundaries on p-value scale
#'     \item EPStatus: Vector containing Status of each endpoint. 0=Success,1=Insufficient Information,2=Excessive Information,3=Computational Error
#'   }
#' @param DesignParam List containing trial design parameters:
#'   \itemize{
#'     \item TotalLooks: Total number of planned looks
#'     \item EndpointName: Names of endpoints
#'     \item EndpointType: Types of endpoints. 0=Continuous, 1=Binary, 2=TTE
#'     \item TailType: Tail type for hypothesis tests. 0=Left tail, 1=Right tail
#'     \item NumPat: Number of patients
#'     \item AllocRatio: Treatment allocation ratio
#'     \item TrialType: Type of trial
#'     \item Alpha: Type I error rate
#'     \item TestStatistics: Test statistics specifications
#'     \item TargetInformation: Target information
#'     \item MultiplicityDetails: Multiplicity adjustment details
#'     \item EffFlg: Efficacy flag matrix
#'     \item FutFlg: Futility flag matrix
#'     \item FutThrsld: Futility thresholds
#'     \item EffSpending: Alpha spending details
#'     \item WinCondCriteria: Win condition criteria
#'   }
#' @param OutList List containing any persistent data to be passed between looks
#' @param UserParam Optional list of user-defined parameters
#'
#' @return A list containing:
#'   \itemize{
#'     \item Decision: Mandatory output. Vector of decisions for each endpoint (0=Continue, 1=Efficacy, 2=Futility)
#'     \item TestStat: Named list of test statistics for each endpoint (optional)
#'     \item RawPVal: Vector of raw p-values for each endpoint (optional)
#'     \item EfficacyBoundary: Vector of efficacy boundaries (optional)
#'     \item WinStatus: Trial win status (optional). 0=No Decision, 1=Win, -1=Lose
#'     \item Score: Vector of score statistics (optional)
#'     \item StdErr: Vector of Standard errors (optional)
#'     \item PropPld: Vector of pooled proportion (optional)
#'     \item SDPld: Vector of pooled standard deviation (optional)
#'     \item ErrorCode: Error code if any errors occurred (optional)
#'   }
#' @examples
#' # Example implementation for a trial with 5 endpoints. Check for Futility only where Efficacy is checked
GenerateDecision <- function(SimData, AnalysisData, DataSummary, LookInfo, DesignParam, OutList, UserParam) {
  # Initialize Decision with last look's decisions
  Decision <- LookInfo$LastLookDecision

  # Get number of endpoints
  nNumEP <- length(DesignParam$EndpointName)

  # For each endpoint
  for (nEPID in 1:nNumEP) {
    # Only process if last look decision was Continue (0)
    if (Decision[nEPID] == 0) {
      # Get efficacy boundary and p-value for current endpoint
      boundary <- LookInfo$EfficacyBoundaryPScale[nEPID]
      pvalue <- LookInfo$TestStatisticsOutputs[[nEPID]]$data$TSPVal

      # Check if both boundary and p-value exist and p-value is less than boundary
      if (!is.nan(boundary) && !is.nan(pvalue)) {
        if (pvalue < boundary) {
          Decision[nEPID] <- 1  # Set to Efficacy
          next
        } else {
            # Check for futility if efficacy is not declared
            nCurrentLook <- LookInfo$LookNum
            if (DesignParam$FutFlg[nCurrentLook, nEPID] == 1) {
                # Get futility threshold for this endpoint at this look
                futThreshold <- DesignParam$FutThrsld[nCurrentLook, nEPID]
                
                # Get endpoint name and type
                strEPName <- DesignParam$EndpointName[nEPID]
                nEPType <- DesignParam$EndpointType[nEPID]
                
                # Get HR or Delta from DataSummary based on endpoint type
                if (nEPType == 2) {  # TTE endpoint
                    observedValue <- DataSummary[[strEPName]]$HR
                    # Declare futility if HR > threshold
                    if (!is.nan(observedValue)) {
                        if (observedValue > futThreshold) Decision[nEPID] <- 2  # Set to Futility
                    }
                } else {  # Non-TTE endpoint
                    observedValue <- DataSummary[[strEPName]]$Delta
                    # Declare futility if Delta < threshold
                    if (!is.nan(observedValue)) {
                        if (observedValue < futThreshold) Decision[nEPID] <- 2  # Set to Futility
                    }
                }
            }
        }
      } 
    }

    # If still not stopped, check if we've reached or exceeded target information
    if (Decision[nEPID] == 0) {
      strEPName <- DesignParam$EndpointName[nEPID]
      nEPType <- DesignParam$EndpointType[nEPID]
      targetInfo <- DesignParam$TargetInformation[nEPID]

          # Get current events/completers count based on endpoint type
      currentCount <- if (nEPType == 2) {
        DataSummary[[strEPName]]$Events  # For TTE endpoints
      } else {
        DataSummary[[strEPName]]$Completers  # For binary and continuous endpoints
      }

      # Check if we've reached or exceeded target information
      if (currentCount >= targetInfo) Decision[nEPID] <- 2  # Set to Futility

      # Check if there is insufficient information
      if (LookInfo$EPStatus[nEPID] == 1) Decision[nEPID] <- 2 # Set to Futility

      # Check if this is the last efficacy look for this endpoint
      nCurrentLook <- LookInfo$LookNum
      # Check if there are any efficacy looks (1s) after current look for this endpoint
      futureEfficacyLooks <- if (nCurrentLook < nrow(DesignParam$EffFlg)) {
        any(DesignParam$EffFlg[(nCurrentLook + 1):nrow(DesignParam$EffFlg), nEPID] == 1)
      } else {
        FALSE
      }

      # If this is the last efficacy look (no more 1s after this) and current look has efficacy testing
      if (!futureEfficacyLooks && DesignParam$EffFlg[nCurrentLook, nEPID] == 1) {
        Decision[nEPID] <- 2  # Set to Futility
      }
    }
  }
  lRet <- list(Decision = Decision)

  return(lRet)
}
