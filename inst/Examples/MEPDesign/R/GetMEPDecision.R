#' User Design Function for Trial Analysis
#'
#' @name GetMEPDecision
#' @description
#' This function allows users to implement custom decision-making logic for the MEP engine.
#' It is called at each analysis look to make decisions about endpoint efficacy, futility
#' and trial continuation based on user-defined criteria.
#'
#' @param SimData Data frame containing the full simulation data for all patients with the following columns:
#'   \itemize{
#'     \item SimID: Integer. Simulation ID
#'     \item PatId: Integer. Patient ID identifying a unique patient in a given simulation
#'     \item ArrivalTime: Numeric. Patient arrival time in the study
#'     \item TreatmentID: Integer. ID for the treatment assigned to the patient. 0 - Control; 1 - Treatment
#'     \item TResponse.EPNAME: Numeric. Response on this endpoint. EPNAME is the user specified endpoint name
#'     \item CensorID.EPNAME: Integer. Whether the patient is censored or not. 0 - censored; 1 - not censored. EPNAME is the user specified endpoint name
#'     \item Response.EPNAME: Numeric. Response on this endpoint after adjusting for endpoint and dropout rules. EPNAME is the user specified endpoint name
#'     \item CalRespT.EPNAME: Numeric. Response time on calendar scale for this endpoint. Same as 'Response + ArrivalTime'. EPNAME is the user specified endpoint name
#'     \item DropoutID.EPNAME: Integer. Whether the patient dropped out before responding to the endpoint. 1 - dropout; 0 - no dropout. EPNAME is the user specified endpoint name
#'   }
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
#'     \item AnalysisTime: Numeric. Current analysis time
#'     \item LookNum: Integer. Current look number
#'     \item TestStatisticsOutputs: Named list of test statistics results from MEP native engine. Names are user-specified endpoint names. Each element contains:
#'       \itemize{
#'         \item data: List containing test statistics (varies by endpoint type and test):
#'           \itemize{
#'             \item For TTE endpoints: Score (numeric), StdErr (numeric standard error), TS (numeric test statistic), TSPVal (numeric p-value)
#'             \item For Binary endpoints: PropPld (numeric pooled proportion), StdErr (numeric standard error), TS (numeric test statistic), TSPVal (numeric p-value)
#'             \item For Continuous endpoints: DoF (numeric degrees of freedom), SDPld (numeric pooled standard deviation), StdErr (numeric standard error), TS (numeric test statistic), TSPVal (numeric p-value)
#'           }
#'         \item ErrorCode: Integer. Error code from computation. 0=Success
#'       }
#'     \item InfoFrac: Unnamed List of numeric vectors. Each element contains the actual information fractions up to the current look for one endpoint. Element order matches EndpointName order
#'     \item LastLookDecision: Integer vector. Decisions from previous look for each endpoint. 0=Continue, 1=Efficacy, 2=Futility
#'     \item EfficacyBoundaryPScale: Numeric vector. Final set of efficacy boundaries on p-value scale used for testing each endpoint by native engine. NaN where boundaries were not calculated. Value order matches EndpointName order
#'     \item EPStatus: Integer vector. Status of each endpoint. 0=Success, 1=Insufficient Information, 2=Excessive Information, 3=Computational Error
#'   }
#' @param DesignParam List containing trial design parameters:
#'   \itemize{
#'     \item TotalLooks: Integer. Total number of planned looks
#'     \item EndpointName: Character vector. Names of endpoints
#'     \item EndpointType: Integer vector. Types of endpoints. 0=Continuous, 1=Binary, 2=TTE
#'     \item TailType: Integer vector. Tail type for hypothesis tests. 0=Left tail, 1=Right tail
#'     \item NumPat: Integer. Number of patients
#'     \item AllocRatio: Numeric. Ratio of Number of Patients on Treatment to Control
#'     \item TrialType: Integer vector. Type of trial. 0=Superiority
#'     \item Alpha: Numeric. Type I error rate
#'     \item TestStatistics: Named list of test statistics specifications for each endpoint. Names are user-specified endpoint names. Each element contains:
#'       \itemize{
#'         \item Test: Integer. Test type. 0=None, 1=LogRank (TTE), 2=Difference of Means (Continuous), 3=Difference of Proportions (Binary), 4=Ratio of Proportions (Binary)
#'         \item TestStat: Integer. Test statistic. 0=None, 1=LogRank (TTE), 3=Harrington Fleming (TTE), 4=t Statistics (Continuous), 5=z Statistics (Binary), 6=Modestly Weighted LogRank/MWLR (TTE)
#'         \item Variance: Integer (Binary/Continuous only). 1=Pooled (Binary), 2=Unpooled (Binary), 3=Equal (Continuous), 4=Unequal (Continuous)
#'         \item Parameter: Numeric vector (for Harrington Fleming or MWLR). For Harrington Fleming: c(p,q). For MWLR: c(delay,w_max)
#'       }
#'     \item TargetInformation: Integer vector. Target information for each endpoint. Completers for continuous/binary and Events for TTE
#'     \item MultiplicityDetails: List containing multiplicity adjustment settings. Contains:
#'       \itemize{
#'         \item MCP: Integer. Multiple comparison procedure. 0=None, 1=Fallback, 2=Fixed Sequence, 3=Bonferroni/Weighted Bonferroni, 4=Holms, 5=Weighted Holms, 6=User Specified GMCP
#'         \item AlphaAlloc: Numeric vector. Alpha allocation percentages for each endpoint (must match EndpointName order).
#'         \item TestOrder: Integer vector (for Fallback/Fixed Sequence only). Testing order for each endpoint (must match EndpointName order).
#'         \item TransMat: Numeric matrix (for GMCP only). Transition matrix for alpha propagation (rows/columns must match EndpointName order).
#'       }
#'     \item EffFlg: Integer matrix. Efficacy flag matrix indicating which endpoint is selected for efficacy testing at which analysis. 1=Test for efficacy, 0=Do not test. Rows represent analyses in analysis order, columns represent endpoints in EndpointName order
#'     \item FutFlg: Integer matrix. Futility flag matrix indicating which endpoint is selected for futility check at which analysis. 1=Check for futility, 0=Do not check. Rows represent analyses in analysis order, columns represent endpoints in EndpointName order
#'     \item FutThrsld: Numeric matrix. Futility thresholds for each endpoint at each analysis. For TTE endpoints, threshold represents HR (declare futility if HR > threshold). For non-TTE endpoints, threshold represents Delta (declare futility if Delta < threshold). Rows represent analyses in analysis order, columns represent endpoints in EndpointName order. Values are 0 when futility check is not performed (FutFlg=0)
#'     \item EffSpending: Named list of alpha spending function specifications for each endpoint. Names are user-specified endpoint names. Each element contains:
#'       \itemize{
#'         \item EffBdry: Integer. Efficacy boundary type. 0=None, 1=Spending Function
#'         \item SpendFunc: Integer (when EffBdry=1). Spending function type. 1=Lan-DeMets (LD), 2=Gamma
#'         \item Parameter: Numeric (when EffBdry=1). For LD: 1=O'Brien-Fleming (OF), 2=Pocock (PK). For Gamma: numeric values (e.g., 1, -3)
#'       }
#'     \item WinCondCriteria: List containing trial winning condition criteria. Contains:
#'       \itemize{
#'         \item whichEPs: Integer vector. Flag indicating which endpoints are considered for winning condition (must match EndpointName order). 0=Not considered, 1=Considered
#'         \item NumEPsWin: Integer. Number of endpoints that must be won. Can be 0 if MustWinEPs are the only criteria
#'         \item MustWinEPs: Integer vector. Flag indicating which endpoints must be won for trial success (must match EndpointName order). 0=Not required, 1=Must win
#'       }
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
#' # Example implementation for a trial with an arbitrary number of endpoints. Check for Futility only where Efficacy is checked
GetMEPDecision <- function(SimData, AnalysisData, DataSummary, LookInfo, DesignParam, OutList, UserParam) {
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
