#' @param SimulatePatientOutcomeStratification
#'
#' @param NumSub The total number of subjects in the trial. A single numeric value, e.g., 250.
#'
#' @param NumArm The number of arms in the trial (single numeric value).  
#' For a two-arm trial this will be 2.
#'
#' @param ArrivalTime A vector of subject arrival times. (Not used in this function but required for integration.)
#'
#' @param TreatmentID A vector of treatment IDs assigned to subjects.  
#' TreatmentID uses 0-based indexing internally:  
#' \itemize{
#'   \item{0 = Arm 1 (control)}
#'   \item{1 = Arm 2 (experimental)}
#' }  
#' Length of TreatmentID must equal NumSub.
#'
#' @param StratumID A vector indicating the stratum for each subject.  
#' Subjects sharing the same value belong to the same stratum.
#'
#' @param SurvMethod This value is pulled from the Input Method drop-down list.  
#' Allowed values:
#' \itemize{
#'   \item{1 = Hazard Rates (direct)}
#'   \item{2 = Cumulative \% Survival}
#'   \item{3 = Median Survival Times}
#' }
#'
#' @param NumPrd Number of time periods provided in the survival parameter table.
#'
#' @param PrdTime
#' \describe{
#'   \item{If SurvMethod = 1}{PrdTime is a vector of starting times of hazard pieces.}
#'   \item{If SurvMethod = 2}{Times at which cumulative % survivals are specified.}
#'   \item{If SurvMethod = 3}{Period time is 0 by default.}
#' }
#'
#' @param SurvParam  
#' A 2-D array providing survival parameters per stratum.  
#' Each row corresponds to **one stratum**, and each column corresponds to an arm:  
#'
#' \describe{
#'
#'   \item{If SurvMethod = 1}{SurvParam stores hazard rates (one per arm per stratum).  
#'   SurvParam[i, j] = hazard rate for stratum *i* and arm *j*.}
#'
#'   \item{If SurvMethod = 2}{SurvParam stores cumulative % survival values per arm.  
#'   SurvParam[i, j] = cumulative % survival for stratum *i* and arm *j*.}
#'
#'   \item{If SurvMethod = 3}{SurvParam stores median survival times per arm.  
#'   SurvParam[i, j] = median survival time for stratum *i* and arm *j*.}
#'
#' }
#'
#' @param UserParam A list of user-defined parameters in East/East Horizon (not used in this function).  
#' The default is NULL.
#'
#'
#' @description
#' This function generates patient survival times across multiple strata based on the
#' parameters specified in the Response Generation table.  
#'
#' For each stratum, the corresponding survival parameters (hazard rates, cumulative % survival, or medians)
#' are converted into hazard rates. Then patient-level survival times are simulated using an
#' Exponential distribution:
#' \deqn{ T \sim \text{Exponential}(\lambda) }
#'
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed. 
#'             \describe{
#'             \item{Response}{Required numeric value. Contains a vector of generated Survival Time for all subjects across the strata}
#'             \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }

#'
#' @export
SimulatePatientOutcomeStratification <- function(NumSub, NumArm, ArrivalTime, TreatmentID, 
                                                StratumID, SurvMethod, NumPrd, PrdTime, 
                                                SurvParam, UserParam = NULL)
{
    nError <- 0
    
    # Initialize vectors
    vSurvResponses <- numeric(0)
    vUniqueStrata <- unique(StratumID)
    
    # Loop through strata
    for(nStratumIdx in seq_along(vUniqueStrata))
    {
        nStratumInd <- vUniqueStrata[nStratumIdx]
        
        # Number of subjects in this stratum
        nStratumSubjects <- sum(StratumID == nStratumInd)
        
        # Response Gen params in this stratum
        vStratumParams <- SurvParam[nStratumIdx, ]
        
        vPatientOutcome <- rep(0, nStratumSubjects)
        vTreatmentIndex <- TreatmentID + 1  # TreatmentID is 0-based
        
        # SurvMethod 1: Hazard Rates
        if(SurvMethod == 1)
        {
          vHazardRates <- vStratumParams
        }
        
        # SurvMethod 2: Cumulative % Survival
        if(SurvMethod == 2)
        {
          dSurvTime <- as.numeric(PrdTime[1])
          vS <- vStratumParams / 100
          vHazardRates <- rep(NA, NumArm)
          
          for(nArmIdx in 1:NumArm)
          {
            if(vS[nArmIdx] > 0 && vS[nArmIdx] < 1 && dSurvTime > 0)
              vHazardRates[nArmIdx] <- -log(vS[nArmIdx]) / dSurvTime
            else {
              vHazardRates[nArmIdx] <- NA
              nError <- 1
            }
          }
        }
        
        # SurvMethod 3: Median Survival
        if(SurvMethod == 3)
        {
          vMedian <- vStratumParams
          vHazardRates <- rep(NA, NumArm)
          
          for(nArmIdx in 1:NumArm)
          {
            if(vMedian[nArmIdx] > 0)
              vHazardRates[nArmIdx] <- log(2) / vMedian[nArmIdx]
            else {
              vHazardRates[nArmIdx] <- NA
              nError <- 1
            }
          }
        }
        
        # Generation of Responses 
        for(nPatIndx in 1:nStratumSubjects)
        {
            nArm <- vTreatmentIndex[nPatIndx]
            dRate <- vHazardRates[nArm]
    
            if(!is.na(dRate) && dRate > 0)
                vPatientOutcome[nPatIndx] <- rexp(1, dRate)
            else
                vPatientOutcome[nPatIndx] <- NA
        }
    
        # Append strata-wise responses
        vSurvResponses <- c(vSurvResponses, vPatientOutcome)
    }

    # For consistency checks
    if(length(vSurvResponses) != NumSub || any(is.na(vSurvResponses)))
        nError <- -100
    
    return(list(
        SurvivalTime = as.double(vSurvResponses),
        ErrorCode = as.integer(nError)
    ))
}
