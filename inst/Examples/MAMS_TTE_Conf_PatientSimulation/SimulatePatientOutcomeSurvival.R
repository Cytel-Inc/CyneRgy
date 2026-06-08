
########################################################################################################################
#' @name SimulatePatientOutcomeSurvival
#' @title Simulate survival outcomes for multi-arm clinical trial simulations.
#' @description
#' Generates patient-level survival times under several survival distribution
#' parameterizations for multi-arm clinical trial simulations. The function supports:
#' \describe{
#'   \item{SurvMethod = 1}{Piecewise exponential survival model using hazard rates.}
#'   \item{SurvMethod = 2}{Survival probability driven piecewise exponential model.}
#'   \item{SurvMethod = 3}{Median survival time based exponential model.}
#' }
#'
#' @param NumSub Integer. Total number of subjects.
#' @param NumArm Integer. Number of treatment arms including control.
#' @param ArrivalTime Numeric vector containing patient arrival times.
#' @param TreatmentID Integer vector indicating treatment assignment for each patient.
#'        Control arm must be indexed as 0.
#' @param SurvMethod Integer specifying the survival generation method.
#' @param NumPrd Integer specifying the number of survival periods.
#' @param PrdTime Numeric vector containing period boundary times.
#' @param SurvParam Matrix of survival parameters. Interpretation depends on SurvMethod:
#'        \describe{
#'          \item{Method 1}{Piecewise hazard rates by period and arm.}
#'          \item{Method 2}{Survival probabilities by period and arm.}
#'          \item{Method 3}{Median survival times by arm.}
#'        }
#' @param UserParam Optional user-defined list of custom parameters.
#'
#' @return List containing:
#'         \describe{
#'           \item{SurvivalTime}{Numeric vector of generated survival times.}
#'           \item{ErrorCode}{Integer error code. 0 indicates success and -100 indicates invalid output generation.}
#'         }
########################################################################################################################

SimulatePatientOutcomeSurvival <- function(NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL)
{ 
  Error <- 0
  retval <- c()
  HR <- c()
  if(SurvMethod == 1)
  {
    SF <- matrix(ncol=NumArm, nrow=NumPrd)
    CDF <- matrix(ncol=NumArm, nrow=NumPrd)
    SF[1, ] <- 1
    CDF[1, ] <- 0
    if(NumPrd>1)  
    {
      for(i in 2:NumPrd)
      {
        SF[i,] <- SF[i-1, ]*exp(-SurvParam[i-1,]*(PrdTime[i]-PrdTime[i-1]))
        CDF[i,] <- 1-SF[i,]
      }
    }
    for(m in 1:NumSub)
    {
      j <- TreatmentID[m]
      u1 <- runif(1)
      Index <- max(which(u1>CDF[,j+1]))
      if(Index < length(PrdTime))
        retval[m] <- PrdTime[Index]-(1/SurvParam[Index, j+1])*(log(1-runif(1)*(1-exp(-SurvParam[Index, j+1]*(PrdTime[Index+1]-PrdTime[Index])))))
      if(Index == length(PrdTime))
      {
        retval[m] <- PrdTime[Index]-(1/SurvParam[Index, j+1])*(log(1-runif(1)*(1-(1e-6/SF[Index, j+1]))))
      }
    }
  }
  if(SurvMethod == 2)
  {
    
    HR <- matrix(ncol=NumArm, nrow=NumPrd)
    SF <- matrix(ncol=NumArm, nrow=NumPrd)
    CDF <- matrix(ncol=NumArm, nrow=NumPrd)
    HRAt <- c()
    retval <- c()
    if(NumPrd  == 1)
      HRAt[1] = 0
    if(NumPrd  != 1)
    {
      HRAt[1]=0
      HRAt[2:NumPrd]=PrdTime[1:(NumPrd-1)]
    }
    for(j in 1:NumArm)
    {
      HR[1,j]<- log(1/((SurvParam[1,j]/100)))/PrdTime[1]
      SF[1,j] <- 1-(SurvParam[1,j]/100)
      CDF[1,j] <- 1-SF[1,j]
    }
    if(NumPrd  != 1)
    {
      for(i in 2:NumPrd)
      {
        for(j in 1:NumArm)
        {
          SF[i,j] <- SF[i-1,j]*exp(-HR[i-1,j]*(PrdTime[i]-PrdTime[i-1]))
          HR[i,j]<- log(SF[i-1,j]/((SurvParam[i,j]/100)))/(PrdTime[i]-PrdTime[i-1])
          CDF[i,j] <- 1-SF[i,j]
        }
      }
    }
    for(m in 1:NumSub)
    {
      j <- TreatmentID[m]
      u1 <- runif(1)
      if(length(which(u1>CDF[,j+1])) == 0)
        Index = 1
      if(length(which(u1>CDF[,j+1])) != 0)
        Index <- max(which(u1>CDF[,j+1]))
      if(Index < length(HRAt))
        retval[m] <- HRAt[Index]-(1/HR[Index, j+1])*(log(1-runif(1)*(1-exp(-HR[Index, j+1]*(HRAt[Index+1]-HRAt[Index])))))
      if(Index == length(HRAt))
      {
        retval[m] <- HRAt[Index]-(1/HR[Index, j+1])*(log(1-runif(1)*(1-(1e-6/SF[Index, j+1]))))
      }
    }
  }
  if(SurvMethod == 3)
  {
    
    SF <- matrix(ncol=NumArm, nrow=NumPrd)
    CDF <- matrix(ncol=NumArm, nrow=NumPrd)
    SP <- matrix(ncol=NumArm, nrow=NumPrd)
    SP <- matrix(
      -log(0.5) / SurvParam[1, 1:NumArm],
      nrow = 1
    )
    SF[1, ] <- 1
    CDF[1, ] <- 0
    if(NumPrd>1)
    {
      for(i in 2:NumPrd)
      {
        SF[i,] <- SF[i-1, ]*exp(-SP[i-1,]*(PrdTime[i]-PrdTime[i-1]))
        CDF[i,] <- 1-SF[i,]
      }
    }
    for(m in 1:NumSub)
    {
      j <- TreatmentID[m]
      u1 <- runif(1)
      Index <- max(which(u1>CDF[,j+1]))
      if(Index < length(PrdTime))
        retval[m] <- PrdTime[Index]-(1/SP[Index, j+1])*(log(1-runif(1)*(1-exp(-SP[Index, j+1]*(PrdTime[Index+1]-PrdTime[Index])))))
      if(Index == length(PrdTime))
      {
        retval[m] <- PrdTime[Index]-(1/SP[Index, j+1])*(log(1-runif(1)*(1-(1e-6/SF[Index, j+1]))))
      }
    }
  }
  if(length(retval) !=NumSub || any(is.na(retval)==TRUE))
    Error <- -100
  
  return(list(SurvivalTime = as.double(retval), ErrorCode = as.integer(Error)))
}





