# Check and verify your code before running.
# Run the code to see the results.
# Extract the function before saving or uploading to East Horizon.
# Visit this help page for more information: https://cytel-inc.github.io/CyneRgy/articles/IntegrationPointResponseRepeatedMeasures.html

#' @name GenerateResponseEmaxModel
#' @param NumSub: Mandatory. The integer number of subjects that need to be simulated, integer value. The argument value is passed from Engine.
#' @param NumVisit: Mandatory. Integer number of Visits
#' @param TreatmentID: Mandatory. Array specifying indexes of arms to which subjects are allocated ﴾one arm index per subject. Index for placebo / control is 0.
#' @param Inputmethod: Mandatory. 0 - Actual values : Indicating that user has given mean and SD values for each visit. These are used to generate responses.
#' @param VisitTime: Mandatory. Numeric Visit Times
#' @param MeanControl: Mandatory. Numeric Control Mean for all visits
#' @param MeanTrt: Mandatory. Numeric Treatment Mean for all visits
#' @param StdDevControl: Mandatory. Numeric Control Standard Deviations for all visits
#' @param StdDevTrt: Mandatory. Numeric Treatment Standard Deviations for all visits
#' @param CorrMat: Mandatory. Correlation Matrix between all visits. Matrix of dimension n*n containing numeric values where n is number of visits. 
#' @param UserParam Optional. User can pass custom scalar variables defined by users as a member of this list. 
#'                  User should access the variables using names, for example UserParam$Var1 and not order. 
#'                  These variables can be of the following types: Integer, Numeric, or Character
#' 
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'                  \item{ErrorCode}{ Optional value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'                                     
#'                  \item{Response<NumVisit>}{ A set of arrays of response for all subjects. Each array corresponds to each visit user has specified}             
#'                      
GenerateResponseEmaxModel <- function(NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL) {
  Error <- 0
  retval <- list()
  
  # Check if correlation matrix is valid
  if (!is.matrix(CorrMat) || nrow(CorrMat) != NumVisit || ncol(CorrMat) != NumVisit) {
    Error <- -1
    retval$ErrorCode <- as.integer(Error)
    return(retval)
  }
  
  
  # Initialize response matrix
  mResponses <- matrix(0, nrow = NumSub, ncol = NumVisit)
  
  # Define the Emax model parameters
  E0 <- UserParam$E0  # Baseline effect
  Emax <- UserParam$Emax  # Maximum effect
  EC50 <- UserParam$EC50  # Concentration at 50% of Emax
  Dose <- UserParam$Dose  # Dose level for treatment group
  
  if (is.null(E0) || is.null(Emax) || is.null(EC50) || is.null(Dose)) {
    Error <- -2
    retval$ErrorCode <- as.integer(Error)
    return(retval)
  }
  
  # Calculate treatment effect using the Emax model
  TreatmentEffect <- E0 + (Emax * Dose) / (EC50 + Dose)
  
  
  for (i in 1:NumVisit) {
    # Control group responses
    mResponses[TreatmentID == 0, i] <- rnorm( n = sum(TreatmentID == 0), mean = MeanControl[i], sd = StdDevControl[i] )
    
    # Treatment group responses
    mResponses[TreatmentID == 1, i] <- rnorm( n = sum(TreatmentID == 1), mean = MeanTrt[i] + TreatmentEffect, sd = StdDevTrt[i])
  }
  
  # Add responses to return list
  for (i in 1:NumVisit) {
    retval[[paste0("Response", i)]] <- as.double(mResponses[, i])
  }
  
  retval$ErrorCode <- as.integer(Error)
  return(retval)
  
}

# Example call to the function
# NumSub <- 100
# NumVisit <- 5
# TreatmentID <- c(rep(0, 50), rep(1, 50))
# Inputmethod <- 0
# VisitTime <- c(0, 1, 2, 3, 4)
# MeanControl <- c(10, 12, 14, 16, 18)
# MeanTrt <- c(10, 13, 16, 19, 22)
# StdDevControl <- c(2, 2, 2, 2, 2)
# StdDevTrt <- c(2, 2, 2, 2, 2)
# CorrMat <- diag(5)
# UserParam <- list(E0 = 0, Emax = 10, EC50 = 5, Dose = 10)
# 
# GenerateResponseEmaxModel(NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam)

