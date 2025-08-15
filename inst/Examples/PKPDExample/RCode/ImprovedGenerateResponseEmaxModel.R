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
GenerateResponseEmaxModel <- function(NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL) {
  Error <- 0
  retval <- list()
  
  # # Check if correlation matrix is valid
  # if (!is.matrix(CorrMat) || nrow(CorrMat) != NumVisit || ncol(CorrMat) != NumVisit) {
  #   Error <- -1
  #   retval$ErrorCode <- as.integer(Error)
  #   return(retval)
  # }
  
  
  # Initialize simulated response matrix
  mResponses <- matrix(0, nrow = NumSub, ncol = NumVisit)
  
  # Define the Emax model parameters from UserParam
  E0   <- UserParam$E0    # Baseline effect
  Emax <- UserParam$Emax  # Maximum effect
  EC50 <- UserParam$EC50  # Concentration at 50% of Emax
  C0 <- UserParam$Concentration # Starting concetration
  Ke   <- UserParam$Ke    # Elimination rate constant

  # Check if all required Emax parameters are provided
  if (is.null(E0) || is.null(Emax) || is.null(EC50) || is.null(C0) || is.null(Ke)) {
    Error <- -2
    retval$ErrorCode <- as.integer(Error)
    return(retval)
  }
  
  # Use this to calculate the concetration at each visit using first-order elimination
  # C(t) = Dose0 * exp(-Ke * t)
  Cp <- C0 * exp(-Ke * VisitTime)
  
  # Calculate treatment effect using the Emax model formula
  TreatmentEffect <- E0 + (Emax * Cp) / (EC50 + Cp)
  
  
  # Simulate responses for each visit
  for (i in 1:NumVisit) {
    # Generates response for control group 
    mResponses[TreatmentID == 0, i] <- rnorm( n = sum(TreatmentID == 0), mean = MeanControl[i], sd = StdDevControl[i] )
    
    # Generates response for treatment group (Emax model output)
    mResponses[TreatmentID == 1, i] <- rnorm( n = sum(TreatmentID == 1), mean = TreatmentEffect[i], sd = StdDevTrt[i])
  }
  
  # Add responses to return list
  for (i in 1:NumVisit) {
    retval[[paste0("Response", i)]] <- as.double(mResponses[, i])
  }
  
  retval$ErrorCode <- as.integer(Error)
  return(retval)
  
}

# Test Call
UserParam <- list(E0 = 5, Emax = 20, EC50 = 50, Concentration = 100, Ke = 0.2)
VisitTime <- c(0, 1, 2, 4, 8)  # in hours or days
TreatmentID <- rep(c(0,1), each = 5)

res <- GenerateResponseEmaxModel(
    NumSub = 10,
    NumVisit = length(VisitTime),
    TreatmentID = TreatmentID,
    Inputmethod = NULL,
    VisitTime = VisitTime,
    MeanControl = rep(5, length(VisitTime)),
    StdDevControl = rep(1, length(VisitTime)),
    StdDevTrt = rep(1.5, length(VisitTime)),
    UserParam = UserParam
)

res
