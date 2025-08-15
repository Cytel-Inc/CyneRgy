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


#PD Model for Emax Response ####
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

# PK model for generating concentration ####

GenerateDrugConcentration <- function(NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL) {
    # Initialize error code and return list
    Error <- 0
    retval <- list()
    
    
    # Parameters for ODE model
    # Parameters and check are temporarily omitted
    
    # ka <- UserParam$ka  # Absorption rate constant
    # ke <- UserParam$ke  # Elimination rate constant
    # Dose <- UserParam$Dose  # Dose administered
    
    # if (is.null(ka) || is.null(ke) || is.null(Dose)) {
    #     Error <- -1  # Fatal error if required parameters are missing
    #     retval$ErrorCode <- as.integer(Error)
    #     return(retval)
    # }
    
    # Hard coded vectors for concentration:
    # Take out once DeSolve is ready
    
    # Control Arm:
    # state <- c( A1 = 500, A2 = 0) 
    # parameters <- c( ka = 1, ke = 0.2)
    # VisitTime <- c( 1,2,3,4,5 )
    # NumVisit <- 5
    vConcentration1 <- c( 281.78192, 311.89025, 186.69717,  84.55619,  31.11674 )
    
    # Experimental Arm:
    # state <- c( A1 = 100, A2 = 0) 
    # parameters <- c( ka = 1, ke = 0.2)
    # VisitTime <- c( 1,2,3,4,5 )
    # NumVisit <- 5
    vConcentration2 <- c( 56.356387, 62.378049, 37.339433, 16.911244,  6.223347 )
    
    # Simulate drug concentration for each subject
    for (i in 1:NumSub) {
        
        # Initial state: A1 = Dose (amount in absorption compartment), A2 = 0 (concentration in central compartment)
        # Note: We are commenting out because EH does not have deSolve package installed.
        
        # state <- c(A1 = Dose, A2 = 0) #this is a full dose in absorption compartment, none in central
        # parameters <- c(ka = ka, ke = ke)
        
        # Solve ODE for each visit time
        # concentration <- numeric(NumVisit) #prepare a vector (NumVisit length) to store concentrations at each visit
        # for (j in 1:NumVisit) {
        #     time <- c(0, VisitTime[j])  # Time points for ODE solver
        #     result <- deSolve::ode(y = state, times = time, func = OneCompartmentModelPK, parms = parameters)
        #     state <- result[nrow(result), -1]  # Update state for next visit
        #     concentration[j] <- state["A2"]  # Extract concentration at current visit
        # }
        
        # Add noise based on treatment group
        if (TreatmentID[i] == 0) {
            concentration <- vConcentration1 + rnorm(NumVisit, mean = MeanControl, sd = StdDevControl)
        } else {
            concentration <- vConcentration2 + rnorm(NumVisit, mean = MeanTrt, sd = StdDevTrt)
        }
        
        # Store concentration for each visit
        for (j in 1:NumVisit) {
            visitName <- paste0("Response", j)
            if (!is.null(retval[[visitName]])) {
                retval[[visitName]] <- c(retval[[visitName]], concentration[j])
            } else {
                retval[[visitName]] <- concentration[j]
            }
        }
    }
    
    # Set error code and return results
    retval$ErrorCode <- as.integer(Error)
    return(retval)
}