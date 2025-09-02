#' @name GenerateDrugConcentration
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


# Temporarily omitted
#library(deSolve)


# Define ODE function for one-compartment model with first-order absorption
# Note: This function is omitted temporarily until deSovle Package is installed withing EH

# OneCompartmentModelPK <- function(time, state, parameters) {
#     with(as.list(c(state, parameters)), {
#         dA1 <- -ka * A1  # Change in drug amount in absorption compartment
#         dA2 <- (ka * A1 - ke * A2)  # Change in drug concentration in central compartment
#         list(c(dA1, dA2))
#     })
# }

GenerateDrugConcentration <- function(NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL) {
    # Initialize error code and return list
    nError <- 0
    lRetval <- list()
    
    
    # Parameters for ODE model
    # Parameters and check are temporarily omitted
    
    # ka <- UserParam$ka  # Absorption rate constant
    # ke <- UserParam$ke  # Elimination rate constant
    # Dose <- UserParam$Dose  # Dose administered
    
    # if ( is.null( ka ) || is.null( ke ) || is.null( Dose ) ) {
    #     nError <- -1  # Fatal error if required parameters are missing
    #     lRetval$ErrorCode <- as.integer( nError )
    #     return( lRetval )
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
    for ( nPatIndx in 1:NumSub ) {
        
        # Initial state: A1 = Dose (amount in absorption compartment), A2 = 0 (concentration in central compartment)
        # Note: We are commenting out because EH does not have deSolve package installed.
        
        # vState <- c( A1 = Dose, A2 = 0 ) # this is a full dose in absorption compartment, none in central
        # vParameters <- c( ka = ka, ke = ke )
        
        # Solve ODE for each visit time
        # vConcentration <- numeric( NumVisit ) #prepare a vector (NumVisit length) to store concentrations at each visit
        # for ( nVisitIndx in 1:NumVisit ) {
        #     vTime <- c( 0, VisitTime[ nVisitIndx ])  # Time points for ODE solver
        #     mResult <- deSolve::ode( y = vState, times = vTime, func = OneCompartmentModelPK, parms = vParameters)
        #     vState <- mResult[ nrow( result ), -1 ]  # Update state for next visit
        #     vConcentration[ nVisitIndx ] <- vState[ "A2" ]  # Extract concentration at current visit
        # }
        
        # Add noise based on treatment group
        if ( TreatmentID[ nPatIndx ] == 0 ) {
            vConcentration <- vConcentration1 + rnorm( NumVisit, mean = MeanControl, sd = StdDevControl )
        } else {
            vConcentration <- vConcentration2 + rnorm( NumVisit, mean = MeanTrt, sd = StdDevTrt )
        }
        
        # Store concentration for each visit
        for ( nVisitIndx in 1:NumVisit ) {
            strVisitName <- paste0( "Response", nVisitIndx )
            if ( !is.null( lRetval[[ strVisitName ]])) {
                lRetval[[ strVisitName ]] <- c( lRetval[[ strVisitName ]], vConcentration[ nVisitIndx ])
            } else {
                lRetval[[ strVisitName ]] <- vConcentration[ nVisitIndx ]
            }
        }
    }
    
    # Set error code and return results
    lRetval$ErrorCode <- as.integer( nError )
    return( lRetval )
}
