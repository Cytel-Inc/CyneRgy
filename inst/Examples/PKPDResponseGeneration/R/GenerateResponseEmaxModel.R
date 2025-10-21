######################################################################################################################## .
#' @name GenerateResponseEmaxModel
#' @title Simulate Treatment Effect with Emax Model
#' 
#' @description
#' Generate drug concentrations per subject per visit, then applies the Emax equation to convert per-visit plasma concentrations into treatment responses using the Emax PD model.
#' 
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
#' @param UserParam User can pass custom scalar variables defined by users as a member of this list. User should access the variables using names, for example UserParam$Var1 and not order. 
#' \describe{
#'   \item{ka}{Absorption rate constant}
#'   \item{ke}{Elimination rate constant}
#'   \item{Dose}{Dose administered}
#'   \item{E0}{Baseline effect}
#'   \item{Emax}{Maximum effect}
#'   \item{EC50}{Concentration at 50% of Emax}
#' }
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
#' @export    
######################################################################################################################## .

GenerateResponseEmaxModel <- function( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL ) {
    nError <- 0
    lRetval <- list()
    
    # Initialize simulated response matrix
    mResponses <- matrix( 0, nrow = NumSub, ncol = NumVisit )
    
    # Define the Emax model parameters from UserParam
    E0   <- UserParam$E0    # Baseline effect
    Emax <- UserParam$Emax  # Maximum effect
    EC50 <- UserParam$EC50  # Concentration at 50% of Emax
    ka   <- UserParam$ka    # Absorption rate constant
    ke   <- UserParam$ke    # Elimination rate constant
    Dose <- UserParam$Dose  # Dose administered
    
    # Check if all required Emax parameters are provided
    if ( is.null( E0 ) || is.null( Emax ) || is.null( EC50 ) || is.null( ka ) || is.null( ke ) || is.null( Dose )) {
        nError <- -1 # Fatal error if required parameters are missing
        lRetval$ErrorCode <- as.integer( nError )
        return( lRetval )
    }
    
    # Call PK function to get concentration responses for treatment group
    lPkResult <- GenerateDrugConcentration( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam )

    # Simulate response for each patient
    for ( nPatIndx in 1:NumSub ) {
        
        for ( nVisitIndx in 1:NumVisit ) {
            Cp <- lPkResult[[ paste0( "Response", nVisitIndx ) ]] [ nPatIndx ]
            
            dTreatmentEffect <- E0 + ( Emax * Cp ) / ( EC50 + Cp ) # Calculate Emax
            
            if ( TreatmentID[ nPatIndx ] == 0 ) {
                mResponses[ nPatIndx, nVisitIndx ] <- rnorm( 1, mean = MeanControl[ nVisitIndx ], sd = StdDevControl[ nVisitIndx ] ) # Generates response for control group 
            } else {
                mResponses[ nPatIndx, nVisitIndx ] <- rnorm( 1, mean = dTreatmentEffect, sd = StdDevTrt[ nVisitIndx ] ) # Generates response for treatment group (Emax model output)
            }
        }
    }
    
    # Add responses to return list
    for ( nVisitIndx in 1:NumVisit ) {
        
        lRetval[[ paste0( "Response", nVisitIndx )]] <- as.double( mResponses[ , nVisitIndx ])
    }
    
    lRetval$ErrorCode <- as.integer( nError )
    return( lRetval )
    
}

# Define helper function for PK model generating concentration
GenerateDrugConcentration <- function( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL ) {
    library( deSolve )
    
    # Initialize error code and return list
    nError <- 0
    lRetval <- list()
    
    # Parameters for ODE model
    ka <- UserParam$ka  # Absorption rate constant
    ke <- UserParam$ke  # Elimination rate constant
    Dose <- UserParam$Dose  # Dose administered
    
    # Simulate drug concentration for each subject
    for ( nPatIndx in 1:NumSub ) {
        
        # Initial state: A1 = Dose (amount in absorption compartment), A2 = 0 (concentration in central compartment)
        vState <- c( A1 = Dose, A2 = 0 ) # this is a full dose in absorption compartment, none in central
        vParameters <- c( ka = ka, ke = ke )
        
        # Solve ODE for each visit time
        vConcentration <- numeric( NumVisit ) #prepare a vector (NumVisit length) to store concentrations at each visit
        for ( nVisitIndx in 1:NumVisit ) {
            vTime <- c( 0, VisitTime[ nVisitIndx ])  # Time points for ODE solver
            mResult <- deSolve::ode( y = vState, times = vTime, func = OneCompartmentModelPK, parms = vParameters)
            vState <- mResult[ nrow( mResult ), -1 ]  # Update state for next visit
            vConcentration[ nVisitIndx ] <- vState[ "A2" ]  # Extract concentration at current visit
        }
        
        # Add noise based on treatment group
        if ( TreatmentID[ nPatIndx ] == 0 ) {
            vConcentration <- vConcentration + rnorm( NumVisit, mean = MeanControl, sd = StdDevControl )
        }
        else
        {
            vConcentration <- vConcentration + rnorm( NumVisit, mean = MeanTrt, sd = StdDevTrt )
        }
        
        # Store concentration for each visit
        for ( nVisitIndx in 1:NumVisit ) {
            strVisitName <- paste0( "Response", nVisitIndx )
            if ( !is.null( lRetval[[ strVisitName ]])) {
                lRetval[[ strVisitName ]] <- c( lRetval[[ strVisitName ]], vConcentration[ nVisitIndx ])
            }
            else
            {
                lRetval[[ strVisitName ]] <- vConcentration[ nVisitIndx ]
            }
        }
    }
    
    # Set error code and return results
    lRetval$ErrorCode <- as.integer( nError )
    return( lRetval )
}

# Define helper ODE function for one-compartment model with first-order absorption
OneCompartmentModelPK <- function( time, state, parameters ) {
    with( as.list( c( state, parameters )), {
        
        dA1 <- -ka * A1  # Change in drug amount in absorption compartment
        dA2 <- ( ka * A1 - ke * A2 )  # Change in drug concentration in central compartment
        
        return( list( c( dA1, dA2 )))
    })
}