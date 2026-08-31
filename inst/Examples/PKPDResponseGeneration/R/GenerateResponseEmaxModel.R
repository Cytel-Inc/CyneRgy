######################################################################################################################## .
#' @name GenerateResponseEmaxModel
#' @title Simulate Treatment Effect with Emax Model
#' @description
#' Generate drug concentrations per subject per visit, then applies the Emax equation to convert per-visit plasma concentrations into treatment responses using the Emax PD model.
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#' @param NumSub Mandatory integer number of subjects passed by the engine.
#' @param NumVisit Mandatory integer number of visits.
#' @param TreatmentID Mandatory vector of arm indexes, where control is `0`.
#' @param Inputmethod Mandatory input method indicator; `0` denotes actual mean and standard deviation values.
#' @param VisitTime Mandatory numeric vector of visit times.
#' @param MeanControl Mandatory numeric vector of control means by visit.
#' @param MeanTrt Mandatory numeric vector of treatment means by visit.
#' @param StdDevControl Mandatory numeric vector of control standard deviations by visit.
#' @param StdDevTrt Mandatory numeric vector of treatment standard deviations by visit.
#' @param CorrMat Mandatory numeric correlation matrix between visits.
#' @param UserParam User can pass custom scalar variables defined by users as a member of this list. User should access the variables using names, for example UserParam$Var1 and not order.
#' Note: UserParam values should be referenced in the main function before
#' being passed to helper functions. Passing UserParam directly to a helper
#' may prevent East Horizon from automatically populating the required parameters.
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'   \item{AbsorptionRate}{Absorption rate constant}
#'   \item{EliminationRate}{Elimination rate constant}
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
######################################################################################################################## .

GenerateResponseEmaxModel <- function( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL )
{
    nError <- 0
    lRetval <- list()

    # Initialize simulated response matrix
    mResponses <- matrix( 0, nrow = NumSub, ncol = NumVisit )

    # Define the Emax model parameters from UserParam
    E0                <- UserParam$E0               # Baseline effect
    Emax              <- UserParam$Emax             # Maximum effect
    EC50              <- UserParam$EC50             # Concentration at 50% of Emax
    dAbsorptionRate   <- UserParam$AbsorptionRate   # Absorption rate constant
    dEliminationRate  <- UserParam$EliminationRate  # Elimination rate constant
    dDose             <- UserParam$Dose             # Dose administered

    # Check if all required Emax parameters are provided
    if( is.null( E0 ) || is.null( Emax ) || is.null( EC50 ) || is.null( dAbsorptionRate ) || is.null( dEliminationRate ) || is.null( dDose ) )
    {
        nError <- -1 # Fatal error if required parameters are missing
        lRetval$ErrorCode <- as.integer( nError )
        return( lRetval )
    }

    # Call PK function to get concentration responses for treatment group
    lPkResult <- GenerateDrugConcentration( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, dAbsorptionRate, dEliminationRate, dDose )

    # Simulate response for each patient
    for( nPatIndx in 1:NumSub )
    {
        for( nVisitIndx in 1:NumVisit )
        {
            Cp <- lPkResult[[ paste0( "Response", nVisitIndx ) ] ] [ nPatIndx ]

            dTreatmentEffect <- E0 + ( Emax * Cp ) / ( EC50 + Cp ) # Calculate Emax

            if( TreatmentID[ nPatIndx ] == 0 )
            {
                mResponses[ nPatIndx, nVisitIndx ] <- rnorm( 1, mean = MeanControl[ nVisitIndx ], sd = StdDevControl[ nVisitIndx ] ) # Generates response for control group
            }
            else
            {
                mResponses[ nPatIndx, nVisitIndx ] <- rnorm( 1, mean = dTreatmentEffect, sd = StdDevTrt[ nVisitIndx ] ) # Generates response for treatment group (Emax model output)
            }
        }
    }

    # Add responses to return list
    for( nVisitIndx in 1:NumVisit )
    {

        lRetval[[ paste0( "Response", nVisitIndx ) ] ] <- as.double( mResponses[ , nVisitIndx ] )
    }

    lRetval$ErrorCode <- as.integer( nError )
    return( lRetval )

}
# Helper function for PK model generating concentration ####
GenerateDrugConcentration <- function( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, dAbsorptionRate, dEliminationRate, dDose )
{
    # Initialize error code and return list
    nError  <- 0
    lRetval <- list()

    if( is.null( dAbsorptionRate ) || is.null( dEliminationRate ) || is.null( dDose ) )
    {
        nError <- -1  # Fatal error if required parameters are missing
        lRetval$ErrorCode <- as.integer( nError )
        return( lRetval )
    }

    # Simulate drug concentration for each subject
    for( nPatIndx in 1:NumSub )
    {
        # Initial state: A1 = dDose (amount in absorption compartment), A2 = 0 (concentration in central compartment)
        vState <- c( A1 = dDose, A2 = 0 ) # this is a full dose in absorption compartment, none in central
        vParameters <- c( dAbsorptionRate =  dAbsorptionRate, dEliminationRate =  dEliminationRate )

        # Solve ODE for each visit time
        vConcentration <- numeric( NumVisit ) # Prepare a vector (NumVisit length) to store concentrations at each visit

        for( nVisitIndx in 1:NumVisit )
        {
            vTime   <- c( 0, VisitTime[ nVisitIndx ] )  # Time points for ODE solver
            mResult <- deSolve::ode( y = vState, times = vTime, func = OneCompartmentModelPK, parms = vParameters )
            vState  <- mResult[ nrow( mResult ), -1 ]  # Update state for next visit

            vConcentration[ nVisitIndx ] <- vState[ "A2" ]  # Extract concentration at current visit
        }

        # Add noise based on treatment group
        if( TreatmentID[ nPatIndx ] == 0 )
        {
            vConcentration <- vConcentration + rnorm( NumVisit, mean = MeanControl, sd = StdDevControl )
        }
        else
        {
            vConcentration <- vConcentration + rnorm( NumVisit, mean = MeanTrt, sd = StdDevTrt )
        }

        # Store concentration for each visit
        for( nVisitIndx in 1:NumVisit )
        {
            strVisitName <- paste0( "Response", nVisitIndx )

            if( !is.null( lRetval[[ strVisitName ] ] ) )
            {
                lRetval[[ strVisitName ] ] <- c( lRetval[[ strVisitName ] ], vConcentration[ nVisitIndx ] )
            }
            else
            {
                lRetval[[ strVisitName ] ] <- vConcentration[ nVisitIndx ]
            }
        }
    }

    # Set error code and return results
    lRetval$ErrorCode <- as.integer( nError )
    return( lRetval )
}

# Helper ODE function for one-compartment model with first-order absorption ####
OneCompartmentModelPK <- function( ime, state, parameters )
{
    with( as.list( c( state, parameters ) ),
    {

        dA1 <- - dAbsorptionRate * A1  # Change in drug amount in absorption compartment
        dA2 <- ( dAbsorptionRate * A1 -  dEliminationRate * A2 )  # Change in drug concentration in central compartment

        return( list( c( dA1, dA2 ) ) )
    } )
}
