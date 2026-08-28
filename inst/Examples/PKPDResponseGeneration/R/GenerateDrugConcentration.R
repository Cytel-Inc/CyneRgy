######################################################################################################################## .
#' @name GenerateDrugConcentration
#' @title Generate Drug Concentration Response from a One-Compartment Model with First-order Absorption
#' @description
#' Use a one-compartment PK model with first-order absorption to simulate plasma concentrations for patients.
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#' @param NumSub Integer. Number of subjects that need to be simulated, integer value. The argument value is passed from Engine.
#' @param NumVisit Integer. Number of Visits
#' @param TreatmentID Array specifying indexes of arms to which subjects are allocated ﴾one arm index per subject. Index for placebo / control is 0.
#' @param Inputmethod There were two options: 0 - the mean and SD values represent actual values.
#'                                            1 - values represent an expected change from baseline at each visit rather than the true means.
#' @param VisitTime Numeric. Visit Times
#' @param MeanControl Numeric. Control Mean for all visits
#' @param MeanTrt Numeric. Treatment Mean for all visits
#' @param StdDevControl Numeric. Control Standard Deviations for all visits
#' @param StdDevTrt Numeric. Treatment Standard Deviations for all visits
#' @param CorrMat Correlation Matrix between all visits. Matrix of dimension n*n containing numeric values where n is number of visits.
#' @param UserParam List. User can pass custom scalar variables defined by users as a member of this list. User should access the variables using names, for example UserParam$Var1 and not order.
#' \describe{
#'   \item{AbsorptionRate}{Absorption rate constant}
#'   \item{EliminationRate}{Elimination rate constant}
#'   \item{Dose}{Dose administered}
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

GenerateDrugConcentration <- function( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL )
{
    # Initialize error code and return list
    nError  <- 0
    lRetval <- list()

    # Parameters for ODE model
    dAbsorptionRate   <- UserParam$AbsorptionRate
    dEliminationRate  <- UserParam$EliminationRate
    dDose             <- UserParam$Dose

    # Fatal error if required parameters are missing
    if( is.null( dAbsorptionRate ) || is.null( dEliminationRate ) || is.null( dDose ) )
    {
        nError <- -1
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
        vConcentration <- numeric( NumVisit ) #prepare a vector (NumVisit length) to store concentrations at each visit

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

# Define helper ODE function for one-compartment model with first-order absorption
OneCompartmentModelPK <- function( ime, state, parameters )
{
    with( as.list( c( state, parameters ) ), {

        dA1 <- - dAbsorptionRate * A1  # Change in drug amount in absorption compartment
        dA2 <- ( dAbsorptionRate * A1 -  dEliminationRate * A2 )  # Change in drug concentration in central compartment

        return( list( c( dA1, dA2 ) ) )
    } )
}
