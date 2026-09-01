######################################################################################################################## .
#' @name GenerateDrugConcentration
#' @title Generate Drug Concentration Response from a One-Compartment Model with First-order Absorption
#' @description
#' Use a one-compartment PK model with first-order absorption to simulate plasma concentrations for patients.
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#' @param NumSub Integer number of subjects in the trial.
#' @param NumVisit Integer number of visits.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject. Required for integration but not used by this example.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param Inputmethod Integer input-method code: 0 for actual means and standard deviations; 1 for change from baseline. Not used by this example.
#' @param VisitTime Numeric vector of length `NumVisit`, indicating the visit times.
#' @param MeanControl Numeric vector of length `NumVisit`, containing control-arm means by visit.
#' @param MeanTrt Numeric vector of length `NumVisit`, containing treatment-arm means by visit.
#' @param StdDevControl Numeric vector of length `NumVisit`, containing control-arm standard deviations by visit.
#' @param StdDevTrt Numeric vector of length `NumVisit`, containing treatment-arm standard deviations by visit.
#' @param CorrMat Numeric `NumVisit` by `NumVisit` correlation matrix between visits.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' \describe{
#'   \item{UserParam$AbsorptionRate}{First-order absorption rate constant.}
#'   \item{UserParam$EliminationRate}{First-order elimination rate constant.}
#'   \item{UserParam$Dose}{Administered dose.}
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

GenerateDrugConcentration <- function( NumSub, NumVisit, ArrivalTime, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL )
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

# Define helper ODE function for one-compartment model with first-order absorption
OneCompartmentModelPK <- function( ime, state, parameters )
{
    with( as.list( c( state, parameters ) ),
    {

        dA1 <- - dAbsorptionRate * A1  # Change in drug amount in absorption compartment
        dA2 <- ( dAbsorptionRate * A1 -  dEliminationRate * A2 )  # Change in drug concentration in central compartment

        return( list( c( dA1, dA2 ) ) )
    } )
}
