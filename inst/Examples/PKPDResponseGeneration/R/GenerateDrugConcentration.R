######################################################################################################################## .
#' @name GenerateDrugConcentration
#'
#' @title Generate Drug Concentration Response from a One-Compartment Model with First-order Absorption
#'
#' @description Use a one-compartment PK model with first-order absorption to simulate plasma concentrations for
#'   patients.
#'
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumVisit Integer number of visits.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param Inputmethod Integer input method: 0 = actual means and standard deviations at each visit; 1 = expected
#'   changes from baseline at each visit. Preserve this engine-supplied spelling.
#'
#' @param VisitTime Numeric vector of visit times of length NumVisit.
#'
#' @param MeanControl Numeric vector of control-arm mean responses of length NumVisit, ordered by visit.
#'
#' @param MeanTrt Numeric vector of experimental-arm mean responses of length NumVisit, ordered by visit.
#'
#' @param StdDevControl Numeric vector of control-arm response standard deviations of length NumVisit, ordered by
#'   visit.
#'
#' @param StdDevTrt Numeric vector of experimental-arm response standard deviations of length NumVisit, ordered by
#'   visit.
#'
#' @param CorrMat Numeric correlation matrix between visits, with NumVisit rows and NumVisit columns.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' \describe{
#'   \item{AbsorptionRate}{Required positive numeric first-order absorption rate constant.}
#'   \item{EliminationRate}{Required positive numeric first-order elimination rate constant.}
#'   \item{Dose}{Required positive numeric administered dose.}
#' }
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Response1, ..., ResponseNumVisit}{Numeric response vectors, one per visit, with one element per subject.
#'     Replace NumVisit by the actual number of visits. Required.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' Example-specific additional output elements:
#' \describe{
#'   \item{Response<NumVisit>}{A set of arrays of response for all subjects. Each array corresponds to each visit
#'     user has specified}
#' }
#'
#' @details Return each visit response as a separate named list element: Response1, Response2, ...,
#'   ResponseNumVisit. Optional ArrivalTime may be included in the function signature when calendar arrival times
#'   are needed; it has the same definition as at the enrollment integration point.
######################################################################################################################## .

GenerateDrugConcentration <- function( NumSub, NumVisit, ArrivalTime, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL ) {
    library( deSolve )

    # Initialize error code and return list
    nErrorCode <- 0
    lRetval <- list( )

    # Parameters for ODE model
    dAbsorptionRate <- UserParam$AbsorptionRate
    dEliminationRate <- UserParam$EliminationRate
    dDose <- UserParam$Dose

    # Fatal error if required parameters are missing
    if ( is.null( dAbsorptionRate ) || is.null( dEliminationRate ) || is.null( dDose ) ) {
        nErrorCode <- -1
        lRetval$ErrorCode <- as.integer( nErrorCode )
        return( lRetval )
    }

    # Simulate drug concentration for each subject
    for ( nPatIndx in 1:NumSub ) {
        # Initial state: A1 = dDose (amount in absorption compartment), A2 = 0 (concentration in central compartment)
        vState <- c( A1 = dDose, A2 = 0 ) # this is a full dose in absorption compartment, none in central
        vParameters <- c( dAbsorptionRate = dAbsorptionRate, dEliminationRate = dEliminationRate )

        # Solve ODE for each visit time
        dPreviousVisitTime <- 0
        vConcentration <- numeric( NumVisit ) # prepare a vector (NumVisit length) to store concentrations at each visit

        for ( nVisitIndx in 1:NumVisit ) {
            vTime <- c( dPreviousVisitTime, VisitTime[ nVisitIndx ] ) # Integrate from the preceding visit.
            mResult <- deSolve::ode( y = vState, times = vTime, func = OneCompartmentModelPK, parms = vParameters )
            vState <- mResult[ nrow( mResult ), -1 ] # Update state for next visit
            dPreviousVisitTime <- VisitTime[ nVisitIndx ]

            vConcentration[ nVisitIndx ] <- vState[ "A2" ] # Extract concentration at current visit
        }

        # Add noise based on treatment group
        if ( TreatmentID[ nPatIndx ] == 0 ) {
            vConcentration <- vConcentration + stats::rnorm( NumVisit, mean = MeanControl, sd = StdDevControl )
        } else {
            vConcentration <- vConcentration + stats::rnorm( NumVisit, mean = MeanTrt, sd = StdDevTrt )
        }

        # Store concentration for each visit
        for ( nVisitIndx in 1:NumVisit ) {
            strVisitName <- paste0( "Response", nVisitIndx )

            if ( !is.null( lRetval[[ strVisitName ]] ) ) {
                lRetval[[ strVisitName ]] <- c( lRetval[[ strVisitName ]], vConcentration[ nVisitIndx ] )
            } else {
                lRetval[[ strVisitName ]] <- vConcentration[ nVisitIndx ]
            }
        }
    }

    # Set error code and return results
    lRetval$ErrorCode <- as.integer( nErrorCode )
    return( lRetval )
}

# Define helper ODE function for one-compartment model with first-order absorption
OneCompartmentModelPK <- function( time, state, parameters ) {
    with( as.list( c( state, parameters ) ), {
        dA1 <- -dAbsorptionRate * A1 # Change in drug amount in absorption compartment
        dA2 <- ( dAbsorptionRate * A1 - dEliminationRate * A2 ) # Change in drug concentration in central compartment

        return( list( c( dA1, dA2 ) ) )
    } )
}
