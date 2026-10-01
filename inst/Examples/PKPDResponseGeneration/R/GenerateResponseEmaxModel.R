######################################################################################################################## .
#' @name GenerateResponseEmaxModel
#' @title Simulate Treatment Effect with Emax Model
#' @description Generate drug concentrations per subject per visit, then apply the Emax equation to convert
#'   per-visit plasma concentrations into treatment responses using the Emax PD model.
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#' @param NumSub Integer number of subjects in the trial.
#' @param NumVisit Integer number of visits.
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#' @param Inputmethod Integer input method: 0 = actual means and standard deviations at each visit; 1 = expected
#'   changes from baseline at each visit. Preserve this engine-supplied spelling.
#' @param VisitTime Numeric vector of visit times measured from enrollment, of length NumVisit and ordered by
#'   visit.
#' @param MeanControl Numeric vector of control-arm mean responses of length NumVisit, ordered by visit.
#' @param MeanTrt Numeric vector of experimental-arm mean responses of length NumVisit, ordered by visit.
#' @param StdDevControl Numeric vector of control-arm response standard deviations of length NumVisit, ordered by
#'   visit.
#' @param StdDevTrt Numeric vector of experimental-arm response standard deviations of length NumVisit, ordered by
#'   visit.
#' @param CorrMat Numeric correlation matrix between visits, with NumVisit rows and NumVisit columns.
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
#'   \item{E0}{Required numeric baseline pharmacodynamic effect.}
#'   \item{Emax}{Required numeric maximum pharmacodynamic effect.}
#'   \item{EC50}{Required positive numeric concentration producing half of Emax.}
#' }
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response1, ..., ResponseNumVisit}{Numeric response vectors, one per visit, with one element per subject.
#'     Replace NumVisit by the actual number of visits.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details Return each visit response as a separate named list element: Response1, Response2, ...,
#'   ResponseNumVisit.
#'
#' This example uses independent normal measurement noise at each visit.
######################################################################################################################## .

GenerateResponseEmaxModel <- function( NumSub, NumVisit, ArrivalTime, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL ) {
    nErrorCode <- 0
    lRetval <- list( )

    # Initialize simulated response matrix
    mResponses <- matrix( 0, nrow = NumSub, ncol = NumVisit )

    # Define the Emax model parameters from UserParam
    dAbsorptionRate <- UserParam$AbsorptionRate # Absorption rate constant
    dEliminationRate <- UserParam$EliminationRate # Elimination rate constant
    dDose <- UserParam$Dose # Dose administered
    dE0 <- UserParam$E0 # Baseline effect
    dEmax <- UserParam$Emax # Maximum effect
    dEC50 <- UserParam$EC50 # Concentration at 50% of Emax

    # Check if all required Emax parameters are provided
    if ( is.null( dE0 ) || is.null( dEmax ) || is.null( dEC50 ) || is.null( dAbsorptionRate ) || is.null( dEliminationRate ) || is.null( dDose ) ) {
        nErrorCode <- -1 # Fatal error if required parameters are missing
        lRetval$ErrorCode <- as.integer( nErrorCode )
        return( lRetval )
    }

    # Call PK function to get concentration responses for treatment group
    lPkResult <- GenerateEmaxDrugConcentration( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, dAbsorptionRate, dEliminationRate, dDose )

    # Simulate response for each patient
    for ( nPatIndx in 1:NumSub ) {
        for ( nVisitIndx in 1:NumVisit ) {
            dConcentration <- lPkResult[[ paste0( "Response", nVisitIndx ) ]][ nPatIndx ]

            dTreatmentEffect <- dE0 + ( dEmax * dConcentration ) / ( dEC50 + dConcentration ) # Calculate Emax

            if ( TreatmentID[ nPatIndx ] == 0 ) {
                mResponses[ nPatIndx, nVisitIndx ] <- stats::rnorm( 1, mean = MeanControl[ nVisitIndx ], sd = StdDevControl[ nVisitIndx ] ) # Generates response for control group
            } else {
                mResponses[ nPatIndx, nVisitIndx ] <- stats::rnorm( 1, mean = dTreatmentEffect, sd = StdDevTrt[ nVisitIndx ] ) # Generates response for treatment group (Emax model output)
            }
        }
    }

    # Add responses to return list
    for ( nVisitIndx in 1:NumVisit ) {
        lRetval[[ paste0( "Response", nVisitIndx ) ]] <- as.double( mResponses[ , nVisitIndx ] )
    }

    lRetval$ErrorCode <- as.integer( nErrorCode )
    return( lRetval )
}


######################################################################################################################## .
#' @name GenerateEmaxDrugConcentration
#' @title Generate Drug Concentration
#' @description Generate visit-level drug concentrations using a one-compartment model with first-order absorption
#'   and elimination, then add arm-specific normal measurement noise.
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#' @param NumSub Integer number of subjects in the trial.
#' @param NumVisit Integer number of visits.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#' @param Inputmethod Integer input method: 0 = actual means and standard deviations at each visit; 1 = expected
#'   changes from baseline at each visit. Preserve this engine-supplied spelling.
#' @param VisitTime Numeric vector of visit times measured from enrollment, of length NumVisit and ordered by
#'   visit.
#' @param MeanControl Numeric vector of control-arm mean responses of length NumVisit, ordered by visit.
#' @param MeanTrt Numeric vector of experimental-arm mean responses of length NumVisit, ordered by visit.
#' @param StdDevControl Numeric vector of control-arm response standard deviations of length NumVisit, ordered by
#'   visit.
#' @param StdDevTrt Numeric vector of experimental-arm response standard deviations of length NumVisit, ordered by
#'   visit.
#' @param CorrMat Numeric correlation matrix between visits, with NumVisit rows and NumVisit columns.
#' @param dAbsorptionRate Positive numeric first-order absorption rate constant.
#' @param dEliminationRate Positive numeric first-order elimination rate constant.
#' @param dDose Positive numeric administered dose.
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response1, ..., ResponseNumVisit}{Numeric response vectors, one per visit, with one element per subject.
#'     Replace NumVisit by the actual number of visits.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

GenerateEmaxDrugConcentration <- function( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, dAbsorptionRate, dEliminationRate, dDose ) {
    # Initialize error code and return list
    nErrorCode <- 0
    lRetval <- list( )

    if ( is.null( dAbsorptionRate ) || is.null( dEliminationRate ) || is.null( dDose ) ) {
        nErrorCode <- -1 # Fatal error if required parameters are missing
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


######################################################################################################################## .
#' @name OneCompartmentModelPK
#' @title One Compartment Model PK
#' @description Compute derivatives for the absorption and central compartments of the one-compartment
#'   pharmacokinetic model.
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#' @param time Time variable for ODE solver
#' @param state State variables (A1: amount in absorption compartment, A2: concentration in central compartment)
#' @param parameters Parameters for the ODE (dAbsorptionRate, dEliminationRate)
#' @return List containing a numeric vector of derivatives in state order: dA1 for absorption and dA2 for the
#'   central compartment.
######################################################################################################################## .

OneCompartmentModelPK <- function( time, state, parameters ) {
    with( as.list( c( state, parameters ) ), {
        dA1 <- -dAbsorptionRate * A1 # Change in drug amount in absorption compartment
        dA2 <- ( dAbsorptionRate * A1 - dEliminationRate * A2 ) # Change in drug concentration in central compartment

        return( list( c( dA1, dA2 ) ) )
    } )
}
