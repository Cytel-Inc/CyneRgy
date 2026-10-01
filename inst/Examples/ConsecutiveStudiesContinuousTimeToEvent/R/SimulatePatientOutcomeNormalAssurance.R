######################################################################################################################## .
#' @name SimulatePatientOutcomeNormalAssurance
#' @title Simulate normal responses for Bayesian assurance
#' @description Generate normal subject responses using a two-component normal mixture prior for the
#'   experimental-minus-control treatment effect. Sample the treatment effect once per simulation, add it to
#'   the control mean, then generate subject responses using the arm-specific standard deviations.
#' @author J. Kyle Wathen, Laurent Spiess, Gabriel Potvin
#' @param NumSub Integer number of subjects in the trial.
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#' @param Mean Numeric vector of mean responses by arm, with the control arm first, followed by experimental arms
#'   in TreatmentID order.
#' @param StdDev Numeric vector of response standard deviations by arm, with the control arm first, followed by
#'   experimental arms in TreatmentID order.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' This generator requires the following named elements:
#' \describe{
#'   \item{UserParam$dWeight1}{Nonnegative numeric sampling weight for the first component of the mixture
#'     prior.}
#'   \item{UserParam$dWeight2}{Nonnegative numeric sampling weight for the second component of the mixture
#'     prior. At least one component weight must be positive; sample() normalizes the weights.}
#'   \item{UserParam$dMean1}{Numeric mean of the treatment-effect prior for component 1.}
#'   \item{UserParam$dMean2}{Numeric mean of the treatment-effect prior for component 2.}
#'   \item{UserParam$dSD1}{Nonnegative numeric standard deviation of the treatment-effect prior for component
#'     1.}
#'   \item{UserParam$dSD2}{Nonnegative numeric standard deviation of the treatment-effect prior for component
#'     2.}
#'   \item{UserParam$dMeanCtrl}{Numeric mean response on the control arm.}
#'   \item{UserParam$dSDCtrl}{Nonnegative numeric control-arm response standard deviation.}
#'   \item{UserParam$dSDExp}{Nonnegative numeric experimental-arm response standard deviation.}
#' }
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#'   \item{vTrueDelta}{Custom numeric vector of true experimental-minus-control mean differences, with one
#'     element per subject and the same value throughout a simulation.}
#'   \item{Delta}{Custom numeric vector equal to vTrueDelta, retained for existing summary-statistic outputs.}
#'   \item{dSimMeanCtrl}{Numeric vector of true control-arm response means, with one element per subject and the
#'     same value throughout a simulation.}
#'   \item{dSimMeanExp}{Numeric vector of true experimental-arm response means, with one element per subject and
#'     the same value throughout a simulation.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details UserParam supplies the control mean, both response standard deviations, and the treatment-effect prior.
######################################################################################################################## .

SimulatePatientOutcomeNormalAssurance <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL ) {
    # Step 1 - Setup the vectors so we can sample which component of the mixture prior to use ####
    vStdDev <- c( UserParam$dSDCtrl, UserParam$dSDExp )
    vMean <- c( UserParam$dMeanCtrl ) # Note: only need control mean as we will sample experimental mean
    vPriorMeans <- c( UserParam$dMean1, UserParam$dMean2 )
    vPriorSDs <- c( UserParam$dSD1, UserParam$dSD2 )

    # Step 2 - Sample the prior mean treatment effect according to the weights of the two normal distributions in the mixture ####
    nPrior <- sample( c( 1, 2 ), 1, prob = c( UserParam$dWeight1, UserParam$dWeight2 ), replace = TRUE )
    dTreatmentEffect <- stats::rnorm( 1, vPriorMeans[ nPrior ], vPriorSDs[ nPrior ] )
    vMean <- c( vMean, vMean[ 1 ] + dTreatmentEffect )

    # Step 3 - Initialize variable ####
    nErrorCode <- 0 # East Horizon code for no errors occurred
    vPatientOutcome <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    # Step 4 - Loop over the patients and simulate the outcome according to the treatment they received ####
    for ( nPatIndx in 1:NumSub ) {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index

        # Make any adjustments to the code as needed, for example, simulating from a normal distribution
        vPatientOutcome[ nPatIndx ] <- stats::rnorm( 1, vMean[ nTreatmentID ], vStdDev[ nTreatmentID ] )
    }

    # Step 5 - Error Checking ####
    if ( any( is.na( vPatientOutcome ) ) ) {
        nErrorCode <- -100
    }

    # Step 6 - Create any variables that are returned that need to be included in the output ####
    # Note: Need to return the true delta, and East Horizon expects it to be a vector.
    vTrueDelta <- rep( vMean[ 2 ] - vMean[ 1 ], length( vPatientOutcome ) )

    # Step 7 - Build the return object, add other variables to the list as needed ####
    #       Add the vTrueDelta so it can easily be output by saving the East Horizon summary stats.
    lReturn <- list(
        Response = as.double( vPatientOutcome ),
        ErrorCode = as.integer( nErrorCode ),
        vTrueDelta = as.double( vTrueDelta ),
        Delta = as.double( vTrueDelta ),
        dSimMeanCtrl = rep( as.double( vMean[ 1 ] ), NumSub ),
        dSimMeanExp = rep( as.double( vMean[ 2 ] ), NumSub )
    )

    return( lReturn )
}


######################################################################################################################## .
#' @name SimulatePatientOutcomeNormalAssuranceUsingPriorInput
#' @title Simulate normal responses from a supplied assurance prior
#' @description Generate normal responses using successive experimental-arm means from the global prior vector
#'   vPrior. Advance the global index nSimIndex once per call. Initialize both globals before simulation; return
#'   ErrorCode = -100 when they are missing or the prior vector is exhausted.
#' @author J. Kyle Wathen, Laurent Spiess, Gabriel Potvin
#' @param NumSub Integer number of subjects in the trial.
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#' @param Mean Numeric vector of mean responses by arm, with the control arm first, followed by experimental arms
#'   in TreatmentID order.
#' @param StdDev Numeric vector of response standard deviations by arm, with the control arm first, followed by
#'   experimental arms in TreatmentID order.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' This generator requires the following named elements:
#' \describe{
#'   \item{UserParam$dSDCtrl}{Nonnegative numeric control-arm response standard deviation.}
#'   \item{UserParam$dSDExp}{Nonnegative numeric experimental-arm response standard deviation.}
#' }
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#'   \item{vTrueDelta}{Custom numeric vector of true experimental-minus-control mean differences, with one
#'     element per subject and the same value throughout a simulation.}
#'   \item{Delta}{Custom numeric vector equal to vTrueDelta, retained for existing summary-statistic outputs.}
#'   \item{dSimMeanCtrl}{Numeric vector of true control-arm response means, with one element per subject and the
#'     same value throughout a simulation.}
#'   \item{dSimMeanExp}{Numeric vector of true experimental-arm response means, with one element per subject and
#'     the same value throughout a simulation.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details Mean supplies the control response mean. The experimental mean is replaced by the next vPrior
#'   value. UserParam supplies the response standard deviations for both arms.
######################################################################################################################## .

SimulatePatientOutcomeNormalAssuranceUsingPriorInput <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL ) {
    # Step 1 - Use the prior that was supplied from output ####
    if ( !exists( "vPrior" ) || !exists( "nSimIndex" ) ) {
        return( list( Response = rep( 0, NumSub ), ErrorCode = -100L ) )
    }
    if ( nSimIndex < 1 || nSimIndex > length( vPrior ) ) {
        return( list( Response = rep( 0, NumSub ), ErrorCode = -100L ) )
    }

    Mean[ 2 ] <- vPrior[ nSimIndex ]
    nSimIndex <<- nSimIndex + 1 # Advance to the next prior mean for the next simulation.

    # Step 2 - Setup the vectors so we can sample which component of the mixture prior to use ####
    vStdDev <- c( UserParam$dSDCtrl, UserParam$dSDExp )


    # Step 3 - Initialize variable ####
    nErrorCode <- 0 # East Horizon code for no errors occurred
    vPatientOutcome <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    # Step 4 - Loop over the patients and simulate the outcome according to the treatment they received ####
    for ( nPatIndx in 1:NumSub ) {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index

        # Make any adjustments to the code as needed, for example, simulating from a normal distribution
        vPatientOutcome[ nPatIndx ] <- stats::rnorm( 1, Mean[ nTreatmentID ], vStdDev[ nTreatmentID ] )
    }

    # Step 5 - Error Checking ####
    if ( any( is.na( vPatientOutcome ) == TRUE ) ) {
        nErrorCode <- -100
    }

    # Step 6 - Create any variables that are returned that need to be included in the output ####
    vTrueDelta <- rep( Mean[ 2 ] - Mean[ 1 ], length( vPatientOutcome ) )

    # Step 7 - Build the return object, add other variables to the list as needed ####
    lReturn <- list(
        Response = as.double( vPatientOutcome ),
        ErrorCode = as.integer( nErrorCode ),
        vTrueDelta = as.double( vTrueDelta ),
        Delta = as.double( vTrueDelta ),
        dSimMeanCtrl = rep( as.double( Mean[ 1 ] ), NumSub ),
        dSimMeanExp = rep( as.double( Mean[ 2 ] ), NumSub )
    )

    return( lReturn )
}
