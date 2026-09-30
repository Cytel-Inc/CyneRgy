######################################################################################################################## .
#' @name SimulatePatientOutcomeNormalAssurance
#'
#' @title Simulate normal responses for Bayesian assurance
#'
#' @description Generate normally distributed subject responses with uncertainty in the true experimental-arm mean.
#'   The experimental mean is sampled from the assurance prior once per simulation; subject outcomes are then
#'   generated using that mean and the supplied standard deviations.
#'
#' @author J. Kyle Wathen and Laurent Spiess
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param Mean Numeric vector of mean responses by arm, with the control arm first, followed by experimental arms
#'   in TreatmentID order.
#'
#' @param StdDev Numeric vector of response standard deviations by arm, with the control arm first, followed by
#'   experimental arms in TreatmentID order.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' If UserParam is supplied, the list must contain the following named elements:
#' \describe{
#'      \item{UserParam$dWeight1}{Probability of sampiling from part 1}
#'      \item{UserParam$dWeight}{Probability of sampling from part 2}
#'      \item{UserParam$dMean1}{Prior mean for part 1}
#'      \item{UserParam$dMean2}{Prior mean for part 2}
#'      \item{UserParam$dSD1}{Prior SD for part 1}
#'      \item{UserParam$dSD2}{Prior SD for part 2}
#'      \item{UserParam$dWeight1}{Weight of prior 1}
#'      \item{UserParam$dWeight2}{Weight of prior 2}
#'      \item{UserParam$dMeanCtrl}{Mean form control }
#'  }
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject. Required.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

SimulatePatientOutcomeNormalAssurance <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL ) {
    # Step 1 - Setup the vectors so we can sample which component of the mixture prior to use
    vStdDev <- c( UserParam$dSDCtrl, UserParam$dSDExp )
    vMean <- c( UserParam$dMeanCtrl ) # Note: only need control mean as we will sample experimental mean
    vPriorMeans <- c( UserParam$dMean1, UserParam$dMean2 )
    vPriorSDs <- c( UserParam$dSD1, UserParam$dSD2 )

    # Step 2 - Sample the prior mean treatment effect according to the weights of the two normal distributions in the mixture
    nPrior <- sample( c( 1, 2 ), 1, prob = c( UserParam$dWeight1, UserParam$dWeight2 ), replace = TRUE )
    dTreatmentEffect <- stats::rnorm( 1, vPriorMeans[ nPrior ], vPriorSDs[ nPrior ] )
    vMean <- c( vMean, vMean[ 1 ] + dTreatmentEffect )

    # Step 3 - Initialize variable ####
    nErrorCode <- 0 # East Horizon code for no errors occurred
    vPatientOutcome <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    # Step 4 - Loop over the patients and simulate the outcome according to the treatment they received ####
    for ( nPatIndx in 1:NumSub ) {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index

        # Make any adjustments to the code as needed, example simulating from for a normal distribution
        vPatientOutcome[ nPatIndx ] <- stats::rnorm( 1, vMean[ nTreatmentID ], vStdDev[ nTreatmentID ] )
    }

    # Step 5 - Error Checking ####
    if ( any( is.na( vPatientOutcome ) == TRUE ) ) {
        nErrorCode <- -100
    }

    # Step 6 - Create any variables that are returned that need to be included in the output
    # Note: Need to return the true delta, and East Horizon expects it to be a vector.
    TrueDelta <- rep( vMean[ 2 ], length( vPatientOutcome ) )

    # Step 7 - Build the return object, add other variables to the list as needed
    #       Add the vTrueDeta so it can easily be output by saving the East Horizon summary stats.
    lReturn <- list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nErrorCode ), vTrueDelta = as.double( TrueDelta ), Delta = as.double( TrueDelta ) )

    return( lReturn )
}


######################################################################################################################## .
#' @name SimulatePatientOutcomeNormalAssuranceUsingPriorInput
#'
#' @title Simulate normal responses from a supplied assurance prior
#'
#' @description Generate normal responses using successive experimental-arm means from the global prior vector
#'   vPrior. Advance the global index nSimIndex once per call. Initialize both globals before simulation; return
#'   ErrorCode = -100 when they are missing or the prior vector is exhausted.
#'
#' @author J. Kyle Wathen and Laurent Spiess
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param Mean Numeric vector of mean responses by arm, with the control arm first, followed by experimental arms
#'   in TreatmentID order.
#'
#' @param StdDev Numeric vector of response standard deviations by arm, with the control arm first, followed by
#'   experimental arms in TreatmentID order.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The
#'   default is NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position.
#'   User-defined scalar parameters may be integer, numeric, or character values.
#' \describe{
#'   \item{dSDCtrl}{Required numeric control-arm response standard deviation.}
#'   \item{dSDExp}{Required numeric experimental-arm response standard deviation.}
#' }
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject. Required.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
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
    nSimIndex <<- nSimIndex + 1 # remove the first element so the next call gets a different true delta

    # Step 2 - Setup the vectors so we can sample which component of the mixture prior to use ####
    vStdDev <- c( UserParam$dSDCtrl, UserParam$dSDExp )


    # Step 3 - Initialize variable ####
    nErrorCode <- 0 # East Horizon code for no errors occurred
    vPatientOutcome <- rep( 0, NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated

    # Step 4 - Loop over the patients and simulate the outcome according to the treatment they received ####
    for ( nPatIndx in 1:NumSub ) {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East Horizon has the treatments as 0, 1 so need to add 1 to get a vector index

        # Make any adjustments to the code as needed, example simulating from for a normal distribution
        vPatientOutcome[ nPatIndx ] <- stats::rnorm( 1, Mean[ nTreatmentID ], vStdDev[ nTreatmentID ] )
    }

    # Step 5 - Error Checking ####
    if ( any( is.na( vPatientOutcome ) == TRUE ) ) {
        nErrorCode <- -100
    }

    # Step 6 - Create any variables that are returned that need to be included in the output
    TrueDelta <- rep( Mean[ 2 ], length( vPatientOutcome ) )

    # Step 7 - Build the return object, add other variables to the list as needed
    lReturn <- list( Response = as.double( vPatientOutcome ), ErrorCode = as.integer( nErrorCode ), vTrueDelta = as.double( TrueDelta ), Delta = as.double( TrueDelta ) )

    return( lReturn )
}
