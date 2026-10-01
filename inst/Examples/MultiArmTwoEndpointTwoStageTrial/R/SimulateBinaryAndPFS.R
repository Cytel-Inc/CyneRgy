######################################################################################################################## .
#' @name SimulateBinaryAndPFS
#'
#' @title Simulate Binary Response and Progression-Free Survival (PFS)
#'
#' @description This function simulates subject-level binary response outcomes and progression-free survival (PFS)
#'   times for a multi-arm clinical trial.
#'
#' @author Julija Saltane, J. Kyle Wathen
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArm Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#'
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#'
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#'
#' @param PropResp Numeric vector of response probabilities by arm, with the control arm first, followed by
#'   experimental arms in TreatmentID order. Each probability is between 0 and 1.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' \describe{
#'          \item{MedianSurvCtrl}{Median survival time for the control arm}
#'          \item{HR1, HR2, ..., HR(n)}{Hazard ratios for each treatment arm relative to control}
#'        }
#'
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated binary subject responses, coded 0 = non-response and 1 = response,
#'     with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' Example-specific additional output elements:
#' \describe{
#'   \item{PFSNonCens}{Numeric vector of PFS times relative to patient enrollment}
#' }
#'
#' @details Example-specific error codes:
#' \describe{
#'   \item{ErrorCode = -1}{Hazard ratio parameters (HR1...HRn) are missing or not consecutive}
#'   \item{ErrorCode = -2}{NA or invalid values encountered in simulation output}
#' }
######################################################################################################################## .

SimulateBinaryAndPFS <- function( NumSub, NumArm, ArrivalTime, TreatmentID, PropResp, UserParam = NULL ) {
    # Step 1. Initialize error code and output vectors
    nErrorCode <- 0
    vBinaryOutcome <- rep( 0, NumSub )
    vPFSNonCens <- rep( NA, NumSub )

    # Step 2. Ensure all parameters are present for simulation of the PFS data
    if ( is.null( UserParam ) ) {
        UserParam <- list( MedianSurvCtrl = 12 )
        for ( i in 1:( NumArm - 1 ) ) {
            UserParam[[ paste0( "HR", i ) ]] <- 0.7
        }
    } else {
        if ( is.null( UserParam$MedianSurvCtrl ) ) {
            UserParam$MedianSurvCtrl <- 12
        }
        for ( i in 1:( NumArm - 1 ) ) {
            HRName <- paste0( "HR", i )
            if ( is.null( UserParam[[ HRName ]] ) ) {
                UserParam[[ HRName ]] <- 0.7
            }
        }
    }
    # Check that HR1, HR2, ..., HR(n) exist and are consecutive
    HRNames <- names( UserParam )[ grepl( "^HR", names( UserParam ) ) ]
    HRNumbers <- sort( as.integer( sub( "^HR", "", HRNames ) ) )

    if ( !all( HRNumbers == seq_len( NumArm - 1 ) ) ) {
        nErrorCode <- -1
        return( list(
            Response = as.double( vBinaryOutcome ),
            PFSNonCens = as.double( vPFSNonCens ),
            ErrorCode = as.integer( nErrorCode )
        ) )
    }

    # Step 3. Convert median survival -> exponential rate
    dRateCtrl <- log( 2 ) / UserParam$MedianSurvCtrl
    vRates <- numeric( NumArm )
    vRates[ 1 ] <- dRateCtrl

    for ( i in 2:NumArm ) {
        HRName <- paste0( "HR", i - 1 )
        vRates[ i ] <- dRateCtrl * UserParam[[ HRName ]]
    }

    # Step 4. Simulate binary and PFS outcomes for each subject
    for ( nPatIndx in 1:NumSub ) {
        nTreatmentID <- TreatmentID[ nPatIndx ] + 1 # 1-based index for R, while it's 0-based index in assignments

        # Simulate binary response
        vBinaryOutcome[ nPatIndx ] <- stats::rbinom( 1, 1, PropResp[ nTreatmentID ] )

        # Simulate time-to-event (exponential distribution)
        dRate <- vRates[ nTreatmentID ]
        if ( dRate > 0 ) {
            dEventTime <- stats::rexp( 1, rate = dRate )
        } else {
            dEventTime <- Inf
        }
        vPFSNonCens[ nPatIndx ] <- dEventTime
    }

    # Check for NA or invalid values
    if ( any( is.na( vBinaryOutcome ) ) || any( is.na( vPFSNonCens ) ) ) {
        nErrorCode <- -2
    }

    return( list(
        Response = as.double( vBinaryOutcome ),
        PFSNonCens = as.double( vPFSNonCens ),
        ErrorCode = as.integer( nErrorCode )
    ) )
}
