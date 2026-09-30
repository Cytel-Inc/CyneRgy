######################################################################################################################## .
#' @name SimulatePatientSurvivalAssurance
#'
#' @title Simulate Time-To-Event Data for Assurance
#'
#' @description The analysis is assumed to be a cox proportional hazard model where a Go decision is made if the
#'   p-value <= 0.025.
#' For assurance, a bi-modal prior on the Log(HR) is used.   The components of the prior are:
#' Weight: 25\% on $N( 0, 0.02 )$
#' Weight: 75\% on $Beta( 2, 2)$, rescaled between -0.4 and 0.
#'
#' @author J. Kyle Wathen and Laurent Spiess
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
#' @param SurvMethod Integer survival input method: 1 = hazard rates; 2 = cumulative survival percentages; 3 =
#'   median survival times.
#'
#' @param NumPrd Integer number of survival periods. Equals 1 for multi-arm confirmatory designs and stratified
#'   survival generation.
#'
#' @param PrdTime Times used to specify survival parameters: starting times of hazard pieces for SurvMethod = 1;
#'   times at which cumulative survival percentages are specified for SurvMethod = 2; 0 for SurvMethod = 3. Legacy
#'   East Horizon inputs may be vectors; East Horizon inputs may be period-by-arm arrays (stratum-by-arm arrays with
#'   stratification).
#'
#' @param SurvParam Array of survival parameters with NumPrd rows and NumArm columns, or one row per stratum when
#'   stratification is enabled. Column 1 is control; subsequent columns are experimental arms. Values are hazard
#'   rates for SurvMethod = 1, cumulative survival percentages for SurvMethod = 2, and median survival times for
#'   SurvMethod = 3. Without stratification, the median-survival method has one row.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' UserParam must be supplied and the list must contain the following named elements:
#' \describe{
#'      \item{UserParam$dWeight1}{Probability of sampling from part 1}
#'      \item{UserParam$dWeight2}{Probability of sampling from part 2}
#'      \item{UserParam$dPriorMean}{Prior mean for normal distibution}
#'      \item{UserParam$dPriorSD}{Prior standard deviation for the normal distribution}
#'      \item{UserParam$dAlpha}{The alpha parameter in the Beta( alpha, beta ) piece of the prior distribution}
#'      \item{UserParam$dBeta}{The beta parameter in the Beta( alpha, beta ) piece of the prior distribution}
#'      \item{UserParam$dUpper}{Upper limit for scaling the Beta distribution.}
#'      \item{UserParam$dLower}{Lower limit for scaling the Beta distribution. }
#'      \item{UserParam$dMeanTTECtrl}{The mean time-to-event for the control treatment. }
#'  }
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject. Required.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

SimulatePatientSurvivalAssurance <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) {
    # Step 1 - Determine how many patients on each treatment need to be simulated ####
    vTrtAllocation <- table( TreatmentID )
    vSurvTime <- rep( -1, NumSub ) # The vector of patient survival times that will be returned.

    ErrorCode <- 0

    # Step 2: First sample the piece of the prior we want to use ####

    nPriorPart <- stats::rbinom( 1, 1, UserParam$dWeight1 )

    if ( nPriorPart == 1 ) {
        # Sample the normal part
        dLogTrueHazard <- stats::rnorm( 1, UserParam$dPriorMean, UserParam$dPriorSD )
    } else {
        # Comes from Beta( UserParam$dAlpha, UserParam$dBeta) scaled to ( UserParam$dLower, UserParam$dUpper )
        dLogTrueHazard <- stats::rbeta( 1, UserParam$dAlpha, UserParam$dBeta )

        dWidth <- UserParam$dUpper - UserParam$dLower
        dLogTrueHazard <- dWidth * ( dLogTrueHazard ) + UserParam$dLower
    }

    # Step 3: Compute the hazard on experimental given the true hazard on control and the sample dLogHazard ####
    dTrueHazard <- exp( dLogTrueHazard )
    dRateCtrl <- 1.0 / UserParam$dMeanTTECtrl
    dRateExp <- dTrueHazard * dRateCtrl

    vRates <- c( dRateCtrl, dRateExp )

    for ( i in 1:NumSub ) {
        vSurvTime[ i ] <- stats::rexp( 1, vRates[ TreatmentID[ i ] + 1 ] )
    }

    # vTrt1 <- rexp( vTrtAllocation[ 1 ], vRates[ 1 ] )
    # vTrt2 <- rexp( vTrtAllocation[ 2 ], vRates[ 2 ] )

    # vSurvTime[ TreatmentID == 0 ] <- vTrt1
    # vSurvTime[ TreatmentID == 1 ] <- vTrt2

    lRet <- list(
        SurvivalTime = as.double( vSurvTime ),
        ErrorCode = as.integer( ErrorCode ),
        TrueHR = as.double( rep( dTrueHazard, NumSub ) )
    )

    return( lRet )
}
