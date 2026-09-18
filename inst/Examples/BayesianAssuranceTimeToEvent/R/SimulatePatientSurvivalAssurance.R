######################################################################################################################## .
#' @name SimulatePatientSurvivalAssurance
#' @title Simulate Time-To-Event Data for Assurance
#' @author J. Kyle Wathen and Laurent Spiess
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param SurvMethod Integer survival-generation method: 1 for hazard rates, 2 for cumulative survival probabilities, or 3 for median survival times.
#' @param NumPrd Integer number of survival periods.
#' @param PrdTime Numeric matrix with `NumPrd` rows and `NumArm` columns, indicating the times used to specify survival parameters. For `SurvMethod = 1`, entries are hazard-piece start times; for `SurvMethod = 2`, entries are times at which cumulative survival is specified; for `SurvMethod = 3`, entries default to 0.
#' @param SurvParam Numeric matrix with `NumPrd` rows and `NumArm` columns containing arm-specific survival parameters.
#'   \describe{
#'     \item{SurvMethod = 1}{Hazard rates for each period and arm. Entry `[i, j]` is the hazard rate in period `i` for arm `j`.}
#'     \item{SurvMethod = 2}{Cumulative survival probabilities for each period and arm. Entry `[i, j]` is the cumulative survival probability in period `i` for arm `j`.}
#'     \item{SurvMethod = 3}{One row of median survival times, with one value per arm.}
#'   }
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#' In this example, UserParam must contain the following named elements:
#' \describe{
#'      \item{UserParam$dWeight1}{Probability of sampling from part 1}
#'      \item{UserParam$dPriorMean}{Prior mean for the normal distribution}
#'      \item{UserParam$dPriorSD}{Prior standard deviation for the normal distribution}
#'      \item{UserParam$dAlpha}{The alpha parameter in the Beta( alpha, beta ) piece of the prior distribution}
#'      \item{UserParam$dBeta}{The beta parameter in the Beta( alpha, beta ) piece of the prior distribution}
#'      \item{UserParam$dUpper}{Upper limit for scaling the Beta distribution.}
#'      \item{UserParam$dLower}{Lower limit for scaling the Beta distribution. }
#'      \item{UserParam$dMeanTTECtrl}{The mean time-to-event for the control treatment. }
#'  }
#' @description
#' The analysis is assumed to be a cox proportional hazard model where a Go decision is made if the p-value $\leq$ 0.025.
#' For assurance, a bi-modal prior on the Log(HR) is used.   The components of the prior are:
#' Weight: 25\% on $N( 0, 0.02 )$
#' Weight: 75\% on $Beta( 2, 2)$, rescaled between -0.4 and 0.
#' @return A named list containing `SurvivalTime`, `ErrorCode`, and the subject-level `TrueHR` vector.
######################################################################################################################## .

SimulatePatientSurvivalAssurance <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{
    # Step 1 - Determine how many patients on each treatment need to be simulated ####
    vTrtAllocation <- table( TreatmentID )
    vSurvTime      <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.

    ErrorCode      <- 0

    # Step 2: First sample the piece of the prior we want to use ####

    nPriorPart <- rbinom( 1, 1, UserParam$dWeight1 )

    if( nPriorPart == 1 )
    {
        # Sample the normal part
        dLogTrueHazard <- rnorm( 1, UserParam$dPriorMean, UserParam$dPriorSD )
    }
    else
    {
        # Comes from Beta( UserParam$dAlpha, UserParam$dBeta) scaled to ( UserParam$dLower, UserParam$dUpper )
        dLogTrueHazard <- rbeta( 1, UserParam$dAlpha, UserParam$dBeta )

        dWidth <- UserParam$dUpper - UserParam$dLower
        dLogTrueHazard <- dWidth * ( dLogTrueHazard ) + UserParam$dLower

    }

    # Step 3: Compute the hazard on experimental given the true hazard on control and the sample dLogHazard ####
    dTrueHazard <- exp( dLogTrueHazard )
    dRateCtrl   <- 1.0 / UserParam$dMeanTTECtrl
    dRateExp    <- dTrueHazard * dRateCtrl

    vRates      <- c( dRateCtrl, dRateExp )

    for( i in 1:NumSub )
    {
        vSurvTime[ i ] <- rexp( 1, vRates[ TreatmentID[ i ] + 1 ] )
    }

    #vTrt1 <- rexp( vTrtAllocation[ 1 ], vRates[ 1 ] )
    #vTrt2 <- rexp( vTrtAllocation[ 2 ], vRates[ 2 ] )

    #vSurvTime[ TreatmentID == 0 ] <- vTrt1
    #vSurvTime[ TreatmentID == 1 ] <- vTrt2

    lRet <- list( SurvivalTime = as.double( vSurvTime ),
                  ErrorCode    = as.integer( ErrorCode ) ,
                  TrueHR       = as.double( rep( dTrueHazard, NumSub ) ) )

    return( lRet )
}
