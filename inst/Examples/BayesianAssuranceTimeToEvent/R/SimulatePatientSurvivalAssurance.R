######################################################################################################################## .
#' @name SimulatePatientSurvivalAssurance
#' @title Simulate Time-To-Event Data for Assurance
#' @author J. Kyle Wathen and Laurent Spiess
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param SurvMethod - This values is pulled from the Input Method drop-down list. This will be 1 (Hazard Rate), 2 (Cumulative \% survival), 3 (Medians)
#' @param NumPrd Number of time periods that are provided.
#' @param PrdTime Numeric matrix with `NumPrd` rows and `NumArm` columns, indicating the times used to specify survival parameters. For `SurvMethod = 1`, entries are hazard-piece start times; for `SurvMethod = 2`, entries are times at which cumulative survival is specified; for `SurvMethod = 3`, entries default to 0.
#' @param SurvParam \describe{Depends on the table in the Response Generation tab. 2‐D array of parameters to generate the survival times
#'    \item{If SurvMethod is 1}{SurvParam is an array (NumPrd rows, NumArm columns) that specifies arm by arm hazard rates (one rate per arm per piece).
#'    Thus SurvParam [i, j] specifies hazard rate in ith period for jth arm.
#'    Arms are in columns with column 1 is control, column 2 is experimental
#'    Time periods are in rows, row 1 is time period 1, row 2 is time period 2...}
#'    \item{If SurvMethod is 2}{SurvParam is an array (NumPrd rows,NumArm columns) specifies arm by arm the Cum \% Survivals (one value per arm per piece). Thus, SurvParam [i, j] specifies Cum \% Survivals in ith period for jth arm.}
#'    \item{If SurvMethod is 3}{SurvParam will be a 1 x 2 array with median survival times on each arms. Column 1 is control, column 2 is experimental }
#'  }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' UserParam must be supplied and contain the following named elements:
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
