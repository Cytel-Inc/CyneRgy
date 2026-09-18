######################################################################################################################## .
#' @name SimulatePatientSurvivalAssuranceUsingPh2Prior
#' @title Simulate Patient Survival Times Using a Phase 2 Prior for Assurance
#' @description Function simulates from exponential, just included as a simple example as a starting point
#' @author J. Kyle Wathen, Laurent Spiess, Gabriel Potvin
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
#'      \item{UserParam$dIntercept}{Intercept for the linear relationship between true treatment difference and log(HR).}
#'      \item{UserParam$dSlope}{Slope for the linear relationship between true treatment difference and log(HR).}
#'      \item{UserParam$dMeanTTECtrl}{Mean time-to-event for the control group.}
#'   }
#' @return A named list containing `SurvivalTime`, the subject-level `TrueHR` vector, and `ErrorCode`. The function
#' loads Phase 2 results into global prior state on first use and advances the global simulation index on each call.
######################################################################################################################## .

SimulatePatientSurvivalAssuranceUsingPh2Prior <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{
      if( !exists( "gvPrior" ) )
      {
        # Load prior obtained from Phase 2
        gvPrior    <<- LoadData()
        gnIndex    <<- 1
      }

    # Step 1 - Determine how many patients on each treatment need to be simulated ####
    vTrtAllocation <- table( TreatmentID )
    vSurvTime      <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.

    ErrorCode    <- rep( -1, NumSub )

    # Step 2: Using the true treatment difference from Ph 2, compute the log( true hazard ratio) ####
    dTrueTreatmentDiff <- gvPrior[ gnIndex ]
    gnIndex <<- gnIndex + 1

    dLogTrueHazardRatio <- UserParam$dIntercept + UserParam$dSlope * dTrueTreatmentDiff
    dTrueHazardRatio    <- exp( dLogTrueHazardRatio )

    # Step 3: Compute the hazard on experimental given the true hazard on control and the computed true hazard ratio ####

    dRateCtrl        <- 1.0 / UserParam$dMeanTTECtrl
    dRateExp         <- dTrueHazardRatio * dRateCtrl

    vRates      <- c( dRateCtrl, dRateExp )

    vTrt1 <- rexp( vTrtAllocation[ 1 ], vRates[ 1 ] )
    vTrt2 <- rexp( vTrtAllocation[ 2 ], vRates[ 2 ] )

    vSurvTime[ TreatmentID == 0 ] <- vTrt1
    vSurvTime[ TreatmentID == 1 ] <- vTrt2

    return( list( SurvivalTime = as.double( vSurvTime ), TrueHR = as.double( rep( dTrueHazardRatio, NumSub ) ), ErrorCode = ErrorCode ) )
}

LoadData <- function()
{
  # Step 1 - Process the East Horizon Explore results for Phase 2
  # The CSV file will contain 1 row for each IA that was conducted and the FA.  However, if the trial is stopped early for efficacy or futility
  # it will only contain the IAs that occur.  Therefore, we must find the last analysis for each simulated trial.
  dfEastHorExp <- readr::read_csv( "Inputs/Ph2_results.csv" )

  # Build a data frame with only 1 row per simulated trial with the last analysis.  The last analysis is the analysis (IA or FA) that makes a futility or efficacy decision.
  dfLastAnalysisResults <- dplyr::ungroup(
    dplyr::slice_max(
      dplyr::group_by( dfEastHorExp, SimIndex ),
      AnalysisIndex
    )
  )

  # Step 2 - The Ph3 is only conducted when the Ph2 is successful (Efficacy) so create a data frame of the simulated trials that are successful
  # Select trials that are successful so we can build posterior of true delta when a Go decision is made
  dfConditionalPostOnPh2Success <- dplyr::select(
    dfLastAnalysisResults[ dfLastAnalysisResults$Decision == "Efficacy", ],
    dTrueDelta
  )

  return( dfConditionalPostOnPh2Success$dTrueDelta )
}
