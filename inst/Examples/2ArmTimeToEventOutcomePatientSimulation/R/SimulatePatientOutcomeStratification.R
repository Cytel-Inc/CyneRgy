######################################################################################################################## .
#' @name SimulatePatientOutcomeStratification
#' @title Simulate patient outcomes using stratification
#' @author Valeria A. G. Mazzanti, J. Kyle Wathen, and Gabriel Potvin
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' For a two-arm trial this will be 2.
#'
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject. Required for integration but not used by this example.
#'
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' TreatmentID uses 0-based indexing internally:
#' \itemize{
#'   \item{0 = Arm 1 (control)}
#'   \item{1 = Arm 2 (experimental)}
#' }
#' Length of TreatmentID must equal NumSub.
#'
#' @param StratumID Integer vector of length `NumSub`, indicating each subject's 1-based stratum ID.
#' Subjects sharing the same value belong to the same stratum.
#'
#' @param SurvMethod This value is pulled from the Input Method drop-down list.
#' Allowed values:
#' \itemize{
#'   \item{1 = Hazard Rates (direct)}
#'   \item{2 = Cumulative \% Survival}
#'   \item{3 = Median Survival Times}
#' }
#'
#' @param NumPrd Number of time periods provided in the survival parameter table.
#'
#' @param PrdTime Numeric matrix with one row per stratum and `NumArm` columns, indicating the times used to specify stratum-by-arm survival parameters. For `SurvMethod = 1`, entries are hazard-piece start times; for `SurvMethod = 2`, entries are times at which cumulative survival is specified; for `SurvMethod = 3`, entries default to 0.
#'
#' @param SurvParam
#' A 2-D array providing survival parameters per stratum.
#' Each row corresponds to **one stratum**, and each column corresponds to an arm:
#'
#' \describe{
#'
#'   \item{If SurvMethod = 1}{SurvParam stores hazard rates (one per arm per stratum).
#'   SurvParam[i, j] = hazard rate for stratum *i* and arm *j*.}
#'
#'   \item{If SurvMethod = 2}{SurvParam stores cumulative \% survival values per arm.
#'   SurvParam[i, j] = cumulative \% survival for stratum *i* and arm *j*.}
#'
#'   \item{If SurvMethod = 3}{SurvParam stores median survival times per arm.
#'   SurvParam[i, j] = median survival time for stratum *i* and arm *j*.}
#'
#' }
#'
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' The default is NULL.
#'
#'
#' @description
#' This function generates patient survival times across multiple strata based on the
#' parameters specified in the Response Generation table.
#'
#' For each stratum, the corresponding survival parameters (hazard rates, cumulative \% survival, or medians)
#' are converted into hazard rates. Then patient-level survival times are simulated using an
#' Exponential distribution:
#' \deqn{ T \sim \text{Exponential}(\lambda) }
#'
#' @return A named list containing numeric vector `SurvivalTime` and integer `ErrorCode`.
######################################################################################################################## .
SimulatePatientOutcomeStratification <- function( NumSub, NumArm, ArrivalTime, TreatmentID,
                                                StratumID, SurvMethod, NumPrd, PrdTime,
                                                SurvParam, UserParam = NULL )
{
    nError <- 0

    # Initialize vectors
    vSurvResponses <- numeric( 0 )
    vUniqueStrata <- unique( StratumID )

    # Loop through strata
    for( nStratumIdx in seq_along( vUniqueStrata ) )
    {
        nStratumInd <- vUniqueStrata[ nStratumIdx ]

        # Number of subjects in this stratum
        nStratumSubjects <- sum( StratumID == nStratumInd )

        # Response Gen params in this stratum
        vStratumParams <- SurvParam[ nStratumIdx, ]

        vPatientOutcome <- rep( 0, nStratumSubjects )
        vTreatmentIndex <- TreatmentID + 1  # TreatmentID is 0-based

        # SurvMethod 1: Hazard Rates
        if( SurvMethod == 1 )
        {
          vHazardRates <- vStratumParams
        }

        # SurvMethod 2: Cumulative % Survival
        if( SurvMethod == 2 )
        {
          dSurvTime <- as.numeric( PrdTime[ 1 ] )
          vS <- vStratumParams / 100
          vHazardRates <- rep( NA, NumArm )

          for( nArmIdx in 1:NumArm )
          {
            if( vS[ nArmIdx ] > 0 && vS[ nArmIdx ] < 1 && dSurvTime > 0 )
              vHazardRates[ nArmIdx ] <- -log( vS[ nArmIdx ] ) / dSurvTime
            else
            {
              vHazardRates[ nArmIdx ] <- NA
              nError <- 1
            }
          }
        }

        # SurvMethod 3: Median Survival
        if( SurvMethod == 3 )
        {
          vMedian <- vStratumParams
          vHazardRates <- rep( NA, NumArm )

          for( nArmIdx in 1:NumArm )
          {
            if( vMedian[ nArmIdx ] > 0 )
              vHazardRates[ nArmIdx ] <- log( 2 ) / vMedian[ nArmIdx ]
            else
            {
              vHazardRates[ nArmIdx ] <- NA
              nError <- 1
            }
          }
        }

        # Generation of Responses
        for( nPatIndx in 1:nStratumSubjects )
        {
            nArm <- vTreatmentIndex[ nPatIndx ]
            dRate <- vHazardRates[ nArm ]

            if( !is.na( dRate ) && dRate > 0 )
                vPatientOutcome[ nPatIndx ] <- rexp( 1, dRate )
            else
                vPatientOutcome[ nPatIndx ] <- NA
        }

        # Append strata-wise responses
        vSurvResponses <- c( vSurvResponses, vPatientOutcome )
    }

    # For consistency checks
    if( length( vSurvResponses ) != NumSub || any( is.na( vSurvResponses ) ) )
        nError <- -100

    return( list(
        SurvivalTime = as.double( vSurvResponses ),
        ErrorCode = as.integer( nError )
    ) )
}
