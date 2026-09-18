######################################################################################################################## .
#' @name SimulatePatientOutcomeStratification
#' @title Simulate patient outcomes using stratification
#' @author Valeria A. G. Mazzanti and J. Kyle Wathen
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param StratumID Integer vector of length `NumSub`, indicating each subject's 1-based stratum ID.
#' @param SurvMethod Integer survival-generation method: 1 for hazard rates, 2 for cumulative survival probabilities, or 3 for median survival times.
#' @param NumPrd Integer number of survival periods.
#' @param PrdTime Numeric matrix with one row per stratum and `NumArm` columns, indicating the times used to specify stratum-by-arm survival parameters. For `SurvMethod = 1`, entries are hazard-piece start times; for `SurvMethod = 2`, entries are times at which cumulative survival is specified; for `SurvMethod = 3`, entries default to 0.
#' @param SurvParam \describe{Depends on the table in the Response Generation tab.
#'    A 2-D array of parameters to generate the survival times, defined by stratum and arm.
#'
#'    \item{If SurvMethod = 1}{SurvParam is an array (NumStratum rows, NumArm columns)
#'    that specifies stratum-by-arm hazard rates (one rate per arm per stratum).
#'    Thus, SurvParam[i, j] specifies the hazard rate for the i-th stratum and j-th arm.
#'    Arms are in columns, with column 1 as control and column 2 as experimental.}
#'
#'    \item{If SurvMethod = 2}{SurvParam is an array (NumStratum rows, NumArm columns)
#'    that specifies stratum-by-arm cumulative \% survival values
#'    (one value per arm per stratum).
#'    Thus, SurvParam[i, j] specifies the cumulative \% survival
#'    for the i-th stratum and j-th arm.}
#'
#'    \item{If SurvMethod = 3}{SurvParam is an array (NumStratum rows, NumArm columns)
#'    that specifies stratum-by-arm median survival times.
#'    Thus, SurvParam[i, j] specifies the median survival time
#'    for the i-th stratum and j-th arm.
#'    Column 1 is control and column 2 is experimental.}
#' }
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#' @description
#' This function generates patient survival times across multiple strata based on the
#' parameters specified in the Response Generation table.
#'
#' For each stratum, the corresponding survival parameters (hazard rates, cumulative \% survival, or medians)
#' are converted into hazard rates. Then patient-level survival times are simulated using an
#' Exponential distribution:
#' \deqn{ T \sim \text{Exponential}(\lambda) }
#'
#' @return A list that contains:
#' \describe{
#'     \item{SurvivalTime}{A numeric vector of length `NumSub` containing the simulated time-to-event outcomes.}
#'     \item{ErrorCode}{An integer value: ErrorCode = 0 indicates no error; ErrorCode > 0 indicates a nonfatal error and aborts the current simulation, but subsequent simulations continue; ErrorCode < 0 indicates a fatal error and stops further simulation.}
#' }
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
