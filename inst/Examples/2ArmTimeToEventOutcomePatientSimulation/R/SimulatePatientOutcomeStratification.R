######################################################################################################################## .
#' @name SimulatePatientOutcomeStratification
#'
#' @title Simulate patient outcomes using stratification
#'
#' @description This function generates patient survival times across multiple strata based on the parameters
#'   specified in the Response Generation table.
#'
#' For each stratum, the corresponding survival parameters (hazard rates, cumulative \% survival, or medians)
#' are converted into hazard rates. Then patient-level survival times are simulated using an
#' Exponential distribution:
#' \deqn{ T \sim \text{Exponential}(\lambda) }
#'
#' @author Valeria A. G. Mazzanti, J. Kyle Wathen, and Gabriel Potvin
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
#' @param StratumID Integer vector of stratum indices, with one element per subject. Stratum indices start at 1.
#'   Available when stratification is enabled.
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
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#'
#' @details Usage of ArrivalTime in this example: A vector of subject arrival times. (Not used in this function but
#'   required for integration.)
#'
#' Usage of UserParam in this example: A list of user-defined parameters in East Horizon (not used in this
#'   function). The default is NULL.
######################################################################################################################## .

SimulatePatientOutcomeStratification <- function( NumSub, NumArm, ArrivalTime, TreatmentID,
                                                 StratumID, SurvMethod, NumPrd, PrdTime,
                                                 SurvParam, UserParam = NULL ) {
    nErrorCode <- 0

    # Initialize vectors
    vSurvResponses <- rep( NA_real_, NumSub )
    vUniqueStrata <- unique( StratumID )

    # Loop through strata
    for ( nStratumIdx in seq_along( vUniqueStrata ) ) {
        nStratumInd <- vUniqueStrata[ nStratumIdx ]

        # Number of subjects in this stratum
        vSubjectIndices <- which( StratumID == nStratumInd )
        nStratumSubjects <- length( vSubjectIndices )

        # Response Gen params in this stratum
        vStratumParams <- SurvParam[ nStratumInd, ]

        vPatientOutcome <- rep( 0, nStratumSubjects )
        vTreatmentIndex <- TreatmentID[ vSubjectIndices ] + 1 # TreatmentID is 0-based

        # SurvMethod 1: Hazard Rates
        if ( SurvMethod == 1 ) {
            vHazardRates <- vStratumParams
        }

        # SurvMethod 2: Cumulative % Survival
        if ( SurvMethod == 2 ) {
            vSurvTimes <- rep( as.numeric( PrdTime[ 1 ] ), NumArm )
            if ( is.matrix( PrdTime ) ) {
                vSurvTimes <- PrdTime[ nStratumInd, ]
            }
            vS <- vStratumParams / 100
            vHazardRates <- rep( NA, NumArm )

            for ( nArmIdx in 1:NumArm ) {
                dSurvTime <- vSurvTimes[ nArmIdx ]
                if ( vS[ nArmIdx ] > 0 && vS[ nArmIdx ] < 1 && dSurvTime > 0 ) {
                    vHazardRates[ nArmIdx ] <- -log( vS[ nArmIdx ] ) / dSurvTime
                } else {
                    vHazardRates[ nArmIdx ] <- NA
                    nErrorCode <- 1
                }
            }
        }

        # SurvMethod 3: Median Survival
        if ( SurvMethod == 3 ) {
            vMedian <- vStratumParams
            vHazardRates <- rep( NA, NumArm )

            for ( nArmIdx in 1:NumArm ) {
                if ( vMedian[ nArmIdx ] > 0 ) {
                    vHazardRates[ nArmIdx ] <- log( 2 ) / vMedian[ nArmIdx ]
                } else {
                    vHazardRates[ nArmIdx ] <- NA
                    nErrorCode <- 1
                }
            }
        }

        # Generation of Responses
        for ( nPatIndx in 1:nStratumSubjects ) {
            nArm <- vTreatmentIndex[ nPatIndx ]
            dRate <- vHazardRates[ nArm ]

            if ( !is.na( dRate ) && dRate > 0 ) {
                vPatientOutcome[ nPatIndx ] <- stats::rexp( 1, dRate )
            } else {
                vPatientOutcome[ nPatIndx ] <- NA
            }
        }

        # Return each outcome in the same subject order as TreatmentID and StratumID.
        vSurvResponses[ vSubjectIndices ] <- vPatientOutcome
    }

    # For consistency checks
    if ( length( vSurvResponses ) != NumSub || any( is.na( vSurvResponses ) ) ) {
        nErrorCode <- -100
    }

    return( list(
        SurvivalTime = as.double( vSurvResponses ),
        ErrorCode = as.integer( nErrorCode )
    ) )
}
