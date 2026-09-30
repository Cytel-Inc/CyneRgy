######################################################################################################################## .
#' @name SimulatePatientOutcomeDEPSurvBinSingleHazardPiece
#'
#' @title Simulate survival-binary dual endpoints with one hazard period
#'
#' @description Generate correlated survival and binary outcomes using a Gaussian copula. Simulate two standard
#'   normal samples, impose the requested correlation on the normal scale, then transform one endpoint to
#'   exponential survival times and the other to binary responses using the endpoint-specific inputs. This example
#'   supports one hazard period.
#'
#' @author Gabriel Potvin, Anoop Singh Rawat, Pradip Maske
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
#' @param EndpointType Integer vector of endpoint types, in EndpointName order: 0 = continuous, 1 = binary, 2 =
#'   time-to-event.
#'
#' @param EndpointName Character vector of endpoint names, in the order specified in East Horizon. Use the actual
#'   names to access endpoint-specific list elements.
#'
#' @param Correlation Integer correlation category between endpoints: 0 = uncorrelated; absolute values 1, 2, 3, 4,
#'   and 5 indicate very weak, weak, moderate, strong, and very strong correlation. Positive values indicate
#'   positive correlation; negative values indicate negative correlation.
#'
#' @param SurvMethod Named list indexed by EndpointName. For each time-to-event endpoint: 1 = hazard rates; 2 =
#'   cumulative survival percentages; 3 = median survival times. The value is NA for a non-survival endpoint.
#'
#' @param NumPrd Named list indexed by EndpointName, containing the integer number of survival periods for each
#'   time-to-event endpoint and NA for a non-survival endpoint.
#'
#' @param PrdTime Named list indexed by EndpointName. Each survival endpoint contains its period times: starting
#'   times of hazard pieces for SurvMethod = 1, times for cumulative survival percentages for SurvMethod = 2, or 0
#'   for SurvMethod = 3. The value is NA for a non-survival endpoint.
#'
#' @param SurvParam Named list indexed by EndpointName. Each survival endpoint contains a NumPrd-by-NumArm array:
#'   hazard rates for SurvMethod = 1, cumulative survival percentages for SurvMethod = 2, or a single row of median
#'   survival times for SurvMethod = 3. Column 1 is control. The value is NA for a non-survival endpoint.
#'
#' @param PropResp Named list indexed by EndpointName. Each binary endpoint contains a numeric vector of response
#'   probabilities by arm, with control first; the value is NA for a non-binary endpoint.
#'
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Response}{Required named list of numeric response vectors, indexed by EndpointName, with one value per
#'     subject in each vector. Time-to-event responses are measured from enrollment.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

SimulatePatientOutcomeDEPSurvBinSingleHazardPiece <- function( NumSub, NumArm, ArrivalTime = NULL, TreatmentID,
                                                              EndpointType, EndpointName, Correlation, SurvMethod,
                                                              NumPrd, PrdTime, SurvParam, PropResp = NULL,
                                                              UserParam = NULL ) {
    nErrorCode <- 0
    vPatientOutcomeEP1 <- rep( 0, NumSub )
    vPatientOutcomeEP2 <- rep( 0, NumSub )
    lResponse <- list( )

    if ( !is.null( UserParam ) ) {
        # Customized logic for data generation using UserParam will go here.
    } else {
        # Get correlation matrix given qualitative correlation input
        mCor <- GetCorrMatrix( Correlation )

        # Cholesky decomposition of correlation matrix
        mChol <- chol( mCor )

        # Generating (NumSub * 2) standard normal responses
        mZ <- matrix( stats::rnorm( NumSub * 2, 0, 1 ), ncol = 2 )

        # Intermediate matrix
        mNormResp <- mZ %*% mChol

        # Thresholds for binary EP
        threshold <- stats::qnorm( PropResp[[ 2 ]] )

        for ( nSubjID in 1:NumSub ) {
            # EP1: Survival time
            vPatientOutcomeEP1[ nSubjID ] <- ( -log( stats::pnorm( mNormResp[ nSubjID, 1 ] ) ) / SurvParam[[ 1 ]][ 1, TreatmentID[ nSubjID ] + 1 ] )

            # EP2: Binary outcome using inverse probability transform
            vPatientOutcomeEP2[ nSubjID ] <- as.numeric( mNormResp[ nSubjID, 2 ] < threshold[ TreatmentID[ nSubjID ] + 1 ] )
        }
        if ( length( vPatientOutcomeEP1 ) != NumSub || any( is.na( vPatientOutcomeEP1 ) == TRUE ) ||
            length( vPatientOutcomeEP2 ) != NumSub || any( is.na( vPatientOutcomeEP2 ) == TRUE ) ) {
            nErrorCode <- -100
        }
    }

    lResponse[[ EndpointName[[ 1 ]] ]] <- vPatientOutcomeEP1
    lResponse[[ EndpointName[[ 2 ]] ]] <- vPatientOutcomeEP2

    return( list( Response = as.list( lResponse ), ErrorCode = as.integer( nErrorCode ) ) )
}

# Helper function to create correlation matrix given qualitative correlation
GetCorrMatrix <- function( Correlation ) {
    rho <- ifelse( Correlation == 0, 0,
        ifelse( Correlation == 1, 0.15,
            ifelse( Correlation == 2, 0.3,
                ifelse( Correlation == 3, 0.5,
                    ifelse( Correlation == 4, 0.7,
                        ifelse( Correlation == 5, 0.85,
                            ifelse( Correlation == -1, -0.15,
                                ifelse( Correlation == -2, -0.3,
                                    ifelse( Correlation == -3, -0.5,
                                        ifelse( Correlation == -4, -0.7,
                                            ifelse( Correlation == -5, -0.85 )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )

    # Return the 2x2 correlation matrix
    return( matrix( c( 1, rho, rho, 1 ), nrow = 2 ) )
}
