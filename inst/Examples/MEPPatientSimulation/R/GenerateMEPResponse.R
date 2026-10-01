######################################################################################################################## .
#' @name GenerateMEPResponse
#' @title Simulate multiple-endpoint subject responses
#' @description Generate correlated continuous, binary, and time-to-event responses for multiple endpoints
#'   using a Gaussian copula and the endpoint-specific parameters in RespParams.
#' @author Anoop Singh Rawat, Gabriel Potvin
#' @param NumPat Integer number of subjects in the trial.
#' @param NumArms Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#' @param EndpointType Integer vector of endpoint types, in EndpointName order: 0 = continuous, 1 = binary, 2 =
#'   time-to-event.
#' @param EndpointName Character vector of endpoint names, in the order specified in East Horizon. Use the actual
#'   names to access endpoint-specific list elements.
#' @param RespParams List of endpoint-specific parameter lists, in EndpointName order.
#' \describe{
#'   \item{Continuous (EndpointType = 0)}{Control and Treatment each contain the mean and standard deviation, in
#'     that order, for example `list( Control = c( Mean = 5, SD = 2 ), Treatment = c( Mean = 10, SD = 2 ) )`.}
#'   \item{Binary (EndpointType = 1)}{Control and Treatment are response probabilities between 0 and 1, for example
#'     `list( Control = 0.1, Treatment = 0.5 )`.}
#'   \item{Time-to-event (EndpointType = 2)}{SurvMethod selects 1 = hazard rates, 2 = cumulative survival
#'     percentages, or 3 = median survival times. Control contains the method-specific control parameters and HR
#'     contains treatment-to-control hazard ratios. For method 1, NumPiece is the number of hazard pieces and
#'     StartAtTime contains their starting times. For method 2, ByTime contains the times at which Control survival
#'     percentages are specified. Method 3 uses the control median survival time.}
#' }
#' @param Correlation Square matrix of integer correlation categories in EndpointName order. Category 0 =
#'   uncorrelated; absolute values 1, 2, 3, 4, and 5 indicate very weak, weak, moderate, strong, and very strong
#'   correlation. Positive values indicate positive correlation; negative values indicate negative correlation.
#'   This is an engine category matrix, not a numeric Pearson correlation matrix.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response}{Required named list of numeric response vectors, indexed by EndpointName, with one value per
#'     subject in each vector. Time-to-event responses are measured from enrollment.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details The integration-point documentation also lists optional ArrivalRank and Corr outputs but does not
#'   specify their structure. These templates and examples return Response and ErrorCode; consult the requirements
#'   of the target East Horizon version before using those optional outputs.
#'
#' This example maps integer correlation categories to latent-normal correlations 0, 0.15, 0.3, 0.5, 0.7, and 0.85
#'   with matching signs, and sets diagonal correlations to 1. Non-integer numeric correlation matrices are also
#'   accepted for direct R calls. The resulting matrix must be positive definite. For time-to-event endpoints this
#'   example supports a single hazard period; extend the response-generation logic before using piecewise survival
#'   inputs.
######################################################################################################################## .

GenerateMEPResponse <- function( NumPat, NumArms, TreatmentID, ArrivalTime, EndpointType, EndpointName, RespParams, Correlation, UserParam = NULL ) {
    nErrorCode <- 0
    lResponse <- list( )
    nEndpoints <- length( EndpointType )

    # Convert the engine's qualitative categories to latent-normal correlations.
    # Numeric correlation matrices remain supported for direct calls from R.
    mCorrelation <- Correlation
    if ( all( Correlation == round( Correlation ) ) ) {
        if ( any( abs( Correlation ) > 5 ) ) {
            stop( "Correlation categories must be integers between -5 and 5." )
        }
        vCorrelations <- c( -0.85, -0.7, -0.5, -0.3, -0.15, 0, 0.15, 0.3, 0.5, 0.7, 0.85 )
        mCorrelation <- matrix( vCorrelations[ Correlation + 6 ], nrow = nEndpoints, ncol = nEndpoints )
        diag( mCorrelation ) <- 1
    }
    mChol <- chol( mCorrelation )

    # Generating (NumPat * nEndpoints) standard normal responses
    mZ <- matrix( stats::rnorm( NumPat * nEndpoints, 0, 1 ), ncol = nEndpoints )

    # Intermediate matrix with correlated normal responses
    mNormResp <- mZ %*% mChol

    # Loop through each endpoint
    for ( nEndpointIndex in seq_len( nEndpoints ) ) {
        vPatientOutcome <- rep( 0, NumPat )

        if ( EndpointType[ nEndpointIndex ] == 2 ) { # Time-to-event endpoint
            # Get parameters from RespParams
            lParams <- RespParams[[ EndpointName[ nEndpointIndex ] ]]
            dHR <- lParams$HR

            if ( lParams$SurvMethod == 1 ) { # Hazard rates
                vHazardCtrl <- lParams$Control
                vHazardTrt <- vHazardCtrl * dHR
            } else if ( lParams$SurvMethod == 2 ) { # Cumulative % survival
                # Convert cumulative survival to hazard rate
                dTime <- lParams$ByTime
                dSurvCtrl <- lParams$Control / 100
                vHazardCtrl <- -log( dSurvCtrl ) / dTime
                vHazardTrt <- vHazardCtrl * dHR
            } else if ( lParams$SurvMethod == 3 ) { # Median survival times
                dMedianCtrl <- lParams$Control
                vHazardCtrl <- log( 2 ) / dMedianCtrl
                vHazardTrt <- vHazardCtrl * dHR
            }

            # Generate survival times
            for ( nSubjID in 1:NumPat ) {
                dHazard <- ifelse( TreatmentID[ nSubjID ] == 0, vHazardCtrl, vHazardTrt )
                vPatientOutcome[ nSubjID ] <- -log( stats::pnorm( mNormResp[ nSubjID, nEndpointIndex ] ) ) / dHazard
            }
        } else if ( EndpointType[ nEndpointIndex ] == 1 ) { # Binary endpoint
            # Get parameters from RespParams
            lParams <- RespParams[[ EndpointName[ nEndpointIndex ] ]]
            vPropResp <- c( lParams$Control, lParams$Treatment )

            # Thresholds for binary outcome
            vThreshold <- stats::qnorm( vPropResp )

            for ( nSubjID in 1:NumPat ) {
                vPatientOutcome[ nSubjID ] <- as.numeric( mNormResp[ nSubjID, nEndpointIndex ] < vThreshold[ TreatmentID[ nSubjID ] + 1 ] )
            }
        } else if ( EndpointType[ nEndpointIndex ] == 0 ) { # Continuous endpoint
            # Get parameters from RespParams
            lParams <- RespParams[[ EndpointName[ nEndpointIndex ] ]]
            vMeanCtrl <- lParams$Control[ 1 ]
            vSDCtrl <- lParams$Control[ 2 ]
            vMeanTrt <- lParams$Treatment[ 1 ]
            vSDTrt <- lParams$Treatment[ 2 ]

            for ( nSubjID in 1:NumPat ) {
                if ( TreatmentID[ nSubjID ] == 0 ) {
                    vPatientOutcome[ nSubjID ] <- vMeanCtrl + vSDCtrl * mNormResp[ nSubjID, nEndpointIndex ]
                } else {
                    vPatientOutcome[ nSubjID ] <- vMeanTrt + vSDTrt * mNormResp[ nSubjID, nEndpointIndex ]
                }
            }
        }

        # Check for errors
        if ( length( vPatientOutcome ) != NumPat || any( is.na( vPatientOutcome ) == TRUE ) ) {
            stop( paste( "Error generating patient outcomes for endpoint", EndpointName[ nEndpointIndex ], ": Invalid or missing values detected" ) )
        }

        # Store response
        lResponse[[ EndpointName[ nEndpointIndex ] ]] <- vPatientOutcome
    }

    return( list( Response = as.list( lResponse ), ErrorCode = as.integer( nErrorCode ) ) )
}
