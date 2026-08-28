######################################################################################################################## .
#' @name GenerateMEPResponse
#' @title Generate Correlated Multi-Endpoint Patient Responses
#' @description Generates correlated continuous, binary, and time-to-event responses for a multiple-endpoint trial.
#' @author Anoop Singh Rawat, Gabriel Potvin
#' @param NumPat Integer. Number of patients in the trial.
#' @param NumArms Integer. Number of arms in the trial, including control.
#' @param TreatmentID Integer vector containing each patient's treatment assignment, with 0 denoting control.
#' @param ArrivalTime Numeric vector containing patient arrival times.
#' @param EndpointType Integer vector containing endpoint types: 0 for continuous, 1 for binary, and 2 for
#' time-to-event.
#' @param EndpointName Character vector containing endpoint names.
#' @param RespParams Named list of response parameters for each endpoint. Continuous entries contain `Control` and
#' `Treatment` mean/standard-deviation pairs; binary entries contain control and treatment probabilities; and
#' time-to-event entries contain `SurvMethod`, its time parameters, `Control`, and `HR`.
#' @param Correlation Numeric correlation matrix with one row and column per endpoint.
#' @param UserParam Optional list of user-defined parameters. This example does not use it. Defaults to `NULL`.
#' @return A list containing `Response`, a named list of response vectors in `EndpointName` order, and `ErrorCode`,
#' an integer status code where 0 indicates success.
######################################################################################################################## .

GenerateMEPResponse <- function( NumPat, NumArms, TreatmentID, ArrivalTime, EndpointType, EndpointName, RespParams, Correlation, UserParam = NULL )
{
    nError    <- 0
    lResponse <- list()
    nNumEP    <- length( EndpointType )

    # Cholesky decomposition of correlation matrix
    mChol <- chol( Correlation )

    # Generating (NumPat * NumEP) standard normal responses
    mZ <- matrix( rnorm( NumPat * nNumEP, 0, 1 ), ncol = nNumEP )

    # Intermediate matrix with correlated normal responses
    mNormResp <- mZ %*% mChol

    # Loop through each endpoint
    for( nEP in seq_len( nNumEP ) )
    {
        vPatientOutcome <- rep( 0, NumPat )

        if( EndpointType[ nEP ] == 2 ) # Time-to-event endpoint
        {
            # Get parameters from RespParams
            lParams <- RespParams[[ EndpointName[ nEP ] ] ]
            dHR <- lParams$HR

            if( lParams$SurvMethod == 1 ) # Hazard rates
            {
                vHazardCtrl <- lParams$Control
                vHazardTrt <- vHazardCtrl * dHR
            }
            else if( lParams$SurvMethod == 2 ) # Cumulative percentage survival
            {
                # Convert cumulative survival to hazard rate
                dTime <- lParams$ByTime
                dSurvCtrl <- lParams$Control / 100
                vHazardCtrl <- -log( dSurvCtrl ) / dTime
                vHazardTrt <- vHazardCtrl * dHR
            }
            else if( lParams$SurvMethod == 3 ) # Median survival times
            {
                dMedianCtrl <- lParams$Control
                vHazardCtrl <- log( 2 ) / dMedianCtrl
                vHazardTrt <- vHazardCtrl * dHR
            }

            # Generate survival times
            for( nSubjID in 1:NumPat )
            {
                dHazard <- ifelse( TreatmentID[ nSubjID ] == 0, vHazardCtrl, vHazardTrt )
                vPatientOutcome[ nSubjID ] <- -log( pnorm( mNormResp[ nSubjID, nEP ] ) ) / dHazard
            }
        }
        else if( EndpointType[ nEP ] == 1 ) # Binary endpoint
        {
            # Get parameters from RespParams
            lParams <- RespParams[[ EndpointName[ nEP ] ] ]
            vPropResp <- c( lParams$Control, lParams$Treatment )

            # Thresholds for binary outcome
            vThreshold <- qnorm( vPropResp )

            for( nSubjID in 1:NumPat )
            {
                vPatientOutcome[ nSubjID ] <- as.numeric( mNormResp[ nSubjID, nEP ] < vThreshold[ TreatmentID[ nSubjID ] + 1 ] )
            }
        }
        else if( EndpointType[ nEP ] == 0 ) # Continuous endpoint
        {
            # Get parameters from RespParams
            lParams <- RespParams[[ EndpointName[ nEP ] ] ]
            vMeanCtrl <- lParams$Control[ 1 ]
            vSDCtrl <- lParams$Control[ 2 ]
            vMeanTrt <- lParams$Treatment[ 1 ]
            vSDTrt <- lParams$Treatment[ 2 ]

            for( nSubjID in 1:NumPat )
            {
                if( TreatmentID[ nSubjID ] == 0 )
                {
                    vPatientOutcome[ nSubjID ] <- vMeanCtrl + vSDCtrl * mNormResp[ nSubjID, nEP ]
                }
                else
                {
                    vPatientOutcome[ nSubjID ] <- vMeanTrt + vSDTrt * mNormResp[ nSubjID, nEP ]
                }
            }
        }

        # Check for errors
        if( length( vPatientOutcome ) != NumPat || any( is.na( vPatientOutcome ) == TRUE ) )
        {
            stop( paste( "Error generating patient outcomes for endpoint", EndpointName[ nEP ], ": Invalid or missing values detected" ) )
        }

        # Store response
        lResponse[[ EndpointName[ nEP ] ] ] <- vPatientOutcome
    }

    return( list( Response = as.list( lResponse ), ErrorCode = as.integer( nError ) ) )
}
