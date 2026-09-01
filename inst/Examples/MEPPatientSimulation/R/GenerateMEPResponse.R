######################################################################################################################## .
#' @name GenerateMEPResponse
#' @title Generate Correlated Multi-Endpoint Patient Responses
#' @description Generates correlated continuous, binary, and time-to-event responses for a multiple-endpoint trial.
#' @author Anoop Singh Rawat, Gabriel Potvin
#' @param NumPat Integer number of patients in the trial.
#' @param NumArms Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param TreatmentID Integer vector of length `NumPat`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#' @param ArrivalTime Numeric vector of length `NumPat`, indicating the arrival time for each subject.
#' @param EndpointType Integer vector identifying each endpoint as continuous (0), binary (1), or time-to-event (2).
#' @param EndpointName Character vector naming the endpoints in `EndpointType` order.
#' @param RespParams List of endpoint-specific generation parameters. Continuous entries contain arm means and standard deviations; binary entries contain arm response probabilities; time-to-event entries contain the survival method, periods, control parameters, and hazard ratios.
#' @param Correlation Numeric correlation-coefficient matrix with one row and column per endpoint and ones on the diagonal.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
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
