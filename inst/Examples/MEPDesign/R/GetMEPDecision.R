######################################################################################################################## .
#' @name GetMEPDecision
#' @title Make multiple-endpoint trial decisions
#' @description This function allows users to implement custom decision-making logic for the MEP engine. It is
#'   called at each analysis look to make decisions about endpoint efficacy, futility and trial continuation based
#'   on user-defined criteria.
#' @author Anoop Singh Rawat, Gabriel Potvin
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$TreatmentID`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{SimID}{Vector of length equal to the number of subjects, indicating the simulation ID.}
#'   \item{PatId}{Vector of length equal to the number of subjects, containing the patient ID identifying a unique
#'     patient in a given simulation.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{TResponse.`EPNAME`}{Vector of length equal to the number of subjects, containing the generated responses
#'     for a specific endpoint. `EPNAME` is the endpoint name specified in East Horizon. Endpoint names can also be
#'     accessed using `DesignParam$EndpointName`.}
#'   \item{CensorID.`EPNAME`}{Vector of length equal to the number of subjects, containing the generated censor
#'     indicator values for a specific endpoint: - `0`: Dropout. - `1`: Completer. `EPNAME` is the endpoint name
#'     specified in East Horizon. Endpoint names can also be accessed using `DesignParam$EndpointName`.}
#'   \item{Response.`EPNAME`}{Vector of length equal to the number of subjects, containing the generated responses
#'     for a specific endpoint after adjusting for endpoint and dropout rules. `EPNAME` is the endpoint name
#'     specified in East Horizon. Endpoint names can also be accessed using `DesignParam$EndpointName`.}
#'   \item{CalRespT.`EPNAME`}{Vector of length equal to the number of subjects, containing the generated response
#'     times on calendar scale for a specific endpoint. Equivalent to `Response + ArrivalTime`. `EPNAME` is the
#'     endpoint name specified in East Horizon. Endpoint names can also be accessed using
#'     `DesignParam$EndpointName`.}
#'   \item{DropoutID.`EPNAME`}{Vector of length equal to the number of subjects, containing whether each patient
#'     dropped out before responding for a specific endpoint: - `0`: Dropout. - `1`: Completer. `EPNAME` is the
#'     endpoint name specified in East Horizon. Endpoint names can also be accessed using
#'     `DesignParam$EndpointName`.}
#' }
#' @param AnalysisData Data frame containing the subset of SimData available at the current analysis look. It uses
#'   the same column definitions as SimData.
#' @param DataSummary Named list of endpoint-specific summary statistics. Access an element by endpoint name, for
#'   example `DataSummary[[DesignParam$EndpointName[1]]]$Events`.
#' \describe{
#'   \item{AvgFollowupTime}{Mean follow-up time.}
#'   \item{MedianFollowupTime}{Median follow-up time.}
#'   \item{Dropouts}{Total number of dropouts.}
#'   \item{Dropouts0}{Number of dropouts in control arm.}
#'   \item{Dropouts1}{Number of dropouts in treatment arm.}
#'   \item{Censored}{Total number of censored subjects.}
#'   \item{Censored0}{Number of censored subjects in control arm.}
#'   \item{Censored1}{Number of censored subjects in treatment arm.}
#'   \item{Pendings}{Total number of pending events or completers.}
#'   \item{Pendings0}{Number of pending events or completers in the control arm.}
#'   \item{Pendings1}{Number of pending events or completers in the treatment arm.}
#'   \item{Events}{Total number of events. Only available for `Endpoint Type = Time-to-Event`.}
#'   \item{Events0}{Number of events in control arm. Only available for `Endpoint Type = Time-to-Event`.}
#'   \item{Events1}{Number of events in treatment arm. Only available for `Endpoint Type = Time-to-Event`.}
#'   \item{Completers}{Total number of completers. Only available for `Endpoint Type = Continuous or Binary`.}
#'   \item{Completers0}{Number of completers in control arm. Only available for `Endpoint Type = Continuous or
#'     Binary`.}
#'   \item{Completers1}{Number of completers in treatment arm. Only available for `Endpoint Type = Continuous or
#'     Binary`.}
#'   \item{HR}{Estimated hazard ratio comparing treatment to control. Only available for `Endpoint Type =
#'     Time-to-Event`.}
#'   \item{HR0}{Hazard-related summary for the control arm. Only available for `Endpoint Type = Time-to-Event`.}
#'   \item{HR1}{Hazard-related summary for the treatment arm. Only available for `Endpoint Type = Time-to-Event`.}
#'   \item{Mean0}{Mean outcome value in the control arm. Only available for `Endpoint Type = Continuous`.}
#'   \item{Mean1}{Mean outcome value in the treatment arm. Only available for `Endpoint Type = Continuous`.}
#'   \item{SD0}{Standard deviation of the outcome in the control arm. Only available for `Endpoint Type =
#'     Continuous`.}
#'   \item{SD1}{Standard deviation of the outcome in the treatment arm. Only available for `Endpoint Type =
#'     Continuous`.}
#'   \item{Delta}{Estimated treatment effect. Only available for `Endpoint Type = Continuous or Binary`}
#'   \item{Prop0}{Observed proportion of responders in the control arm. Only available for `Endpoint Type =
#'     Binary`.}
#'   \item{Prop1}{Observed proportion of responders in the treatment arm. Only available for `Endpoint Type =
#'     Binary`.}
#' }
#' @param LookInfo Named list of information for the current look, including the single look in a fixed-sample
#'   multiple-endpoint design.
#' \describe{
#'   \item{LookNum}{Integer. Current look number.}
#'   \item{TestStatisticsOutputs}{Named List. Named List of length equal to the number of endpoints, containing the
#'     test statistics results from the engine. Names are user-specified endpoint names from
#'     `DesignParam$EndpointName`. Order also matches `DesignParam$EndpointName` order. Each element contains the
#'     integer `ErrorCode` (0 = Success) and the list `data`, which contains: - For TTE endpoint: `Score`
#'     (numeric), `StdErr` (numeric standard error), `TS` (numeric test statistic), `TSPVal` (numeric p-value). -
#'     For Binary endpoint: `PropPld` (numeric pooled proportion), `StdErr` (numeric standard error), `TS` (numeric
#'     test statistic), `TSPVal` (numeric p-value). - For Continuous endpoint: `DoF` (numeric degrees of freedom),
#'     `SDPld` (numeric pooled standard deviation), `StdErr` (numeric standard error), `TS` (numeric test
#'     statistic), `TSPVal` (numeric p-value).}
#'   \item{LastLookDecision}{Vector of Integer. Vector of length equal to the number of endpoints, containing the
#'     decisions from previous look. Order matches `DesignParam$EndpointName` order. Possible values: - `0`:
#'     Continue. - `1`: Efficacy. - `2`: Futility.}
#'   \item{EfficacyBoundaryPScale}{Vector of Numeric. Vector of length equal to the number of endpoints, containing
#'     the final set of efficacy boundaries on the p-value scale used for testing each endpoint by engine. `NaN`
#'     where boundaries were not calculated. Order matches `DesignParam$EndpointName` order.}
#'   \item{EPStatus}{Vector of Integer. Vector of length equal to the number of endpoints, containing the status of
#'     each endpoint. Order matches `DesignParam$EndpointName` order. Possible values: - `0`: Success. - `1`:
#'     Insufficient information. - `2`: Excessive information. - `3`: Computational error.}
#' }
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$Alpha`, rather than by position. Availability depends on the endpoint, design, and East Horizon
#'   product as indicated below.
#' \describe{
#'   \item{EndpointName}{Vector of String. Vector of length equal to the number of endpoints, containing the
#'     endpoint names.}
#'   \item{EndpointType}{Vector of Integer. Vector of length equal to the number of endpoints, indicating the
#'     endpoint types. Order matches `EndpointName` order. Possible values: - `0`: Continuous. - `1`: Binary. -
#'     `2`: Time-to-Event.}
#'   \item{TargetInformation}{Vector of Integer. Vector of length equal to the number of endpoints, containing the
#'     target information of each endpoint: number of completers for Continuous or Binary, number of events for
#'     TTE. Order matches `EndpointName` order.}
#'   \item{EffFlg}{Matrix of Integer. Matrix of size equal to the number of analysis looks (rows) x the number of
#'     endpoints (columns), containing the efficacy flag indicating which endpoint is selected for efficacy testing
#'     at which analysis. Columns order matches `EndpointName` order. Possible values: - `1`: Test for efficacy. -
#'     `0`: Do not test.}
#'   \item{FutFlg}{Matrix of Integer. Matrix of size equal to the number of analysis looks (rows) x the number of
#'     endpoints (columns), containing the futility flag indicating which endpoint is selected for futility testing
#'     at which analysis. Columns order matches `EndpointName` order. Possible values: - `1`: Test for futility -
#'     `0`: Do not test.}
#'   \item{FutThrsld}{Matrix of Numeric. Matrix of size equal to the number of analysis looks (rows) x the number
#'     of endpoints (columns), containing the futility thresholds. Columns order matches `EndpointName` order. `0`
#'     when futility check is not performed (`FutFlg = 0`). Depends on the endpoint: - For TTE endpoint: thresholds
#'     represent HR (declare futility if HR > threshold). - For Binary or Continuous endpoint: thresholds represent
#'     Delta (declare futility if Delta < threshold).}
#' }
#' @param OutList Optional named list used to pass outputs between analysis looks. Return it at one look to receive
#'   the same list as input at the next look; the input is NULL at the first look. Access elements by name.
#'   Available for designs that support passing state between looks.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Integer vector of endpoint decisions in DesignParam$EndpointName order: 0 = continue; 1 =
#'     efficacy; 2 = futility. This is the required output.}
#'   \item{OutList}{Optional named list used to pass outputs between analysis looks. Return it at one look to
#'     receive the same list as input at the next look; the input is NULL at the first look. Access elements by
#'     name. Available for designs that support passing state between looks.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details This example retains prior endpoint decisions and tests efficacy against the current p-value boundary.
#'   It checks scheduled futility thresholds even at looks without an efficacy boundary. An undecided endpoint
#'   stops for futility at its target information, when information is insufficient, or after its last planned
#'   efficacy look.
######################################################################################################################## .

GetMEPDecision <- function( SimData, AnalysisData, DataSummary, LookInfo,
                            DesignParam, OutList = NULL, UserParam = NULL ) {
    # Initialize vDecision with last look's decisions
    vDecision <- LookInfo$LastLookDecision

    # Get number of endpoints
    nNumEP <- length( DesignParam$EndpointName )

    # For each endpoint
    for ( nEPID in seq_len( nNumEP ) ) {
        # Only process if last look decision was Continue (0)
        if ( vDecision[ nEPID ] == 0 ) {
            strEPName <- DesignParam$EndpointName[ nEPID ]
            # Get efficacy dBoundary and p-value for current endpoint
            dBoundary <- LookInfo$EfficacyBoundaryPScale[ nEPID ]
            dPValue <- LookInfo$TestStatisticsOutputs[[ strEPName ]]$data$TSPVal

            # Check if both dBoundary and p-value exist and p-value is less than dBoundary
            if ( !is.na( dBoundary ) && !is.na( dPValue ) ) {
                if ( dPValue < dBoundary ) {
                    vDecision[ nEPID ] <- 1
                    next
                }
            }

            # Check scheduled futility even when this look has no efficacy boundary.
            nCurrentLook <- LookInfo$LookNum
            if ( DesignParam$FutFlg[ nCurrentLook, nEPID ] == 1 ) {
                dFutilityThreshold <- DesignParam$FutThrsld[ nCurrentLook, nEPID ]
                nEPType <- DesignParam$EndpointType[ nEPID ]

                if ( nEPType == 2 ) {
                    dObservedValue <- DataSummary[[ strEPName ]]$HR
                    if ( !is.na( dObservedValue ) && dObservedValue > dFutilityThreshold ) {
                        vDecision[ nEPID ] <- 2
                    }
                } else {
                    dObservedValue <- DataSummary[[ strEPName ]]$Delta
                    if ( !is.na( dObservedValue ) && dObservedValue < dFutilityThreshold ) {
                        vDecision[ nEPID ] <- 2
                    }
                }
            }
        }

        # If still not stopped, check if we've reached or exceeded target information
        if ( vDecision[ nEPID ] == 0 ) {
            strEPName <- DesignParam$EndpointName[ nEPID ]
            nEPType <- DesignParam$EndpointType[ nEPID ]
            nTargetInformation <- DesignParam$TargetInformation[ nEPID ]

            # Get current events/completers count based on endpoint type
            nCurrentCount <- if ( nEPType == 2 ) {
                DataSummary[[ strEPName ]]$Events
            } else {
                DataSummary[[ strEPName ]]$Completers
            }

            if ( !is.na( nCurrentCount ) && nCurrentCount >= nTargetInformation ) {
                vDecision[ nEPID ] <- 2
            }
            if ( LookInfo$EPStatus[ nEPID ] == 1 ) {
                vDecision[ nEPID ] <- 2
            }

            nCurrentLook <- LookInfo$LookNum
            bFutureEfficacyLooks <- if ( nCurrentLook < nrow( DesignParam$EffFlg ) ) {
                any( DesignParam$EffFlg[ ( nCurrentLook + 1 ):nrow( DesignParam$EffFlg ), nEPID ] == 1 )
            } else {
                FALSE
            }

            if ( !bFutureEfficacyLooks && DesignParam$EffFlg[ nCurrentLook, nEPID ] == 1 ) {
                vDecision[ nEPID ] <- 2
            }
        }
    }

    lRet <- list(
        Decision = as.integer( vDecision ),
        ErrorCode = as.integer( 0 )
    )

    return( lRet )
}
