######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Template: Make multiple-endpoint trial decisions
#' @description Make multiple-endpoint trial decisions. Use this template as a starting point for custom logic.
#'   Preserve the engine-supplied argument names and access named list elements by name. Supply additional
#'   user-defined inputs through UserParam where that argument is supported.
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$ArrivalTime`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{SimID}{Vector of length equal to the number of subjects, indicating the simulation ID.}
#'   \item{PatId}{Vector of length equal to the number of subjects, containing the patient ID identifying a unique
#'     patient in a given simulation.}
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{TResponse.`EPNAME`}{Vector of length equal to the number of subjects, containing the generated responses
#'     for a specific endpoint. `EPNAME` is the endpoint name specified in East Horizon. Endpoint names can also be
#'     accessed using `DesignParam$EndpointName`.}
#'   \item{CensorID.`EPNAME`}{Vector of length equal to the number of subjects, containing the generated censor
#'     indicator values for a specific endpoint: – `0`: Dropout. – `1`: Completer. `EPNAME` is the endpoint name
#'     specified in East Horizon. Endpoint names can also be accessed using `DesignParam$EndpointName`.}
#'   \item{Response.`EPNAME`}{Vector of length equal to the number of subjects, containing the generated responses
#'     for a specific endpoint after adjusting for endpoint and dropout rules. `EPNAME` is the endpoint name
#'     specified in East Horizon. Endpoint names can also be accessed using `DesignParam$EndpointName`.}
#'   \item{CalRespT.`EPNAME`}{Vector of length equal to the number of subjects, containing the generated response
#'     times on calendar scale for a specific endpoint. Equivalent to `Response + ArrivalTime`. `EPNAME` is the
#'     endpoint name specified in East Horizon. Endpoint names can also be accessed using
#'     `DesignParam$EndpointName`.}
#'   \item{DropoutID.`EPNAME`}{Vector of length equal to the number of subjects, containing whether each patient
#'     dropped out before responding for a specific endpoint: – `0`: Dropout. – `1`: Completer. `EPNAME` is the
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
#'   \item{AnalysisTime}{Numeric. Current analysis time.}
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
#'   \item{InfoFrac}{List of Numeric Vector. List of length equal to the number of endpoints, containing the
#'     vectors of the actual information fractions up to the current look for one endpoint. Order matches
#'     `DesignParam$EndpointName` order.}
#'   \item{LastLookDecision}{Vector of Integer. Vector of length equal to the number of endpoints, containing the
#'     decisions from previous look. Order matches `DesignParam$EndpointName` order. Possible values: – `0`:
#'     Continue. – `1`: Efficacy. – `2`: Futility.}
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
#'   \item{TotalLooks}{Integer. Total number of planned looks.}
#'   \item{EndpointName}{Vector of String. Vector of length equal to the number of endpoints, containing the
#'     endpoint names.}
#'   \item{EndpointType}{Vector of Integer. Vector of length equal to the number of endpoints, indicating the
#'     endpoint types. Order matches `EndpointName` order. Possible values: – `0`: Continuous. – `1`: Binary. –
#'     `2`: Time-to-Event.}
#'   \item{TailType}{Vector of Integer. Vector of length equal to the number of endpoints, indicating the nature of
#'     critical region for each endpoint. Order matches `EndpointName` order. Possible values: – `0`: Left-tailed.
#'     – `1`: Right-tailed.}
#'   \item{NumPat}{Integer number of subjects in the trial.}
#'   \item{AllocRatio}{Numeric. Treatment allocation ratio: number of patients in the treatment arm over number of
#'     patient in the control arm.}
#'   \item{TrialType}{Vector of Integer. Vector of length equal to the number of endpoints, indicating the trial
#'     type for each endpoint. Order matches `EndpointName` order. Possible values: – `0`: Superiority. – `1`:
#'     Non-inferiority.}
#'   \item{Alpha}{Numeric type I error rate (significance level).}
#'   \item{TestStatistics}{Named List. Named List of length equal to the number of endpoints, containing the test
#'     statistics specifications. Names are user-specified endpoint names from `EndpointName`. Order also matches
#'     `EndpointName` order. Each element contains: - `Test`: Integer test type. Possible values: `0` = None, `1` =
#'     Log-Rank (TTE), `2` = Difference of Means (Continuous), `3` = Difference of Proportions (Binary), `4` =
#'     Ratio of Proportions (Binary). - `TestStat`: Integer test statistic. Possible values: `0` = None, `1` =
#'     Log-Rank (TTE), `3` = Harrington Fleming (TTE), `4` = t-statistic (Continuous), `5` = z-statistic (Binary),
#'     `6` = Modestly Weighted Log-Rank (TTE). - `Variance`: Integer variance. Only available for Binary or
#'     Continuous endpoint. Possible values: `1` = Pooled (Binary), `2` = Unpooled (Binary), `3` = Equal
#'     (Continuous), `4` = Unequal (Continuous). - `Parameter`: Vector of Numeric. Only available for TTE with
#'     Harrington Fleming or MWLR. For Harrington Fleming: `c(p,q)`. For MWLR: `c(delay,w_max)`.}
#'   \item{TargetInformation}{Vector of Integer. Vector of length equal to the number of endpoints, containing the
#'     target information of each endpoint: number of completers for Continuous or Binary, number of events for
#'     TTE. Order matches `EndpointName` order.}
#'   \item{MultiplicityDetails}{List. Multiplicity adjustment details. Contains: - `MCP`: Integer, multiple
#'     comparison procedure. Possible values: `0` = None, `1` = Fallback, `2` = Fixed Sequence, `3` =
#'     Bonferroni/Weighted Bonferroni, `4` = Holm, `5` = Weighted Holm, `6` = User-specified GMCP. -
#'     `AlphaAlloc`: Vector of Numeric of length equal to the number of endpoints, containing the Alpha allocation
#'     percentages for each endpoint. Order matches `EndpointName` order. - `TestOrder`: Vector of Integer of
#'     length equal to the number of endpoints, containing the testing order for each endpoint. Available for
#'     Fallback or Fixed Sequence only. Order matches `EndpointName` order. - `TransMax`: Matrix of Numeric of size
#'     equal to the number of endpoints x the number of endpoints, containing the transitions for Alpha
#'     propagation. Available for GMCP only. Order matches `EndpointName` order.}
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
#'   \item{EffSpending}{Named List. Named List of length equal to the number of endpoints, containing the Alpha
#'     spending details. Names are user-specified endpoint names from `EndpointName`. Order also matches
#'     `EndpointName` order. Each element contains: - `EffBdry`: Integer efficacy boundary type. Possible values:
#'     `0` = None, `1` = Spending Function. - `SpendFunc`: Integer spending function type. Only available for
#'     `EffBdry = 1`. Possible values: `1` = Lan-DeMets (LD), `2` = Gamma. - `Parameter`: Numeric or integer value.
#'     Only available for `EffBdry = 1`. Possible values: - For LD: `1` = O'Brien-Fleming (OF), `2` = Pocock (PK).
#'     For Gamma: numeric values (e.g., 1, -3).}
#'   \item{WinCondCriteria}{List. List containing trial winning condition criteria. Contains: -`whichEPs`: Vector
#'     of Integer of length equal to the number of endpoints, indicating which endpoints are considered for winning
#'     condition. Order matches `EndpointName` order. Possible values: `0` = Not considered, `1` = Considered.
#'     -`NumEPsWin`: Integer. Number of endpoints that must be won. Can be `0` if `MustWinEPs` is used. -
#'     `MustWinEPs`: Vector of Integer of length equal to the number of endpoints, indicating which endpoints must
#'     be won for trial success. Order matches `EndpointName` order. Possible values: `0` = Not required, `1` =
#'     Must win.}
#' }
#' @param OutList Optional named list used to pass outputs between analysis looks. Return it at one look to receive
#'   the same list as input at the next look; the input is NULL at the first look. Access elements by name.
#'   Available for designs that support passing state between looks.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{Decision}{Integer vector of endpoint decisions in DesignParam$EndpointName order: 0 = continue; 1 =
#'     efficacy; 2 = futility. This is the required output.}
#'   \item{TestStat}{Optional named list of endpoint test statistics, using the names and order in
#'     DesignParam$EndpointName.}
#'   \item{Response}{Named list of numeric response vectors, one vector per endpoint with one element per subject,
#'     using the names and order in DesignParam$EndpointName.}
#'   \item{RawPVal}{Optional numeric vector of raw endpoint p-values.}
#'   \item{EfficacyBoundary}{Optional numeric vector of endpoint efficacy boundaries.}
#'   \item{WinStatus}{Optional integer trial status: 0 = no decision; 1 = win; -1 = lose.}
#'   \item{Score}{Optional numeric vector of score statistics.}
#'   \item{StdErr}{Optional numeric vector of standard errors.}
#'   \item{PropPld}{Optional numeric vector of pooled proportions.}
#'   \item{SDPld}{Optional numeric vector of pooled standard deviations.}
#'   \item{OutList}{Optional named list used to pass outputs between analysis looks. Return it at one look to
#'     receive the same list as input at the next look; the input is NULL at the first look. Access elements by
#'     name. Available for designs that support passing state between looks.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( SimData, AnalysisData, DataSummary, LookInfo,
                               DesignParam, OutList = NULL, UserParam = NULL ) {
    # Write the decision generation logic here
    vDecision <- rep( 1L, length( DesignParam$EndpointName ) ) # One placeholder decision per endpoint.

    lRet <- list( Decision = vDecision )

    return( lRet )
}
