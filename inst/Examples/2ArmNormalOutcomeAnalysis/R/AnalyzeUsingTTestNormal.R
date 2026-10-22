######################################################################################################################## .
#' @name AnalyzeUsingTTestNormal
#' @title Analyze continuous subject responses
#' @description Use the t.test() function in the stats package to compute the statistic. The purpose of this example
#'   is to demonstrate how the analysis and decision making can be modified in a simple approach. The test
#'   statistic is compared to the upper boundary computed and sent by East Horizon as an input. This example does NOT
#'   include a futility rule.
#' @author Shubham Lahoti, J. Kyle Wathen, and Gabriel Potvin
#' @param SimData Data frame of subject-level data for the current simulation, with one row per subject. Access
#'   columns by name, for example `SimData$TreatmentID`. Columns include the fields below when applicable,
#'   plus any custom outputs from enrollment, randomization, response, or dropout generation.
#' \describe{
#'   \item{TreatmentID}{Integer vector of treatment assignments, with one element per subject: 0 = placebo/control,
#'     1 = first experimental arm, 2 = second experimental arm, and so on.}
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#' }
#' @param DesignParam Named list of design and simulation parameters. Access elements by name, for example
#'   `DesignParam$TailType`, rather than by position. Availability depends on the endpoint, design, and East Horizon product
#'   as indicated below. The fields listed here are used by this example.
#' \describe{
#'   \item{TailType}{Integer. Nature of critical region: - `0`: Left-tailed. - `1`: Right-tailed. East Horizon
#'     Design: Only available if `Test Type = One-sided`.}
#'   \item{CriticalPoint}{Numeric. Critical value (for one-sided tests). East Horizon Explore: Only available if
#'     `Statistical Design = Fixed Sample`. East Horizon Design: Only available if `Test Type = One-sided` and
#'     `Statistical Design = Fixed Sample`.}
#' }
#' @param LookInfo Named list of group sequential analysis parameters, or NULL for a fixed-sample design. Access
#'   elements by name, for example `LookInfo$CurrLookIndex`, rather than by position. Pass LookInfo explicitly to
#'   `CyneRgy::GetDecisionString()` and `CyneRgy::GetDecision()`, including NULL for a fixed-sample design.
#' \describe{
#'   \item{NumLooks}{Integer total number of analysis looks.}
#'   \item{CurrLookIndex}{Integer index of the current analysis look, starting at 1.}
#'   \item{CumCompleters}{Integer vector of cumulative completer counts, one per look.}
#'   \item{RejType}{Integer one-sided rejection type supported by the decision helpers: 0 = upper efficacy;
#'     1 = upper futility; 2 = lower efficacy; 3 = lower futility; 4 = upper efficacy and lower futility;
#'     5 = lower efficacy and upper futility.}
#'   \item{EffBdryUpper}{Vector of Numeric. Vector of length `LookInfo$NumLooks`, containing the upper efficacy
#'     boundary values (for two-sided tests) for each look. East Horizon Explore: Only available if `Tail Type =
#'     Right-tailed`. Two-sided tests do not exist, so this variable is not useful: use EffBdry instead. East
#'     Horizon Design: Only available if `Test Type = One-sided` and `Tail Type = Right-tailed`, or `Test Type =
#'     Two-sided asymmetric or symmetric`.}
#' }
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore).}
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
######################################################################################################################## .

AnalyzeUsingTTestNormal <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {

    # Step 1: Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
    if ( !is.null( LookInfo ) ) {
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfLooks <- LookInfo$NumLooks
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        nRejType <- LookInfo$RejType
        nTailType <- DesignParam$TailType
    } else {
        nLookIndex <- 1
        nQtyOfLooks <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
        nTailType <- DesignParam$TailType
    }
    # Create the vector of simulated data for this IA - East Horizon sends all of the simulated data
    vPatientOutcome <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]

    # Create vectors of data for each treatment - E is Experimental and S is Standard of Care
    vOutcomesS <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE <- vPatientOutcome[ vPatientTreatment == 1 ]

    # compute the estimates for mean and Std. Dev for (E and S)
    dMeanOfResponsesOnE <- mean( vOutcomesE )
    dStdDevOfResponsesOnE <- stats::sd( vOutcomesE )
    nQtyOfPatsOnE <- length( vOutcomesE )

    dMeanOfResponsesOnS <- mean( vOutcomesS )
    dStdDevOfResponsesOnS <- stats::sd( vOutcomesS )
    nQtyOfPatsOnS <- length( vOutcomesS )

    # delta = mean(E) - mean(S). Change the alternative if delta < 0, put alternative = "less"
    # var.equal = TRUE assumes the variances of both arms to be same, so the intermediate computations
    # uses Pooled std deviation estimate. This will be consistent with Example - 1.
    lAnalysisResult <- stats::t.test( vOutcomesE, vOutcomesS,
        alternative = "greater",
        var.equal = TRUE
    )

    dTValue <- lAnalysisResult$statistic # extract t-test statistic value
    dBoundary <- ifelse( is.null( LookInfo ), DesignParam$CriticalPoint, LookInfo$EffBdryUpper[ nLookIndex ] )

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
        bIAEfficacyCondition = dTValue > dBoundary,
        bFAEfficacyCondition = dTValue > dBoundary
    )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    nErrorCode <- 0

    lRet <- list(
        TestStat = as.double( dTValue ),
        Decision = as.integer( nDecision ),
        ErrorCode = as.integer( nErrorCode )
    )
    return( lRet )
}
