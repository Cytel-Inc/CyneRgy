######################################################################################################################## .
#' @name AnalyzeUsingMeanLimitsOfCI
#' @title Analyze using a simplified limits of confidence interval design
#' @description In this simplified example of upper and lower confidence boundary designs, if it is likely that the
#'   treatment difference is above the Minimum Acceptable Value (MAV) then a Go decision is made. If a Go decision
#'   is not made, then if it is unlikely that the treatment difference is above the Target Value (TV) a No-Go
#'   decision is made. In this example, the t.test() from the stats package in R is utilized to analyze the data and
#'   compute a user-specified confidence interval using UserParam$dConfLevel. The team would like to make a Go
#'   decision if there is at least a 90\% chance that the difference in treatment is greater than the MAV. If a Go
#'   decision is not made, then a No-Go decision is made if there is less than a 10\% chance the difference is
#'   greater than the TV. Using a frequentist CI an approximation to this design can be done by the logic described
#'   below. At an analysis, if the Lower Limit of the CI, denoted by LL, is greater than user-specified
#'   UserParam$dMAV then a Go decision is made.
#'
#' If a Go decision is not made, then if the Upper Limit of the CI, denoted by UL, is less than user-specified
#'   UserParam$dTV a No-Go decision is made. Specifically, if LL > UserParam$dMAV --> Go if UL < UserParam$dTV -->
#'   No-Go Otherwise, continue to the next analysis. At the Final Analysis: If LL > UserParam$dMAV then a Go
#'   decision is made, otherwise, a No-Go decision is made
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
#' }
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' UserParam must be supplied, the list must contain the following named elements:
#' \describe{
#'   \item{UserParam$dMAV}{A value (-Inf, Inf) that specifies the Minimum Acceptable Value (MAV).}
#'   \item{UserParam$dTV}{A value (-Inf, Inf) that specifies the Target Value (TV).}
#'   \item{UserParam$dConfLevel}{Numeric confidence level in (0, 1) used to construct the confidence
#'     interval.}
#' }
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{Decision}{Integer boundary-crossing code: 0 = no boundary crossed; 1 = lower efficacy boundary crossed;
#'     2 = upper efficacy boundary crossed; 3 = futility boundary crossed; 4 = equivalence boundary crossed
#'     (unavailable in East Horizon Explore).}
#'   \item{TestStat}{Numeric test statistic on the Wald (Z) scale.}
#'   \item{Delta}{Estimated experimental-minus-control treatment effect (proportion difference for binary outcomes;
#'     mean difference for continuous outcomes).}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details This example returns the lower confidence limit in TestStat for reporting and supplies Decision to
#'   apply its custom stopping rule. The reported confidence limit is not a Wald Z statistic.
#'   UserParam$dConfLevel specifies the one-sided confidence level for each bound: the lower bound uses
#'   alternative = 'greater' and the upper bound uses alternative = 'less'.
#'
#'
#' This function is only applicable to the case where MAV <= TV.
######################################################################################################################## .

AnalyzeUsingMeanLimitsOfCI <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL ) {

    # Step 1: Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
    if ( !is.null( LookInfo ) ) {
        # Group sequential design
        nLookIndex <- LookInfo$CurrLookIndex
        nQtyOfLooks <- LookInfo$NumLooks
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        nRejType <- LookInfo$RejType
        nTailType <- DesignParam$TailType
    } else {
        # Fixed Design
        nLookIndex <- 1
        nQtyOfLooks <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
        nTailType <- DesignParam$TailType
    }

    if ( is.null( UserParam ) ) {
        # FATAL ERROR AS WE DON'T KNOW WHAT THE USER WANTS TO DO.
        # Creating a FATAL error will avoid misleading results when UserParam is not supplied
        return( list(
            TestStat = as.double( 0 ),
            ErrorCode = as.integer( -1 ),
            Decision = as.integer( 0 ),
            Delta = as.double( 0 )
        ) )
    }

    # Create the vector of simulated data for this IA - East Horizon sends all of the simulated data ####
    vPatientOutcome <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]

    # Create vectors of data for each treatment  ####
    vOutcomesS <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE <- vPatientOutcome[ vPatientTreatment == 1 ]

    # Perform the desired analysis, then determine if the lower limit of the confidence interval is greater than the user-specified value ####
    # Compute one-sided confidence bounds at the same level for efficacy and futility.
    # var.equal = TRUE assumes the variances of both arms to be same, so the intermediate computations uses Pooled std deviation estimate. This will be consistent with Example - 1 & 2.

    lAnalysisResult <- stats::t.test( vOutcomesE, vOutcomesS,
        alternative = "greater",
        var.equal = TRUE, conf.level = UserParam$dConfLevel
    )
    lUpperLimitResult <- stats::t.test( vOutcomesE, vOutcomesS,
        alternative = "less",
        var.equal = TRUE, conf.level = UserParam$dConfLevel
    )
    dLowerLimitCI <- lAnalysisResult$conf.int[ 1 ]
    dUpperLimitCI <- lUpperLimitResult$conf.int[ 2 ]

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
        bIAEfficacyCondition = dLowerLimitCI > UserParam$dMAV,
        bIAFutilityCondition = dUpperLimitCI < UserParam$dTV,
        bFAEfficacyCondition = dLowerLimitCI > UserParam$dMAV
    )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    nErrorCode <- 0

    return( list(
        TestStat = as.double( dLowerLimitCI ),
        ErrorCode = as.integer( nErrorCode ),
        Decision = as.integer( nDecision ),
        Delta = as.double( lAnalysisResult$estimate[ 1 ] - lAnalysisResult$estimate[ 2 ] )
    ) )
}
