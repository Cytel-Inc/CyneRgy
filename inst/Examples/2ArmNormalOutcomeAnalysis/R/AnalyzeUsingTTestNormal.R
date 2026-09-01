######################################################################################################################## .
#' @name AnalyzeUsingTTestNormal
#' @title Analyze a Normal Outcome with a Two-Sample T-Test
#' @author Shubham Lahoti, J. Kyle Wathen, and Gabriel Potvin
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#'        \describe{
#'          \item{ArrivalTime}{ A numeric value with the time the patient arrived in the trial}
#'          \item{TreatmentID}{An integer value where 0 indicates control treatment and 1 experimental treatment.}
#'          \item{Response}{A numeric value indicating the response.}
#'          \item{CensorIndOrg}{An integer value indicating whether the subject was censored or not.}
#'        }
#' @param DesignParam List of design and simulation parameters needed to compute test statistics and perform testing. Access elements by name, for example `DesignParam$Alpha`, rather than by position.
#'      \describe{
#'          \item{SampleSize}{Sample size of the trial}
#'          \item{Alpha}{Type I Error}
#'          \item{TestType}{Values are One side: 0; Two Sided: 1, Two Sided, Asymmetric: 2}
#'          \item{TailType}{Values are Left Tailed: 0, Right Tailed: 1}
#'          \item{LowerAlpha}{Lower Type I error. Present for Left Tailed and Two Sided Asymmetric Tests }
#'          \item{UpperAlpha}{Upper Type I error. Present for Right Tailed and Two Sided Asymmetric Tests }
#'          \item{MaxCompleters}{Maximum Number of Completers}
#'          \item{FollowUpType}{Follow-up type: 0 for until the end of the study, or 1 for a fixed period.}
#'          \item{AllocInfo}{Vector of ratios of treatment sample sizes to control sample size. Length = Number of treatment arms. }
#'          \item{CriticalPoint}{Critical Value. Present in Fixed Sample designs only }
#'          \item{UpperCriticalPoint}{Upper Critical Value. Present in Right Tail Fixed Sample designs only }
#'          \item{LowerCriticalPoint}{Lower Critical Value. Present in Left Tail Fixed Sample designs only }
#'          \item{FollowUpDur}{Follow up duration}
#'          \item{TrtEffNull}{Treatment Effect under Null on natural scale. Applicable for Non-inferiority trials.}
#'
#'      }
#' @param LookInfo List of parameters for the current analysis look. It is `NULL` for fixed-sample designs. Access elements by name, for example `LookInfo$NumLooks`, rather than by position.
#'                 \describe{
#'                      \item{NumLooks}{An integer value with the number of looks in the study}
#'                      \item{CurrLookIndex}{An integer value with the current index look, starting from 1}
#'                      \item{CumCompleters}{Cumulative number of completer for all non time-to-event studies.}
#'                      \item{InfoFrac}{Information fraction}
#'                      \item{RejType}{Rejection type identifying the enabled efficacy and futility boundaries.}
#'                      \item{CumAlpha}{Cumulative alpha spent. Present in one sided tests only }
#'                      \item{CumAlphaUpper}{Upper cum. alpha spent. Present in right tailed and two sided tests only }
#'                      \item{CumAlphaLower}{Lower cum. alpha spent. Present in left tailed and two sided tests only }
#'                      \item{EffBdryScale}{Efficacy boundary scale.  Possible values are: Z Scale: 0, p-Value Scale: 1}
#'                      \item{EffBdry}{Vector of efficacy boundaries. Present in one sided tests only }
#'                      \item{EffBdryUpper}{Vector of upper efficacy boundaries. Present in right tailed and two sided tests only }
#'                      \item{EffBdryLower}{Vector of lower efficacy boundary. Present in left tailed and two sided tests only }
#'                      \item{FutBdryScale}{Futility boundary scale. Possible value are:  Z Scale: 0, p-Value Scale: 1, Delta Scale: 2, Conditional Power Scale: 3}
#'                      \item{FutBdry}{Vector of futility boundaries. Present in one sided tests only }
#'                      \item{FutBdryUpper}{Vector of upper futility boundaries. Present in left tailed and two sided tests only }
#'                      \item{FutBdryLower}{Vector of lower futility boundaries. Present in right tailed and two sided tests only }
#'                      \item{CPDeltaOption}{Conditional-power treatment-effect option: 0 for design Delta or 1 for estimated Delta.}
#'                      \item{BindingType}{Futility binding type: 0 for non-binding or 1 for binding.}
#'                 }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' @description Use the t.test() function in the base package to compute the statistic. The purpose of this example is to demonstrate how the analysis and decision making can be modified in a simple approach.
#'              The test statistic is compared to the upper boundary computed and sent by East Horizon as an input. This example does NOT include a futility rule.
#' @return A named list containing `TestStat`, `Decision`, and `ErrorCode`.
#' @details
#' ## CyneRgy Decision Helpers
#'
#' The analysis may use `CyneRgy::GetDecisionString()` and
#' `CyneRgy::GetDecision()` to determine the decision returned to
#' East Horizon Explore.
#'
#' When these helpers are used, the following input fields are required
#' and MUST be included when generating sample/test data:
#'
#' DesignParam:
#'   - TailType: Integer indicating the direction of the statistical test.
#'       0 = Left-tailed
#'       1 = Right-tailed
#'
#' LookInfo (for group sequential designs, NULL for fixed designs):
#' When not NULL, must contain the following fields:
#'   - NumLooks: Total number of looks.
#'   - CurrLookIndex: Current look index, starting at 1.
#'   - RejType: Integer identifying which stopping boundaries are enabled.
#'       0 = 1-Sided Efficacy Upper
#'       1 = 1-Sided Futility Upper
#'       2 = 1-Sided Efficacy Lower
#'       3 = 1-Sided Futility Lower
#'       4 = 1-Sided Efficacy Upper and Futility Lower
#'       5 = 1-Sided Efficacy Lower and Futility Upper
#'       6 = 2-Sided Efficacy Only (not used in East Horizon Explore)
#'       7 = 2-Sided Futility Only (not used in East Horizon Explore)
#'       8 = 2-Sided Efficacy and Futility (not used in East Horizon Explore)
#'       9 = Equivalence (not used in East Horizon Explore)
#'
######################################################################################################################## .

AnalyzeUsingTTestNormal <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Step 1: Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
    if( !is.null( LookInfo ) )
    {
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfLooks          <- LookInfo$NumLooks
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        nRejType             <- LookInfo$RejType
        nTailType            <- DesignParam$TailType
    }
    else
    {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsInAnalysis <- nrow( SimData )
        nTailType            <- DesignParam$TailType
    }
    # Create the vector of simulated data for this IA - East Horizon sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]

    # Create vectors of data for each treatment - E is Experimental and S is Standard of Care
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]

    # compute the estimates for mean and Std. Dev for (E and S)
    dMeanOfResponsesOnE   <- mean( vOutcomesE )
    dStdDevOfResponsesOnE <- sd( vOutcomesE )
    nQtyOfPatsOnE         <- length( vOutcomesE )

    dMeanOfResponsesOnS   <- mean( vOutcomesS )
    dStdDevOfResponsesOnS <- sd( vOutcomesS )
    nQtyOfPatsOnS         <- length( vOutcomesS )

    # delta = mean(E) - mean(S). Change the alternative if delta < 0, put alternative = "less"
    # var.equal = TRUE assumes the variances of both arms to be same, so the intermediate computations
    # uses Pooled std deviation estimate. This will be consistent with Example - 1.
    lAnalysisResult       <- t.test( vOutcomesE, vOutcomesS, alternative = "greater",
                                     var.equal = TRUE )

    dTValue              <- lAnalysisResult$statistic    # extract t-test statistic value
    dBoundary            <- ifelse( is.null( LookInfo ), DesignParam$CriticalPoint, LookInfo$EffBdryUpper[ nLookIndex ] )

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                               bIAEfficacyCondition = dTValue > dBoundary,
                                               bFAEfficacyCondition = dTValue > dBoundary )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    nError <- 0

    lRet <- list( TestStat = as.double( dTValue ),
                  Decision  = as.integer( nDecision ),
                  ErrorCode = as.integer( nError ) )
    return( lRet )
}
