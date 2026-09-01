######################################################################################################################## .
#' @name AnalyzeUsingPropLimitsOfCI
#' @title Analyze using a simplified limits of confidence interval design
#' @author J. Kyle Wathen and Gabriel Potvin
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#'        \describe{
#'          \item{ArrivalTime}{A numeric value with the time the patient arrived in the trial}
#'          \item{TreatmentID}{An integer value where 0 indicates control treatment and 1 experimental treatment.}
#'          \item{Response}{An integer value where 1 indicates response and 0 indicates no response.}
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
#'          \item{RespLag}{Follow up duration}
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
#'                      \item{FutBdryScale}{Futility boundary scale. Possible value are: Z Scale: 0, p-Value Scale: 1, Delta Scale: 2, Conditional Power Scale: 3}
#'                      \item{FutBdry}{Vector of futility boundaries. Present in one sided tests only }
#'                      \item{FutBdryUpper}{Vector of upper futility boundaries. Present in left tailed and two sided tests only }
#'                      \item{FutBdryLower}{Vector of lower futility boundaries. Present in right tailed and two sided tests only }
#'                      \item{CPDeltaOption}{Conditional-power treatment-effect option: 0 for design Delta or 1 for estimated Delta.}
#'                      \item{BindingType}{Futility binding type: 0 for non-binding or 1 for binding.}
#'                 }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' \describe{
#'   \item{UserParam$dLowerLimit}{A value (0,1) that specifies the lower limit, eg  Minimum Acceptable Value (MAV).}
#'   \item{UserParam$dUpperLimit}{A value (0,1) that specifies the upper limit for the confidence interval, eg Target Value (TV).}
#'   \item{UserParam$dConfLevel}{A value (0,1) that specifies the confidence level for the prop.test function in base R.}
#' }
#' @description In this simplified example of upper and lower confidence boundary designs, if it is likely that the treatment difference is above the Minimum Acceptable Value (MAV) then a Go decision is made.
#'               If a Go decision is not made, then if is is unlikely that the treatment difference is above the Target Value (TV) a No Go decision is made.
#'               In this example, the prop.test from base R is utilized to analyze the data and compute at user-specified confidence interval (dConfLevel).
#'               The team would like to make a Go decision if there is at least a 90\% chance that the difference in treatment is greater than the MAV.
#'               If a Go decision is not made, then a No Go decision is made if there is less than a 10\% chance the difference is greater than the TV.
#'               Using a frequentist CI an approximation to this design can be done by the logic described below.
#'               At an analysis, if the Lower Limit of the CI, denoted by LL, is greater than user-specified dLowerLimit then a Go decision is made.
#'
#'               If a Go decision is not made, then if the Upper Limit of the CI, denoted by UL, is less than user-specified dUpperLimit a No Go decision is made.
#'               Specifically,
#'                  if LL > UserParam$dLowerLimit --> Go
#'                  if UL < UserParam$dUpperLimit --> No Go
#'               Otherwise, continue to the next analysis.
#'               At the Final Analysis: If LL > UserParam$dLowerLimit  then a Go decision is made, otherwise, a No Go decision is made
#' @return A named list containing `TestStat`, `ErrorCode`, `Decision`, and `Delta`.
#'@note In this example, the boundary information that is computed and sent from East Horizon is ignored in order to implement this decision approach.
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

AnalyzeUsingPropLimitsOfCI<- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Step 1: Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
    if( !is.null( LookInfo ) )
    {
        # Group sequential design
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfLooks          <- LookInfo$NumLooks
        nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
        nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
        nRejType             <- LookInfo$RejType
        nTailType            <- DesignParam$TailType
    }
    else
    {
        # Fixed Design
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfEvents         <- DesignParam$MaxCompleters
        nQtyOfPatsInAnalysis <- nrow( SimData )
        nTailType            <- DesignParam$TailType
    }

    if( is.null( UserParam ) )
    {

        # FATAL ERROR AS WE DON'T KNOW WHAT THE USER WANTS TO DO.
        # Creating a FATAL error will avoid misleading results when UserParam is not supplied
        return( list( TestStat  = as.double( 0 ),
                    ErrorCode = as.integer( -1 ),
                    Decision  = as.integer( 0 ),
                    Delta     = as.double( 0 ) ) )
    }

    # Create the vector of simulated data for this IA - East Horizon sends all of the simulated data ####
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]

    # Create vectors of data for each treatment  ####
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- vPatientOutcome[ vPatientTreatment == 1 ]

    # Perform the desired analysis, then determine if the lower limit of the confidence interval is greater than the user-specified value ####
    mData                <- cbind( table( vOutcomesS ), table( vOutcomesE ) )
    lAnalysisResult      <- prop.test( mData, alternative = "two.sided", correct = FALSE, conf.level = UserParam$dConfLevel )
    dLowerLimitCI        <- lAnalysisResult$conf.int[ 1 ]
    dUpperLimitCI        <- lAnalysisResult$conf.int[ 2 ]

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                               bIAEfficacyCondition = dLowerLimitCI > UserParam$dLowerLimit,
                                               bIAFutilityCondition = dUpperLimitCI < UserParam$dUpperLimit,
                                               bFAEfficacyCondition = dLowerLimitCI > UserParam$dLowerLimit )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    nError     <- 0

    return( list( TestStat  = as.double( dLowerLimitCI ),
                ErrorCode = as.integer( nError ),
                Decision  = as.integer( nDecision ),
                Delta     = as.double( lAnalysisResult$estimate[ 1 ] - lAnalysisResult$estimate[ 2 ] ) ) )
}
