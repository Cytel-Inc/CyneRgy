######################################################################################################################## .
#' @name AnalyzeUsingEastManualFormula
#' @title Compute the statistic using formula 28.2 in the East manual.
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
#' @description Use the formula 28.2 in the East manual to compute the statistic.  The purpose of this example is to demonstrate how the analysis and decision making can be modified in a simple approach.
#'              The test statistic is compared to the upper boundary computed and sent by East Horizon as an input. This example does NOT include a futility rule.
#' @return After the blanks are completed, a named list containing `TestStat`, `ErrorCode`, and `Decision`.
######################################################################################################################## .

AnalyzeUsingEastManualFormula <- function( SimData, DesignParam, LookInfo, UserParam = NULL )
{
    # In this example, the majority of the code is provided.  The fill in the blank areas are noted by _____________________.
    # This is done to allow you to practice creating these examples. You will need to remove the ____________ and enter the correct code.
    # The fully worked examples are provided in the corresponding example R files.

    # Retrieve necessary information from the objects East Horizon sent
    nLookIndex           <- LookInfo$CurrLookIndex
    nQtyOfEvents         <- LookInfo$CumEvents[ nLookIndex ]
    nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]

    # Create the vector of simulated data for this IA - East Horizon sends all of the simulated data
    vPatientOutcome      <- SimData$Response[ 1:nQtyOfPatsInAnalysis ]
    vPatientTreatment    <- SimData$TreatmentID[ 1:nQtyOfPatsInAnalysis ]

    # Create vectors of data for each treatment - E is Experimental and S is Standard of Care
    vOutcomesS           <- vPatientOutcome[ vPatientTreatment == 0 ]
    vOutcomesE           <- ______[ vPatientTreatment == 1 ]

    nQtyOfResponsesOnE   <- sum( vOutcomesE )
    nQtyOfPatsOnE        <- length( vOutcomesE )

    nQtyOfResponsesOnS   <- sum( ______ )
    nQtyOfPatsOnS        <- length( vOutcomesS )

    # Compute the estimates in equation 28.2 from the East user manual
    dPiHatExperimental   <- nQtyOfResponsesOnE / nQtyOfPatsOnE
    dPiHatControl        <- _______ / nQtyOfPatsOnS

    dPiHatj              <- ( nQtyOfResponsesOnE + nQtyOfResponsesOnS ) / ( nQtyOfPatsOnE + nQtyOfPatsOnS )

    # Equation 28.2 in East manual
    dZj                  <- ( dPiHatExperimental - dPiHatControl ) / sqrt( dPiHatj * ( 1 - dPiHatj ) * ( 1 / nQtyOfPatsOnE + 1 / nQtyOfPatsOnS ) )

    # A decision of 2 means success, 0 means continue the trial
    nDecision            <- ifelse( dZj > LookInfo$EffBdryUpper[ nLookIndex ], 2, 0 )

    if( nDecision == 0 )
    {
        # For this example, there is NO futility check but this is left for consistency with other examples

    }

    nError <- 0

    return( list( TestStat = as.double( dZj ), ErrorCode = as.integer( nError ), ________ = as.integer( nDecision ) ) )
}
