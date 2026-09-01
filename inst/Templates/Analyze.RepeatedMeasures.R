######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Analyze Repeated-Measures Outcomes
#' @description Analyze simulated repeated-measures outcomes at the current interim or final look.
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#'        \describe{
#'          \item{ArrivalTime}{A numeric value with the time the patient arrived in the trial}
#'          \item{ArrTimeVisit[VisitID]}{A numeric value with the time the patient arrived in the trial for the [VisitID]th visit.}
#'          \item{TreatmentID}{An integer value where 0 indicates control treatment and 1 experimental treatment.}
#'          \item{Response[VisitID]}{Numeric value for the response from the patient at the [VisitID]th visit.}
#'          \item{CensorInd[VisitID]}{A binary (0-1) value where 1 indicates that the patient was censored at the [VisitID]th visit.}
#'          \item{DropoutVisitID}{An integer value which indicates the ID of the visit where the patient was censored.}
#'        }
#' @param DesignParam List of design and simulation parameters needed to compute test statistics and perform testing. Access elements by name, for example `DesignParam$Alpha`, rather than by position.
#'      \describe{
#'          \item{SampleSize}{Sample size of the trial}
#'          \item{Alpha}{Type I Error}
#'          \item{TestType}{Values are One side: 0; Two Sided: 1, Two Sided, Asymmetric: 2}
#'          \item{TailType}{Values are Left Tailed: 0, Right Tailed: 1}
#'          \item{LowerAlpha}{Lower Type I error. Present for Left Tailed and Two Sided Asymmetric Tests }
#'          \item{UpperAlpha}{Upper Type I error. Present for Right Tailed and Two Sided Asymmetric Tests }
#'          \item{MaxCompleters}{Maximum Completers for a Continuous ep design}
#'          \item{ResponseLag}{Fixed Followup time between first visit and Final visit}
#'          \item{AllocInfo}{Vector of ratios of treatment sample sizes to control sample size. Length = Number of treatment arms}
#'          \item{CriticalPoint}{Z Critical value for a given Alpha}
#'          \item{UpperCriticalPoint}{Upper Critical Value. Present in Right Tail Fixed Sample designs only }
#'          \item{LowerCriticalPoint}{Lower Critical Value. Present in Left Tail Fixed Sample designs only }
#'          \item{NumVisit}{Integer number of visits in a Design}
#'          \item{VisitTime}{Numeric vector containing visit times}
#'          \item{VisitStatus}{Integer vector indicating the visit selection status. 0 - Visit selected for analysis. 1 - Otherwise}
#'          \item{PrimContrastCoeff}{Numeric vector containing Primary Contrast Coefficient per visit}
#'          \item{SecContrastCoeff}{Numeric vector containing Secondary Contrast Coefficient per visit}
#'          \item{DropImpt}{Integer value for Dropout imputation method. 1 indicates None, 0 indicates LOCF}
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
#'                      \item{InterimVisit}{1 based index of the visit which is driving the interims}
#'                      \item{FutContrast}{The contrast based on which futility boundaries are being computed. 0- Primary, 1-Secondary}
#'                      \item{IncludePipeline}{Flag indicating whether to include pipeline subjects in the interim or not. 0- Don't include. 1- Include}
#'                 }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'                  User should access the variables using names, for example UserParam$Var1 and not order.
#'                  These variables can be of the following types: Integer, Numeric, or Character
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{Decision}{Required value. Integer Value with the following meaning:
#'                                  \describe{
#'                                    \item{Decision = 0}{when No boundary, futility or efficacy is  crossed}
#'                                    \item{Decision = 1}{when the Lower Efficacy Boundary Crossed}
#'                                    \item{Decision = 2}{when the Upper Efficacy Boundary Crossed}
#'                                    \item{Decision = 3}{when the Futility Boundary Crossed}
#'                                    \item{Decision = 4}{when the Equivalence Boundary Crossed}
#'                                    }
#'                                    }
#'                  \item{AnalysisTime} {Optional Numeric value. Estimate of Analysis time. Same as look time for interims. Same as study duration for the final analysis. To be computed and returned by the user. }
#'                  \item{ErrorCode}{ Optional value \describe{
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'
#'                  \item{PrimDelta}{ Float value that gives estimate of Primary contrast delta,
#'                                    Mandatory If FutBdryScale = 2 (Delta Scale) and FutContrast = 0 (Primary Contrast)
#'                                    else optional}
#'
#'                  \item{SecDelta}{ Float value that gives estimate of Secondary contrast delta
#'                                  Mandatory if FutBdryScale = 2 (Delta Scale) and FutContrast = 1 (Secondary Contrast)
#'                                  else optional}
#'
#'                   \item{TestStat}{ Float value, Mandatory If FutBdryScale = 2 (Delta Scale) and FutContrast = 0 (Primary Contrast)}
#'                                  { Mandatory If FutBdryScale = 2 (Delta Scale) and FutContrast = 1 (Secondary Contrast)
#'                                  else optional }
#'
#'                      }
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

{{FUNCTION_NAME}} <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
     nError           <- 0
     nDecision        <- 0
     dPrimDeltaEst    <- 0
     dSecDeltaEst     <- 0
    bIAEfficacyCheck <- TRUE
     bIAFutilityCheck <- FALSE
     bFAEfficacyCheck <- TRUE

     # Step 1 - If LookInfo is Null, then this is a fixed design and we use the DesignParam$MaxEvents
     # Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
     if( !is.null( LookInfo ) )
     {
         nQtyOfLooks          <- LookInfo$NumLooks
         nLookIndex           <- LookInfo$CurrLookIndex
         nQtyOfPatsInAnalysis <- LookInfo$CumCompleters[ nLookIndex ]
         nRejType             <- LookInfo$RejType
         nTailType            <- DesignParam$TailType
     }
     else
     {
         nQtyOfLooks          <- 1
         nLookIndex           <- 1
         nQtyOfPatsInAnalysis <- nrow( SimData )
         nTailType            <- DesignParam$TailType
     }

     # Generate decision using GetDecisionString and GetDecision helpers
     strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                                bIAEfficacyCondition = bIAEfficacyCheck,
                                                bIAFutilityCondition = bIAFutilityCheck,
                                                bFAEfficacyCondition = bFAEfficacyCheck )
     nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    return( list( Decision = as.integer( nDecision ), PrimDelta = as.double( dPrimDeltaEst ), SecDelta = as.double( dSecDeltaEst ), ErrorCode = as.integer( nError ) ) )
}
