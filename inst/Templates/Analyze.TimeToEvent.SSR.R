######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Analyze Time-to-Event Outcomes with Sample Size Re-Estimation
#' @description Analyze simulated time-to-event outcomes and return the re-estimated event count and decision for the current look.
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#'        \describe{
#'          \item{ArrivalTime}{ A numeric value with the time the patient arrived in the trial}
#'          \item{TreatmentID}{An integer value where 0 indicates control treatment and 1 experimental treatment.}
#'          \item{SurvivalTime}{Numeric value for the survival time or time-to-event for the patient, note this is not the time in the trial
#'                               that the patient experiences the event.}
#'          \item{DropOutTime}{Numeric value for the dropout time for the patient in a time to event trial.}
#'        }
#' @param DesignParam List of design and simulation parameters needed to compute test statistics and perform testing. Access elements by name, for example `DesignParam$Alpha`, rather than by position.
#'      \describe{
#'          \item{SampleSize}{Sample size of the trial}
#'          \item{Alpha}{Type I Error}
#'          \item{TestType}{Values are One side: 0; Two Sided: 1, Two Sided, Asymmetric: 2}
#'          \item{TailType}{Values are Left Tailed: 0, Right Tailed: 1}
#'          \item{LowerAlpha}{Lower Type I error. Present for Left Tailed and Two Sided Asymmetric Tests }
#'          \item{UpperAlpha}{Upper Type I error. Present for Right Tailed and Two Sided Asymmetric Tests }
#'          \item{CriticalPoint}{Critical Value. Present in Fixed Sample designs only }
#'          \item{UpperCriticalPoint}{Upper Critical Value. Present in Right Tail Fixed Sample designs only }
#'          \item{LowerCriticalPoint}{Lower Critical Value. Present in Left Tail Fixed Sample designs only }
#'          \item{MaxEvents}{Maximum Events in a time to event based trial}
#'          \item{FollowUpType}{For survival tests, Follow Up Type. Possible values are: Until End of Study: 0, For fixed period: 1}
#'          \item{AllocInfo}{Vector of ratios of treatment sample sizes to control sample size. Length = Number of treatment arms }
#'      }
#' @param AdaptInfo List containing event-count and sample-size re-estimation parameters:
#'      \describe{
#'          \item{SSRFuncScale}{Rule type: 0 for a continuous rule or 1 for a step-function rule.}
#'          \item{PromZoneMin}{Lower bound of the promising zone for continuous SSR.}
#'          \item{PromZoneMax}{Upper bound of the promising zone.}
#'          \item{MaxEventsMult}{Maximum event-count multiplier.}
#'          \item{MaxSSMult}{Maximum sample-size multiplier.}
#'          \item{MaxSSMultInp}{List containing `From`, `To`, `MaxEventsMult`, and `MaxSSMult` values for step-function rules.}
#'      }
#' @param LookInfo List of parameters for the current analysis look. It is `NULL` for fixed-sample designs. Access elements by name, for example `LookInfo$NumLooks`, rather than by position.
#'                 \describe{
#'                      \item{NumLooks}{An integer value with the number of looks in the study}
#'                      \item{CurrLookIndex}{An integer value with the current index look, starting from 1}
#'                      \item{CumEvents}{Vector containing the cumulative number of events for each look.}
#'                      \item{InfoFrac}{Information fraction}
#'                      \item{LookTime}{Look time on the calendar scale.}
#'                      \item{RejType}{Rejection type identifying the enabled efficacy and futility boundaries.}
#'                      \item{CumAlpha}{Cumulative alpha spent. Present in one sided tests only }
#'                      \item{CumAlphaUpper}{Upper cum. alpha spent. Present in right tailed and two sided tests only }
#'                      \item{CumAlphaLower}{Lower cum. alpha spent. Present in left tailed and two sided tests only }
#'                      \item{EffBdryScale}{Efficacy boundary scale. Possible values are: Z Scale: 0, p-Value Scale: 1}
#'                      \item{EffBdry}{Vector of efficacy boundaries. Present in one sided tests only }
#'                      \item{EffBdryUpper}{Vector of upper efficacy boundaries. Present in right tailed and two sided tests only }
#'                      \item{EffBdryLower}{Vector of lower efficacy boundary. Present in left tailed and two sided tests only }
#'                      \item{FutBdryScale}{Futility boundary scale: Z scale = 0, p-value scale = 1, Delta scale = 2, conditional-power scale = 3, or hazard-ratio scale = 6.}
#'                      \item{FutBdry}{Vector of futility boundaries. Present in one sided tests only }
#'                      \item{FutBdryUpper}{Vector of upper futility boundaries. Present in left tailed and two sided tests only }
#'                      \item{FutBdryLower}{Vector of lower futility boundaries. Present in right tailed and two sided tests only }
#'                      \item{CPDeltaOption}{Conditional-power treatment-effect option: 0 for design Delta or 1 for estimated Delta.}
#'                      \item{BindingType}{Futility binding type: 0 for non-binding or 1 for binding.}
#'                 }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{Decision}{Optional value. Integer Value with the following meaning:
#'                                  \describe{
#'                                    \item{Decision = 0}{when No boundary, futility or efficacy is  crossed}
#'                                    \item{Decision = 1}{when the Lower Efficacy Boundary Crossed}
#'                                    \item{Decision = 2}{when the Upper Efficacy Boundary Crossed}
#'                                    \item{Decision = 3}{when the Futility Boundary Crossed}
#'                                    \item{Decision = 4}{when the Equivalence Boundary Crossed}
#'                                    }
#'                                    }
#'                  \item{TestStat}{Numeric value. Required if Decision is not returned}
#'                  \item{AnalysisTime} {Optional Numeric value. Estimate of Analysis time. Same as look time for interims. Same as study duration for the final analysis. To be computed and returned by the user. }
#'                  \item{ErrorCode}{Optional integer value \describe{
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'                  \item{HazardRatio}{Optional numeric value.
#'                                            Used in East Horizon Explore for creating the observed hazard ratio graph.
#'                                            Only applicable for time-to-event data.}
#'                      }
#' Template for sample size re-estimation with a time-to-event endpoint.
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( SimData, DesignParam, LookInfo = NULL, AdaptInfo = NULL, UserParam = NULL )
{
    nError  <- 0
    retval1 <- 0
    retval2 <- 0
    # Write the actual code here.
    # Use appropriate error handling and modify
    # the error appropriately.
    return( list( ReEstEvents = as.integer( retval1 ),
                  Decision    = as.integer( retval2 ),
                  ErrorCode   = as.integer( nError ) ) )
}
