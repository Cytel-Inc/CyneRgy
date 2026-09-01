######################################################################################################################## .
#' Last Modified Date: {{CREATION_DATE}}
#'
#' @name {{FUNCTION_NAME}}
#'
#' @title Generate Dual-Endpoint Decisions
#'
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#'   \describe{
#'     \item{ArrivalTime}{Calendar time at which the subject entered the trial.}
#'     \item{TreatmentID}{Treatment assignment, where 0 represents control and 1 represents experimental treatment.}
#'     \item{ResponseX}{Response or survival time for endpoint `X`, where `X` is the endpoint index.}
#'     \item{CensorIndOrgX}{Original censoring indicator for endpoint `X`; 0 indicates censored and 1 indicates complete.}
#'     \item{ClndrRespTimeX}{Calendar response or event time for endpoint `X`.}
#'   }
#' @param DesignParam List of design and simulation parameters needed to compute test statistics and perform testing. Access elements by name, for example `DesignParam$Alpha`, rather than by position.
#'                    \describe{
#'                      \item{EndpointType}{Integer vector with number of endpoints elements. Indicates endpoint type for each endpoint:
#'                            0 - Continuous, 1 - Binary, 2 - TTE}
#'                      \item{EndpointName}{Character vector with number of endpoints elements. Names for each endpoint as specified by the user}
#'                      \item{WinCond}{Integer value indicating winning condition: 1 - At least Endpoint 1,
#'                            2 - At least Endpoint 2, 3 - At least one endpoint, 4 - Both endpoints}
#'                      \item{TailType}{List with tail type for each endpoint. Access using the actual endpoint names specified by the user,
#'                            e.g., TailType[EndpointName[1]] or TailType[EndpointName[2]]. Values: 0 - Left Tailed, 1 - Right Tailed}
#'                      \item{FollowUpType}{List with follow up type for each endpoint. Access using the actual endpoint names specified by the user,
#'                            e.g., FollowUpType[EndpointName[1]] or FollowUpType[EndpointName[2]].
#'                            Values: 0 - Until End of the Study, 1 - For Fixed Period, NA - For Binary endpoint}
#'                      \item{FollowUpDur}{List with follow-up duration for each endpoint. Access using the actual endpoint names specified by the user,
#'                            e.g., FollowUpDur[EndpointName[1]] or FollowUpDur[EndpointName[2]]}
#'                      \item{TrialType}{List with trial type for each endpoint. Access using the actual endpoint names specified by the user,
#'                            e.g., TrialType[EndpointName[1]] or TrialType[EndpointName[2]].
#'                            Values: 0 - Superiority, 1 - Non-Inferiority}
#'                      \item{VarType}{Integer value indicating variance type in TTE-Binary designs: 0 - Pooled, 1 - Un-Pooled}
#'                      \item{PlanEndTrial}{Integer value indicating planned end of trial: 1 - Full Info for both endpoints,
#'                            2 - Full Info for Endpoint 1, 3 - Full Info for Endpoint 2}
#'                      \item{AllocInfo}{Numeric vector with ratios of treatment group sample sizes to control group sample size}
#'                      \item{Alpha}{Numeric value for Type I Error}
#'                      \item{CriticalPoint}{List with critical value for each endpoint in fixed sample designs.
#'                            Access using the actual endpoint names specified by the user,
#'                            e.g., CriticalPoint[EndpointName[1]] or CriticalPoint[EndpointName[2]]}
#'                      \item{UpperCriticalPoint}{List with upper critical value for each endpoint in right-tailed fixed sample designs.
#'                            Access using the actual endpoint names specified by the user,
#'                            e.g., UpperCriticalPoint[EndpointName[1]] or UpperCriticalPoint[EndpointName[2]]}
#'                      \item{LowerCriticalPoint}{List with lower critical value for each endpoint in left-tailed fixed sample designs.
#'                            Access using the actual endpoint names specified by the user,
#'                            e.g., LowerCriticalPoint[EndpointName[1]] or LowerCriticalPoint[EndpointName[2]]}
#'                      \item{SampleSize}{Integer value for total sample size}
#'                      \item{TestID}{Integer value for test ID. For dual endpoints, this is the same as single endpoint TTE test}
#'                      \item{MultAdj}{Integer value for multiplicity adjustments: 0 - None, 1 - Fallback, 2 - Fixed Sequence,
#'                            3 - Weighted Bonferroni, 4 - Weighted Bonferroni-Holms, 5 - Weighted Hochberg}
#'                      \item{TestOrder}{Integer value for testing order in Fallback or Fixed Sequence: 1 - Start with Endpoint 1,
#'                            2 - Start with Endpoint 2}
#'                      \item{MaxEvents}{List with maximum events for each endpoint. Access using the actual endpoint names specified by the user,
#'                            e.g., MaxEvents[EndpointName[1]] or MaxEvents[EndpointName[2]]. Fixed to NA for any Binary endpoint}
#'                      \item{MaxCompleters}{List with maximum number of completers for each endpoint. Access using the actual endpoint names specified by the user,
#'                            e.g., MaxCompleters[EndpointName[1]] or MaxCompleters[EndpointName[2]]. Fixed to NA for any TTE endpoint}
#'                      \item{TrtEffNull}{List with treatment effect under null hypothesis for each endpoint. Access using the actual endpoint names specified by the user,
#'                            e.g., TrtEffNull[EndpointName[1]] or TrtEffNull[EndpointName[2]]. Specified in natural log scale for TTE endpoints}
#'                      \item{AlphaAlloc}{List with Type-1 Error allocation percentage for each endpoint in certain multiplicity
#'                            adjustment methods. Access using the actual endpoint names specified by the user,
#'                            e.g., AlphaAlloc[EndpointName[1]] or AlphaAlloc[EndpointName[2]]}
#'                      \item{TargetSSFA}{List with target sample size for final analysis for each endpoint. Access using the actual endpoint names specified by the user,
#'                            e.g., TargetSSFA[EndpointName[1]] or TargetSSFA[EndpointName[2]]. Fixed to NA for TTE endpoints or if not specified by user}
#'                      \item{TestStat}{List with test statistic output for each endpoint. Access using the actual endpoint names specified by the user,
#'                            e.g., TestStat[EndpointName[1]] or TestStat[EndpointName[2]]. Fixed to NA for endpoints whose analysis is pending}
#'                    }
#' @param LookInfo List of parameters for the current analysis look. It is `NULL` for fixed-sample designs. Access elements by name, for example `LookInfo$NumLooks`, rather than by position.
#'                   \describe{
#'                     \item{NumEndpointLooks}{Integer list with number of looks for each endpoint. Access using endpoint names,
#'                           e.g., NumEndpointLooks["Endpoint 1"] or NumEndpointLooks["Endpoint 2"]}
#'                     \item{NumLooks}{Integer value for total number of looks. Equal to Max(NumEndpointLooks["Endpoint 1"], NumEndpointLooks["Endpoint 2"])}
#'                     \item{CurrLookIndex}{Integer value with the current index look (1-Based)}
#'                     \item{SyncInterim}{Integer value indicating which endpoint to base interim looks on: 1 - Based on Endpoint 1, 2 - Based on Endpoint 2}
#'                     \item{InputInfoFrac}{Numeric list with information fraction for each endpoint. Access using endpoint names,
#'                           e.g., InputInfoFrac["Endpoint 1"] or InputInfoFrac["Endpoint 2"]. Same as [Analysis Spacing (\%) / 100].
#'                           Some entries may be NA depending on SyncInterim setting and number of looks for each endpoint}
#'                     \item{CumCompleters}{Integer list with cumulative completers for each endpoint. Access using endpoint names,
#'                           e.g., CumCompleters["Endpoint 1"] or CumCompleters["Endpoint 2"]. Fixed to NA for any TTE endpoint}
#'                     \item{CumEvents}{Integer list with cumulative events for each endpoint. Access using endpoint names,
#'                           e.g., CumEvents["Endpoint 1"] or CumEvents["Endpoint 2"]. Fixed to NA for any Binary endpoint}
#'                     \item{RejType}{Integer list with rejection type for each endpoint. Access using endpoint names,
#'                           e.g., RejType["Endpoint 1"] or RejType["Endpoint 2"]. Values: 0 - 1 Sided Efficacy Upper,
#'                           1 - 1 Sided Futility Upper, 2 - 1 Sided Efficacy Lower, 3 - 1 Sided Futility Lower,
#'                           4 - 1 Sided Efficacy Upper Futility Lower, 5 - 1 Sided Efficacy Lower Futility Upper}
#'                     \item{EffBdryScale}{Integer list with efficacy boundary scale for each endpoint. Access using endpoint names,
#'                           e.g., EffBdryScale["Endpoint 1"] or EffBdryScale["Endpoint 2"]. Values: 0 - Z Scale}
#'                     \item{EffBdry}{Numeric list with efficacy boundary values for each endpoint. Access using endpoint names,
#'                           e.g., EffBdry["Endpoint 1"] or EffBdry["Endpoint 2"]. Some entries may be NA if user has skipped
#'                           efficacy boundaries for some looks}
#'                     \item{EffBdryUpper}{Numeric list with upper efficacy boundary values for each endpoint (for right-tailed tests).
#'                           Access using endpoint names, e.g., EffBdryUpper["Endpoint 1"] or EffBdryUpper["Endpoint 2"]}
#'                     \item{EffBdryLower}{Numeric list with lower efficacy boundary values for each endpoint (for left-tailed tests).
#'                           Access using endpoint names, e.g., EffBdryLower["Endpoint 1"] or EffBdryLower["Endpoint 2"]}
#'                     \item{FutBdryScale}{Integer list with futility boundary scale for each endpoint. Access using endpoint names,
#'                           e.g., FutBdryScale["Endpoint 1"] or FutBdryScale["Endpoint 2"]. Values: 0 - Z scale, 2 - Delta Scale, 6 - HR Scale}
#'                     \item{FutBdry}{Numeric list with futility boundary values for each endpoint. Access using endpoint names,
#'                           e.g., FutBdry["Endpoint 1"] or FutBdry["Endpoint 2"]. Dimension for each endpoint is Number of Looks - 1.
#'                           Some entries may be NA if user has skipped futility boundaries for some looks}
#'                     \item{FutBdryUpper}{Numeric list with upper futility boundary values for each endpoint (for left-tailed tests).
#'                           Access using endpoint names, e.g., FutBdryUpper["Endpoint 1"] or FutBdryUpper["Endpoint 2"]}
#'                     \item{FutBdryLower}{Numeric list with lower futility boundary values for each endpoint (for right-tailed tests).
#'                           Access using endpoint names, e.g., FutBdryLower["Endpoint 1"] or FutBdryLower["Endpoint 2"]}
#'                     \item{BindingType}{Integer list with binding type for each endpoint. Access using endpoint names,
#'                           e.g., BindingType["Endpoint 1"] or BindingType["Endpoint 2"]. Values: 0 - Non Binding}
#'                   }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#' @param TestStat List of test statistics for both the endpoints. These test statistics will be on the Z-scale. Access using the actual endpoint names specified by the user,
#'                            e.g., TestStat[EndpointName[1]] or TestStat[EndpointName[2]]
#' @param OutList List of outputs that was returned by the user in the previous look. Only relevant for Group Sequential Design and set to NULL for first look.
#' Supported data types are lists, and scalar and vector of type numeric, integer and character.
#' @return The function must return a list in the return statement of the function. The information below lists
#'             elements of the list, if the element is required or optional and a description of the return values if needed.
#'             \describe{
#'                  \item{Decision}{Required value. A list of decisions on both endpoints: 0 - No Boundary Crossed, 1 - Lower Efficacy Boundary Crossed, 2 - Upper Efficacy Boundary Crossed, 4 - Futility Boundary Crossed.}
#'                  \item{Outlist}{Optional list of quantities to pass to the next look. This will be available as inputs to this function in the next look.
#'                            Only applicable for Group Sequential Design. Supported data types are lists, and scalar and vector of type numeric, integer and character.}
#'                  \item{ErrorCode}{Optional integer value \describe{
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }}
#'             }
#'
#'
#' @description
#' Generate endpoint decisions and optional persistent output for a dual-endpoint analysis.
#' The function signature must remain the same.
#' If your custom logic requires use of additional parameters that are not listed above, add them to UserParam.
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( SimData, DesignParam, LookInfo = NULL, TestStat = NULL, OutList = NULL, UserParam = NULL )
{
    EndpointName  <- DesignParam$EndpointName
    lDecision <- list()
    lDecision[ EndpointName[[ 1 ] ] ] <- lDecision[ EndpointName[[ 2 ] ] ] <- 0
    nError          <- 0
    Retval          <- 0
    OutList         <- list()
    OutList$OutVal  <- Retval

    # Write logic to implement a particular multiplicity adjustment method and update Decisions

    return( list( Decision = as.list( lDecision ), OutList = as.list( OutList ), ErrorCode = as.integer( nError ) ) )
}
