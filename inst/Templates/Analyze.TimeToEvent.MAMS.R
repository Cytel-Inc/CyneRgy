######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}

#' @name {{FUNCTION_NAME}}
#' @title Analyze Multi-Arm Time-to-Event Outcomes
#' @param SimData Data frame containing subject data generated in the current simulation, with one row per subject. Access variables by column name; optional outputs from response generation and dropout are also available as columns.
#'        \describe{
#'          \item{ArrivalTime}{A numeric value with the time the patient arrived in the trial}
#'          \item{TreatmentID}{An integer value specifying the index of arms to which subjects are allocated (one arm index per subject). Index for control is 0}
#'          \item{SurvivalTime}{Numeric value for the survival time or time-to-event for the patient, note this is not the time in the trial
#'                               that the patient experiences the event.}
#'          \item{DropOutTime}{Numeric value for the dropout time for the patient in a time-to-event trial.}
#'        }
#'
#' @param DesignParam List of design and simulation parameters needed to compute test statistics and perform testing. Access elements by name, for example `DesignParam$Alpha`, rather than by position.
#'      \describe{
#'          \item{SampleSize}{Integer. Sample size of the trial}
#'          \item{Alpha}{Numeric. Type I Error}
#'          \item{TrialType}{Integer. Type of the Trial. Values are Superiority: 0}
#'          \item{TestType}{Integer. Values are One side: 0}
#'          \item{TailType}{Integer. Values are Left Tailed: 0, Right Tailed: 1}
#'          \item{InitialAllocInfo}{Vector of the ratios of the treatment group sample sizes to control group sample size. Length = number of treatment arms.}
#'          \item{TestID}{Integer identifier for the configured time-to-event test.}
#'          \item{MultAdjMethod}{Integer. Multiple Comparison Procedure. Values are Bonferroni: 0, Weighted Bonferroni: 2, Hochberg's Step Up: 4, Fixed Sequence: 6, Fallback: 7}
#'          \item{NumTreatments}{Integer. Number of Treatment arms}
#'          \item{AlphaProp}{Vector of Proportions of Alpha for each treatment arm}
#'          \item{TestSeq}{Vector of integer Test Sequence for each comparison which corresponds to each treatment arm.}
#'          \item{CriticalPoint}{Numeric. Critical Value for a fixed sample design.}
#'          \item{IsArmPresent}{Vector of integer flags indicating whether an arm is still present in the trial or was dropped in the interim. Length = number of treatment arms. Values are - Dropped in the interim: 0, Still present in the trial: 1}
#'          \item{UpdatedAllocInfo}{Vector of ratios of the treatment group sample sizes to control group sample size which may have been updated during treatment selection. Length = number of treatment arms.}
#'          \item{MaxEvents}{Integer. Maximum Events.}
#'          \item{FollowUpType}{Integer. Follow up Type. Values are Until end of the study: 0, For fixed period: 1}
#'          \item{FollowUpDur}{Numeric follow-up duration in time units.}
#'      }
#'
#' @param LookInfo List of parameters for the current analysis look. It is `NULL` for fixed-sample designs. Access elements by name, for example `LookInfo$NumLooks`, rather than by position.
#'        \describe{
#'        \item{NumLooks}{Total number of analyses}
#'        \item{CurrLookIndex}{Current analysis index}
#'        \item{CumEvents}{Vector containing the cumulative number of events for each look.}
#'        \item{InfoFrac}{Vector of information fractions for each look.}
#'        \item{LookTime}{Look time on the calendar scale.}
#'        \item{RejType}{Rejection type identifying the enabled efficacy and futility boundaries.}
#'        \item{EffBdryScale}{Efficacy boundary scale: 0 for Z scale or 1 for adjusted p-value scale.}
#'        \item{EffBdry}{Efficacy boundaries for one-sided tests.}
#'        \item{EffBdryUpper}{Upper efficacy boundaries where applicable.}
#'        \item{EffBdryLower}{Lower efficacy boundaries where applicable.}
#'        \item{FutBdryScale}{Futility boundary scale: 1 for adjusted p-value, 2 for Delta, or 6 for hazard ratio.}
#'        \item{FutBdry}{Futility boundaries for one-sided tests.}
#'        \item{FutBdryUpper}{Upper futility boundaries where applicable.}
#'        \item{FutBdryLower}{Lower futility boundaries where applicable.}
#'        \item{BindingType}{Futility binding type: 0 for non-binding or 1 for binding.}
#'        }
#'
#' @param UserParam A list of user-defined parameters in East Horizon. Set the default to NULL, as shown in this example. If values are provided, access them as UserParam$ParameterName. Parameters must be Integer, Numeric, or Character. Do not pass UserParam directly to a helper function, as this may prevent East Horizon from populating the required parameters.
#'
#' @return A list containing `ErrorCode` and one or more of the following analysis outputs:
#'   \describe{
#'     \item{Decision}{Optional integer vector of length `DesignParam$NumTreatments`; `NA` indicates an arm dropped previously, 0 indicates no boundary crossed, 1 indicates lower efficacy, 2 indicates upper efficacy, and 3 indicates futility.}
#'     \item{TestStat}{Optional numeric vector of Wald (Z)-scale test statistics, one per treatment arm. Required with `RawPVal` for Dunnett multiplicity adjustments.}
#'     \item{AdjPVal}{Optional numeric vector of multiplicity-adjusted p-values, one per treatment arm.}
#'     \item{RawPVal}{Optional numeric vector of unadjusted p-values, one per treatment arm.}
#'     \item{Delta}{Optional numeric vector of treatment-effect estimates. Required when the futility boundary uses the Delta scale.}
#'     \item{HR}{Optional numeric vector of hazard-ratio estimates. Required when the futility boundary uses the hazard-ratio scale.}
#'     \item{AnalysisTime}{Optional numeric estimate of analysis time; the look time at an interim analysis or study duration at the final analysis.}
#'     \item{ErrorCode}{Optional integer status code; 0 indicates no error, a positive value aborts the current simulation but allows subsequent simulations, and a negative value stops further simulation.}
#'   }
#'
#' @description Analyze simulated time-to-event outcomes for a multiple-arm confirmatory design at the current look.
#'
#' The function signature must remain unchanged. However, additional user-defined logic
#' and parameters may be incorporated through the UserParam list if needed.
#' @keywords Multi-Arm, time-to-event endpoints analysis.
######################################################################################################################## .

{{FUNCTION_NAME}} <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Step 1 - Initialization
    vDecision       <- rep( 0, DesignParam$NumTreatments )
    nError          <- 0
    vHRRatio        <- rep( NA, DesignParam$NumTreatments )
    dTimeOfAnalysis <- NA

    # Step 2 - Retrieve design and interim analysis information ####
    # If interim look information is supplied use the current look specific
    # efficacy boundaries and event counts. Otherwise use the fixed sample settings
    if( !is.null( LookInfo ) )
    {

        # Example interim design setup
        nQtyOfLooks             <- LookInfo$NumLooks
        nLookIndex              <- LookInfo$CurrLookIndex
        vEfficacyBoundary       <- LookInfo$EffBdry[ nLookIndex ]

    }
    else
    {

        # Example fixed sample setup
        nQtyOfLooks             <- 1
        nLookIndex              <- 1
        vEfficacyBoundaryPScale <- DesignParam$Alpha
    }

    # Step 3 - Implement the analysis logic ####

    # Step 4 - Error checking ####
    # Add any required validation checks and update the error code if needed

    # Step 5 - Build the return object ####
    lReturn <- list( Decision     = as.integer( vDecision ),
                     ErrorCode    = as.integer( nError ),
                     HR           = as.double( vHRRatio ),
                     AnalysisTime = as.double( dTimeOfAnalysis ) )

    return( lReturn )

}
