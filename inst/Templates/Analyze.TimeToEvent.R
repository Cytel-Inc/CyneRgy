######################################################################################################################## .
# Last Modified Date: {{CREATION_DATE}}
#' @name {{FUNCTION_NAME}}
#' @title Analyze Time-to-Event Outcomes
#' @description Analyze simulated time-to-event outcomes at the current interim or final look.
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
#'                  User should access the variables using names, for example UserParam$Var1 and not order.
#'                  These variables can be of the following types: Integer, Numeric, or Character
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
    nError              <- 0
    nDecision          <- 0
    dTestStatistic   <- 0
    bIAEfficacyCheck <- TRUE
    bIAFutilityCheck <- FALSE
    bFAEfficacyCheck <- TRUE

    # Step 1 - If LookInfo is Null, then this is a fixed design and we use the DesignParam$MaxEvents
    # Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
    if( !is.null( LookInfo ) )
    {
        # Look info was provided so this is a group sequential design and need to use the look information
        nQtyOfLooks  <- LookInfo$NumLooks
        nLookIndex   <- LookInfo$CurrLookIndex
        vCumEvents   <- LookInfo$InfoFrac * DesignParam$MaxEvents
        nQtyOfEvents <- vCumEvents[ nLookIndex ]
        nRejType     <- LookInfo$RejType
        nTailType    <- DesignParam$TailType
    }
    else
    {
        nQtyOfLooks  <- 1
        nLookIndex   <- 1
        nQtyOfEvents <- DesignParam$MaxEvents
        nTailType    <- DesignParam$TailType
    }

    # Step 2 - The data that is sent in SimData is the simulated data, and the first step ####
    #           is to create an analysis data set based on the number events to get the time

    # Create the calendar time in the trial that the patients' event are observed
    SimData$TimeOfEvent  <- SimData$ArrivalTime + SimData$SurvivalTime

    # Compute the time of analysis using the number of events
    SimData              <- SimData[ order( SimData$TimeOfEvent ), ]
    dTimeOfAnalysis      <- SimData[ nQtyOfEvents, ]$TimeOfEvent

    # Add the Observed Time variable
    SimData              <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis , ]   # Exclude any patients that were not enrolled by the time of the analysis
    SimData$Event        <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )  # If the event is observed after the analysis it is not observed, e.g., censored
    SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )

    # Step 3 - Perform the desired analysis ####
    # Example - Fit a cox model
    # fitCox    <- coxph( Surv( ObservedTime, Event ) ~ as.factor( TreatmentID ), data = SimData )

    # Step 4 - Use the analysis to make a decision ####
    # The example code below uses the Cox model to compute the p-value and if less than alpha return efficacy value
    # dPValue   <- summary(fitCox)$coefficients[,"Pr(>|z|)"]
    # dZVal     <- summary(fitCox)$coefficients[,"z"]
    # dPValue   <- pnorm( dZVal, lower.tail = TRUE)

    # Step 5 - Setup look decision logic ####
    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                               bIAEfficacyCondition = dPValue <= DesignParam$Alpha,
                                               bIAFutilityCondition = bIAFutilityCheck,
                                               bFAEfficacyCondition = dPValue <= DesignParam$Alpha )

    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    lRet <- list( TestStat = as.double( dTestStatistic ),
                 Decision  = as.integer( nDecision ),
                 ErrorCode = as.integer( nError ) )
    return( lRet )
}
