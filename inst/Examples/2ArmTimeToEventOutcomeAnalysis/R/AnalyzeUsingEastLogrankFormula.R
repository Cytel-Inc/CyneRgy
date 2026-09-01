######################################################################################################################## .
#' AnalyzeUsingEastLogrankFormula
#' @name AnalyzeUsingEastLogrankFormula
#' @title Compute the statistic using formulas Q.242 and Q.243 in the East manual.
#' @author Anoop Singh Rawat, Shubham Lahoti, and Gabriel Potvin
#' @description Use the formulas Q.242 and Q.243 in the East manual to compute the statistic.  The purpose of this example is to demonstrate how the analysis and decision making can be modified in a simple approach.
#'              The test statistic is compared to the lower boundary computed and sent by East Horizon as an input. This example does NOT include a futility rule.
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
#' @return A named list containing log-rank `TestStat`, integer `Decision`, integer `ErrorCode`, and observed `HazardRatio`.
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

AnalyzeUsingEastLogrankFormula <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    # Step 1: Retrieve necessary information from the objects East Horizon sent. You may not need all the variables ####
    if( !is.null( LookInfo ) )
    {
        # Look info was provided so use it
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        vCumEvents           <- LookInfo$InfoFrac * DesignParam$MaxEvents
        nQtyOfEvents         <- vCumEvents[ nLookIndex ]
        dEffBdry             <- LookInfo$EffBdryLower[ nLookIndex ]
        nRejType             <- LookInfo$RejType
        nTailType            <- DesignParam$TailType
    }
    else
    {   # Look info is not provided for fixed sample designs so fetch the information appropriately
        nQtyOfLooks          <- 1
        nLookIndex           <- 1
        nQtyOfEvents         <- DesignParam$MaxEvents
        dEffBdry             <- DesignParam$CriticalPoint
        nTailType            <- DesignParam$TailType
    }

    SimData$TimeOfEvent      <- SimData$ArrivalTime + SimData$SurvivalTime    # This is the calendar time in the trial that the patients event is observed

    # Compute the time of analysis
    SimData                  <- SimData[ order( SimData$TimeOfEvent ), ]
    dTimeOfAnalysis          <- SimData[ nQtyOfEvents, ]$TimeOfEvent

    # Add the Observed Time variable
    SimData                  <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis , ]   # Exclude any patients that were not enrolled by the time of the analysis
    SimData$Event            <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )  # If the event is observed after the analysis it is not observed, eg censored
    SimData$ObservedTime     <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )

    # Order the data by observed time for the remainder of the computations
    SimData                  <- SimData[ order( SimData$ObservedTime ), ]

    # Compute Observed HR

    coxModel                 <- survival::coxph( survival::Surv( ObservedTime, Event ) ~ TreatmentID, data = SimData )
    dTrueHR                  <- exp( coxModel$coefficients )

    SimData$EventOnTreatment <- ifelse( SimData$TreatmentID == 1, SimData$Event, 0 ) # If the event is observed on treatment
    SimData$EventOnControl   <- ifelse( SimData$TreatmentID == 0, SimData$Event, 0 ) # If the event is observed on control

    # Arm wise count of subjects at risk at the beginning. Is same as arm wise sample size
    nSubjectsAtRiskTreatment <- nrow( SimData[ SimData$TreatmentID == 1, ] )
    nSubjectsAtRiskControl   <- nrow( SimData[ SimData$TreatmentID == 0, ] )

    # Initialize intermediate quantities required for test statistic computation
    dNum <- 0
    dDen <- 0

    # Iterate over subjects to calculate dNum and dDen required for test statistic computation
    for( nSubject in 1:nrow( SimData ) )
    {   # Update the count of subjects at risk for each arm for non event times
        if( SimData$Event[ nSubject ] == 0 )
        {
            if( SimData$TreatmentID[ nSubject ] == 1 )
            {
                nSubjectsAtRiskTreatment <- nSubjectsAtRiskTreatment - 1
            }
            if( SimData$TreatmentID[ nSubject ] == 0 )
            {
                nSubjectsAtRiskControl   <- nSubjectsAtRiskControl - 1
            }

        } # For subjects with events, compute dNum and dDen
        if( SimData$Event[ nSubject ] == 1 )
        {
            nEventsOnTreatment           <- SimData$EventOnTreatment[ nSubject ]
            nEventsOnControl             <- SimData$EventOnControl[ nSubject ]
            nEvents                      <- nEventsOnTreatment + nEventsOnControl
            nSubjectsAtRisk              <- nSubjectsAtRiskTreatment + nSubjectsAtRiskControl
            # Equation Q.242 in East Manual
            dNum <- dNum + nEventsOnTreatment - nSubjectsAtRiskTreatment * nEvents / nSubjectsAtRisk
            # Generate dDen based on number of subjects at risk
            if( nSubjectsAtRisk != 1 )
            {   # Equation Q.243 in East Manual
                dDen <- dDen + nSubjectsAtRiskTreatment * nSubjectsAtRiskControl * ( nSubjectsAtRisk - nEvents ) * nEvents / ( ( nSubjectsAtRisk - 1 ) * nSubjectsAtRisk ^ 2 )
            }
            # Update the count of subjects at risk before the next iteration
            nSubjectsAtRiskTreatment     <- nSubjectsAtRiskTreatment - nEventsOnTreatment
            nSubjectsAtRiskControl       <- nSubjectsAtRiskControl - nEventsOnControl
        }
    }

    # Compute the logrank test statistic
    dTS       <- dNum / sqrt( dDen )

    # Generate decision using GetDecisionString and GetDecision helpers
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                               bIAEfficacyCondition = dTS <  dEffBdry,
                                               bFAEfficacyCondition = dTS <  dEffBdry )
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    nError   <- 0

    lRet      <- list( TestStat = as.double( dTS ),
                      Decision  = as.integer( nDecision ),
                      ErrorCode = as.integer( nError ),
                      HazardRatio = as.double( dTrueHR ) )
    return( lRet )
}
