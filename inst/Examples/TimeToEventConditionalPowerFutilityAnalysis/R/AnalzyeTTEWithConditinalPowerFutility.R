######################################################################################################################## .
#' @name AnalzyeTTEWithConditinalPowerFutility
#' @title Time-To-Event Weighted Conditional Power Futility Analysis
#' @description
#' Performs a logrank analysis with conditional-power futility rules using a target
#' hazard ratio, the estimated hazard ratio, or a weighted combination of both.
#' @author Gabriel Potvin, Valeria A. G. Mazzanti, Sheetal Solanki
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
#' \describe{
#'   \item{UserParam$nComputationOption}{Specifies the method for conditional power:
#'       1 = Target hazard ratio (using UserParam$TargetHazardRatio),
#'       2 = Estimated hazard ratio,
#'       3 = Weighted hazard ratio (must supply UserParam$WeightEstimatedHR, UserParam$WeightTargetHR and UserParam$TargetHazardRatio).}
#'   \item{UserParam$FutilityThreshold}{Conditional-power threshold below which futility is declared.}
#'   \item{UserParam$TargetHazardRatio}{Target hazard ratio used in computation options 1 and 3.}
#'   \item{UserParam$WeightEstimatedHR}{Weight assigned to the estimated hazard ratio in computation option 3.}
#'   \item{UserParam$WeightTargetHR}{Weight assigned to the target hazard ratio in computation option 3.}
#' }
#'
#' @return A list containing:
#'   \describe{
#'     \item{TestStat}{Numeric. The logrank test statistic ($Z$).}
#'     \item{HR}{Numeric. The estimated hazard ratio at the current analysis.}
#'     \item{Decision}{Integer. Analysis decision code:
#'       0 = No boundary crossed,
#'       1 = Lower efficacy boundary crossed,
#'       2 = Upper efficacy boundary crossed,
#'       3 = Futility boundary crossed,
#'       4 = Equivalence boundary crossed.}
#'     \item{ErrorCode}{Integer. Error status:
#'       0 = No error,
#'       >0 = Nonfatal error (current simulation aborted, others continue),
#'       <0 = Fatal error (all simulations aborted).}
#'     \item{dConditionalPower}{Numeric. The conditional power at the current analysis (–1 if not computed).}
#'   }
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

AnalzyeTTEWithConditinalPowerFutility <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
  nError             <- 0
  nDecision         <- 0
  dTestStatistic    <- 0
  nLookIndex        <- 1
  dEstimatedHR      <- 1

  if( !is.null( LookInfo ) )
  {
    nQtyOfLooks  <- LookInfo$NumLooks
    nLookIndex   <- LookInfo$CurrLookIndex
    vCumEvents   <- LookInfo$InfoFrac * DesignParam$MaxEvents
    nQtyOfEvents <- vCumEvents[ nLookIndex ]
  }
  else
  {
    nQtyOfLooks  <- 1
    nLookIndex   <- 1
    nQtyOfEvents <- DesignParam$MaxEvents
  }

  # Prepare event and censoring times from simulated data
  SimData$TimeOfEvent  <- SimData$ArrivalTime + SimData$SurvivalTime
  SimData              <- SimData[ order( SimData$TimeOfEvent ), ]
  dTimeOfAnalysis      <- SimData[ nQtyOfEvents, ]$TimeOfEvent
  SimData              <- SimData[ SimData$ArrivalTime <= dTimeOfAnalysis, ]
  SimData$Event        <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, 0, 1 )
  SimData$ObservedTime <- ifelse( SimData$TimeOfEvent > dTimeOfAnalysis, dTimeOfAnalysis - SimData$ArrivalTime, SimData$TimeOfEvent - SimData$ArrivalTime )

  # Perform Logrank test
  logrankTest <- survival::survdiff( survival::Surv( ObservedTime, Event ) ~ TreatmentID, data = SimData )
  dTestStatistic <- sqrt( logrankTest$chisq ) * sign( logrankTest$obs[ 2 ] - logrankTest$exp[ 2 ] )
  dPValue <- 1 - pchisq( logrankTest$chisq, df = 1 )

  # Retrieve user-defined parameters
  dTargetHR            <- UserParam$TargetHazardRatio
  dFutilityThreshold   <- UserParam$FutilityThreshold

  # Critical value for the efficacy boundary (one-sided significance level of 2.5%)
  dEffBdry <- -qnorm( 0.025 )

  nDecision <- 0 # Set a default value
  if( nLookIndex < nQtyOfLooks )
  {
    if( UserParam$nComputationOption == 1 )
    {
      # Option 1: Compute CP using HR* = UserParam$TargetHazardRatio

      r <- DesignParam$AllocInfo / ( 1 + DesignParam$AllocInfo )
      dSEHR <- 1 / sqrt( LookInfo$CumCompleters[ 1 ] * r * ( 1-r ) )
      dConditionalPower <- pnorm(
                                  dEffBdry * sqrt( 1 + LookInfo$CumCompleters[ 1 ] / ( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] ) ) -
                                  dTestStatistic * sqrt( LookInfo$CumCompleters[ 1 ] / ( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] ) ) -
                                  log( dTargetHR ) * sqrt( r * ( 1-r ) ) * sqrt( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] )
                           )
    }
    else if( UserParam$nComputationOption == 2 )
    {
      # Option 2: Compute CP using HR* = observed HR

      r <- DesignParam$AllocInfo / ( 1 + DesignParam$AllocInfo )
      dSEHR <- 1 / sqrt( LookInfo$CumCompleters[ 1 ] * r * ( 1-r ) )
      dEstimatedHR <- exp( dTestStatistic * dSEHR )
      dConditionalPower <- pnorm(
                                  dEffBdry * sqrt( 1 + LookInfo$CumCompleters[ 1 ] / ( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] ) ) -
                                  dTestStatistic * sqrt( LookInfo$CumCompleters[ 1 ] / ( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] ) ) -
                                  log( dEstimatedHR ) * sqrt( r * ( 1-r ) ) * sqrt( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] )
                           )
    }
    else if( UserParam$nComputationOption == 3 )
    {

      dWeightEstimated     <- UserParam$WeightEstimatedHR
      dWeightTarget        <- UserParam$WeightTargetHR

      # Option 3: Compute CP using HR* = weighted combination

      r <- DesignParam$AllocInfo / ( 1 + DesignParam$AllocInfo )
      dSEHR <- 1 / sqrt( LookInfo$CumCompleters[ 1 ] * r * ( 1-r ) )
      dEstimatedHR <- exp( dTestStatistic * dSEHR )
      dWeightedHR <- ( dWeightEstimated * dEstimatedHR ) + ( dWeightTarget * dTargetHR )
      dConditionalPower <- pnorm(
                                  dEffBdry * sqrt( 1 + LookInfo$CumCompleters[ 1 ] / ( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] ) ) -
                                  dTestStatistic * sqrt( LookInfo$CumCompleters[ 1 ] / ( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] ) ) -
                                  log( dWeightedHR ) * sqrt( r * ( 1-r ) ) * sqrt( LookInfo$CumCompleters[ 2 ] - LookInfo$CumCompleters[ 1 ] )
                           )
    }
    else
    {
      throw( new( "InvalidParameterError",
                  message = "Invalid computation option specified in UserParam$nComputationOption" ) )
    }

    # Make futility decision based on conditional power
    if( dConditionalPower < dFutilityThreshold )
    {
      nDecision <- 3  # Futility boundary crossed
    }
  }

  else if( nLookIndex == nQtyOfLooks )
  {
    # Check efficacy
    dConditionalPower  <- -1
    if( dTestStatistic <= dEffBdry )
    {
      # Efficacy boundary crossed
      strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, bIAEfficacyCondition = FALSE, bIAFutilityCondition = FALSE,
                     bFAEfficacyCondition = TRUE, bFAFutilityCondition = FALSE )
    }
    else
    {
      # Final analysis: if efficacy not achieved, declare futility
      strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, bIAEfficacyCondition = FALSE, bIAFutilityCondition = FALSE,
                     bFAEfficacyCondition = FALSE, bFAFutilityCondition = TRUE )
    }
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
  }

  # Return test statistic, hazard ratio, conditional power, and decision
  lRet <- list( TestStat = as.double( dTestStatistic ),
                HR = as.double( dEstimatedHR ),
                Decision  = as.integer( nDecision ),
                ErrorCode = as.integer( nError ),
                dConditionalPower = as.double( dConditionalPower ) )
  return( lRet )
}
