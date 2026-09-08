#################################################################################################### .
#   Description: Common two-arm time-to-event endpoint simulation and analysis functions.
#################################################################################################### .


#' @name SimulatePatientSurvivalWeibull
#' @title Simulate Two-Arm Weibull Survival Outcomes
#'
#' @description Calls the Weibull implementation from the common `2ArmTimeToEventOutcomePatientSimulation` example.
#'
#' @param NumSub Integer number of subjects.
#' @param NumArm Integer number of trial arms.
#' @param ArrivalTime Numeric subject arrival times.
#' @param TreatmentID Integer treatment identifiers beginning at `0`.
#' @param SurvMethod Integer identifying how survival parameters are supplied.
#' @param NumPrd Integer number of survival periods.
#' @param PrdTime Numeric period start times.
#' @param SurvParam Numeric survival parameters arranged as required by `SurvMethod`.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the response integration point.
#' @export

SimulatePatientSurvivalWeibull <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod,
                                             NumPrd, PrdTime, SurvParam, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmTimeToEventOutcomePatientSimulation", "SimulatePatientSurvivalWeibull.R",
        "SimulatePatientSurvivalWeibull",
        list( NumSub = NumSub, NumArm = NumArm, ArrivalTime = ArrivalTime, TreatmentID = TreatmentID,
              SurvMethod = SurvMethod, NumPrd = NumPrd, PrdTime = PrdTime, SurvParam = SurvParam,
              UserParam = UserParam )
    ) )
}


#' @name SimulatePatientSurvivalMixtureExponentials
#' @title Simulate Survival Outcomes From a Mixture of Exponentials
#'
#' @description Calls the mixture-of-exponentials implementation from the common time-to-event patient simulation example.
#'
#' @inheritParams SimulatePatientSurvivalWeibull
#' @return A list in the format required by the response integration point.
#' @export

SimulatePatientSurvivalMixtureExponentials <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod,
                                                         NumPrd, PrdTime, SurvParam, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmTimeToEventOutcomePatientSimulation", "SimulatePatientSurvivalMixtureExponentials.R",
        "SimulatePatientSurvivalMixtureExponentials",
        list( NumSub = NumSub, NumArm = NumArm, ArrivalTime = ArrivalTime, TreatmentID = TreatmentID,
              SurvMethod = SurvMethod, NumPrd = NumPrd, PrdTime = PrdTime, SurvParam = SurvParam,
              UserParam = UserParam )
    ) )
}


#' @name AnalyzeUsingSurvivalPackage
#' @title Analyze Two-Arm Time-to-Event Outcomes
#'
#' @description Calls the log-rank implementation based on the `survival` package from the common
#' `2ArmTimeToEventOutcomeAnalysis` example.
#'
#' @param SimData Data frame containing the simulated patient data.
#' @param DesignParam List of design and simulation parameters.
#' @param LookInfo Optional list describing the current analysis look.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the analysis integration point.
#' @export

AnalyzeUsingSurvivalPackage <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmTimeToEventOutcomeAnalysis", "AnalyzeUsingSurvivalPackage.R", "AnalyzeUsingSurvivalPackage",
        list( SimData = SimData, DesignParam = DesignParam, LookInfo = LookInfo, UserParam = UserParam )
    ) )
}


#' @name AnalyzeUsingHazardRatioLimitsOfCI
#' @title Analyze Time-to-Event Outcomes Using Hazard-Ratio Confidence Limits
#'
#' @description Calls the confidence-limit implementation from the common time-to-event analysis example.
#'
#' @inheritParams AnalyzeUsingSurvivalPackage
#' @return A list in the format required by the analysis integration point.
#' @export

AnalyzeUsingHazardRatioLimitsOfCI <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmTimeToEventOutcomeAnalysis", "AnalyzeUsingHazardRatioLimitsOfCI.R",
        "AnalyzeUsingHazardRatioLimitsOfCI",
        list( SimData = SimData, DesignParam = DesignParam, LookInfo = LookInfo, UserParam = UserParam )
    ) )
}


#' @name AnalyzeUsingEastLogrankFormula
#' @title Analyze Time-to-Event Outcomes Using a Log-Rank Formula
#'
#' @description Calls the manual log-rank implementation from the common time-to-event analysis example.
#'
#' @inheritParams AnalyzeUsingSurvivalPackage
#' @return A list in the format required by the analysis integration point.
#' @export

AnalyzeUsingEastLogrankFormula <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmTimeToEventOutcomeAnalysis", "AnalyzeUsingEastLogrankFormula.R",
        "AnalyzeUsingEastLogrankFormula",
        list( SimData = SimData, DesignParam = DesignParam, LookInfo = LookInfo, UserParam = UserParam )
    ) )
}
