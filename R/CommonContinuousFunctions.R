#################################################################################################### .
#   Description: Common two-arm continuous endpoint simulation and analysis functions.
#################################################################################################### .


#' @name SimulatePatientOutcomePercentAtZero
#' @title Simulate Two-Arm Continuous Outcomes
#'
#' @description Calls the implementation from the common `2ArmNormalOutcomePatientSimulation` example. Outcomes follow the
#' arm-specific normal distributions, with optional arm-specific probabilities of a structural zero in `UserParam`.
#'
#' @param NumSub Integer number of subjects.
#' @param ArrivalTime Numeric subject arrival times.
#' @param TreatmentID Integer treatment identifiers beginning at `0`.
#' @param Mean Numeric arm-specific means.
#' @param StdDev Numeric arm-specific standard deviations.
#' @param UserParam Optional list of user-defined parameters described in the complete example.
#'
#' @return A list in the format required by the corresponding integration point.
#' @export

SimulatePatientOutcomePercentAtZero <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmNormalOutcomePatientSimulation", "SimulatePatientOutcomePercentAtZero.R",
        "SimulatePatientOutcomePercentAtZero",
        list( NumSub = NumSub, ArrivalTime = ArrivalTime, TreatmentID = TreatmentID,
              Mean = Mean, StdDev = StdDev, UserParam = UserParam )
    ) )
}


#' @name SimulatePatientOutcomePercentAtZeroBetaDist
#' @title Simulate Two-Arm Continuous Outcomes With Random Structural-Zero Probabilities
#'
#' @description Calls the beta-distribution variant from the common `2ArmNormalOutcomePatientSimulation` example.
#'
#' @inheritParams SimulatePatientOutcomePercentAtZero
#' @return A list in the format required by the response integration point.
#' @export

SimulatePatientOutcomePercentAtZeroBetaDist <- function( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmNormalOutcomePatientSimulation", "SimulatePatientOutcomePercentAtZeroBetaDist.R",
        "SimulatePatientOutcomePercentAtZeroBetaDist",
        list( NumSub = NumSub, ArrivalTime = ArrivalTime, TreatmentID = TreatmentID,
              Mean = Mean, StdDev = StdDev, UserParam = UserParam )
    ) )
}


#' @name AnalyzeUsingTTestNormal
#' @title Analyze Two-Arm Continuous Outcomes With a T-Test
#'
#' @description Calls the t-test implementation from the common `2ArmNormalOutcomeAnalysis` example.
#'
#' @param SimData Data frame containing the simulated patient data.
#' @param DesignParam List of design and simulation parameters.
#' @param LookInfo Optional list describing the current analysis look.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the analysis integration point.
#' @export

AnalyzeUsingTTestNormal <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmNormalOutcomeAnalysis", "AnalyzeUsingTTestNormal.R", "AnalyzeUsingTTestNormal",
        list( SimData = SimData, DesignParam = DesignParam, LookInfo = LookInfo, UserParam = UserParam )
    ) )
}


#' @name AnalyzeUsingMeanLimitsOfCI
#' @title Analyze Continuous Outcomes Using Confidence-Interval Limits
#'
#' @description Calls the confidence-interval implementation from the common `2ArmNormalOutcomeAnalysis` example.
#'
#' @inheritParams AnalyzeUsingTTestNormal
#' @return A list in the format required by the analysis integration point.
#' @export

AnalyzeUsingMeanLimitsOfCI <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmNormalOutcomeAnalysis", "AnalyzeUsingMeanLimitsOfCI.R", "AnalyzeUsingMeanLimitsOfCI",
        list( SimData = SimData, DesignParam = DesignParam, LookInfo = LookInfo, UserParam = UserParam )
    ) )
}


#' @name AnalyzeUsingEastManualFormulaNormal
#' @title Analyze Continuous Outcomes Using the Manual Test-Statistic Formula
#'
#' @description Calls the manual-formula implementation from the common `2ArmNormalOutcomeAnalysis` example.
#'
#' @inheritParams AnalyzeUsingTTestNormal
#' @return A list in the format required by the analysis integration point.
#' @export

AnalyzeUsingEastManualFormulaNormal <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmNormalOutcomeAnalysis", "AnalyzeUsingEastManualFormulaNormal.R",
        "AnalyzeUsingEastManualFormulaNormal",
        list( SimData = SimData, DesignParam = DesignParam, LookInfo = LookInfo, UserParam = UserParam )
    ) )
}
