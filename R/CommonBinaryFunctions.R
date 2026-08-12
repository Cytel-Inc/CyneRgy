#################################################################################################### .
#   Description: Common two-arm binary endpoint functions.
#################################################################################################### .
#' @name SimulatePatientOutcomePercentAtZeroBetaDist.Binary
#' @title Simulate Binary Outcomes With Random Structural-Zero Probabilities
#'
#' @description Calls the beta-distribution variant from the common `2ArmBinaryOutcomePatientSimulation` example.
#'
#' @param NumSub Integer number of subjects.
#' @param NumArm Integer number of trial arms.
#' @param ArrivalTime Numeric subject arrival times.
#' @param TreatmentID Integer treatment identifiers beginning at `0`.
#' @param PropResp Numeric response probability for each arm.
#' @param UserParam Optional list of user-defined parameters described in the complete example.
#'
#' @return A list in the format required by the response integration point.
#' @export
#################################################################################################### .

SimulatePatientOutcomePercentAtZeroBetaDist.Binary <- function( NumSub, NumArm, ArrivalTime, TreatmentID,
                                                                 PropResp, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmBinaryOutcomePatientSimulation", "SimulatePatientOutcomePercentAtZeroBetaDist.Binary.R",
        "SimulatePatientOutcomePercentAtZeroBetaDist.Binary",
        list( NumSub = NumSub, NumArm = NumArm, ArrivalTime = ArrivalTime, TreatmentID = TreatmentID,
              PropResp = PropResp, UserParam = UserParam )
    ) )
}


#' @name AnalyzeUsingPropTest
#' @title Analyze Two-Arm Binary Outcomes With a Proportion Test
#'
#' @description Calls the proportion-test implementation from the common `2ArmBinaryOutcomeAnalysis` example.
#'
#' @param SimData Data frame containing the simulated patient data.
#' @param DesignParam List of design and simulation parameters.
#' @param LookInfo Optional list describing the current analysis look.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the analysis integration point.
#' @export

AnalyzeUsingPropTest <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmBinaryOutcomeAnalysis", "AnalyzeUsingPropTest.R", "AnalyzeUsingPropTest",
        list( SimData = SimData, DesignParam = DesignParam, LookInfo = LookInfo, UserParam = UserParam )
    ) )
}


#' @name AnalyzeUsingPropLimitsOfCI
#' @title Analyze Binary Outcomes Using Confidence-Interval Limits
#'
#' @description Calls the confidence-interval implementation from the common `2ArmBinaryOutcomeAnalysis` example.
#'
#' @inheritParams AnalyzeUsingPropTest
#' @return A list in the format required by the analysis integration point.
#' @export

AnalyzeUsingPropLimitsOfCI <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmBinaryOutcomeAnalysis", "AnalyzeUsingPropLimitsOfCI.R", "AnalyzeUsingPropLimitsOfCI",
        list( SimData = SimData, DesignParam = DesignParam, LookInfo = LookInfo, UserParam = UserParam )
    ) )
}


#' @name AnalyzeUsingEastManualFormula
#' @title Analyze Binary Outcomes Using the Manual Test-Statistic Formula
#'
#' @description Calls the manual-formula implementation from the common `2ArmBinaryOutcomeAnalysis` example.
#'
#' @inheritParams AnalyzeUsingPropTest
#' @return A list in the format required by the analysis integration point.
#' @export

AnalyzeUsingEastManualFormula <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmBinaryOutcomeAnalysis", "AnalyzeUsingEastManualFormula.R", "AnalyzeUsingEastManualFormula",
        list( SimData = SimData, DesignParam = DesignParam, LookInfo = LookInfo, UserParam = UserParam )
    ) )
}
