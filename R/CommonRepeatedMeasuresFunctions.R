#################################################################################################### .
#   Description: Common repeated-measures response and analysis functions.
#################################################################################################### .
#' @name GenRespDiffOfMeansRepMeasures
#' @title Generate Repeated-Measures Responses
#'
#' @description Calls the response-generation implementation from the common
#' `2ArmNormalRepeatedMeasuresResponseGeneration` example. This function requires the suggested `MASS` package.
#'
#' @param NumSub Integer number of subjects.
#' @param NumVisit Integer number of visits.
#' @param ArrivalTime Numeric subject arrival times.
#' @param TreatmentID Integer treatment identifiers beginning at `0`.
#' @param Inputmethod Integer identifying how response parameters are supplied.
#' @param VisitTime Numeric vector of visit times.
#' @param MeanControl Numeric control-arm means by visit.
#' @param MeanTrt Numeric treatment-arm means by visit.
#' @param StdDevControl Numeric control-arm standard deviations by visit.
#' @param StdDevTrt Numeric treatment-arm standard deviations by visit.
#' @param CorrMat Numeric within-subject correlation matrix.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the response integration point.
#' @export
#################################################################################################### .

GenRespDiffOfMeansRepMeasures <- function( NumSub, NumVisit, ArrivalTime, TreatmentID, Inputmethod, VisitTime,
                                            MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat,
                                            UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmNormalRepeatedMeasuresResponseGeneration", "GenerateResponseDiffOfMeansRepeatedMeasures.R",
        "GenRespDiffOfMeansRepMeasures",
        list( NumSub = NumSub, NumVisit = NumVisit, ArrivalTime = ArrivalTime, TreatmentID = TreatmentID,
              Inputmethod = Inputmethod, VisitTime = VisitTime, MeanControl = MeanControl, MeanTrt = MeanTrt,
              StdDevControl = StdDevControl, StdDevTrt = StdDevTrt, CorrMat = CorrMat,
              UserParam = UserParam )
    ) )
}


#' @name Analyze.RepeatedMeasures
#' @title Analyze Repeated-Measures Outcomes
#'
#' @description Calls the GLS repeated-measures implementation from the common `2ArmNormalRepeatedMeasuresAnalysis` example.
#' This function requires the suggested `nlme` package. Multi-look analyses also require the suggested `rpact` package.
#'
#' @param SimData Data frame containing the simulated repeated-measures data.
#' @param DesignParam List of design and simulation parameters.
#' @param LookInfo Optional list describing the current analysis look.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the analysis integration point.
#' @export

Analyze.RepeatedMeasures <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmNormalRepeatedMeasuresAnalysis", "Analyze.RepeatedMeasures.R", "Analyze.RepeatedMeasures",
        list( SimData = SimData, DesignParam = DesignParam, LookInfo = LookInfo, UserParam = UserParam )
    ) )
}
