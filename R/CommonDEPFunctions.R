#################################################################################################### .
#   Description: Common dual-endpoint simulation, analysis, and decision functions.
#################################################################################################### .
#' @name AnalyzeDEPUsingFisherExact
#' @title Analyze Survival and Binary Dual Endpoints
#'
#' @description Calls the Fisher-exact implementation from the common `DEPAnalysis` example.
#'
#' @param SimData Data frame containing the simulated dual-endpoint patient data.
#' @param DesignParam List of design and simulation parameters.
#' @param LookInfo Optional list describing the current analysis look.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the DEP analysis integration point.
#' @export
#################################################################################################### .

AnalyzeDEPUsingFisherExact <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "DEPAnalysis", "AnalyzeDEPUsingFisherExact.R", "AnalyzeDEPUsingFisherExact",
        list( SimData = SimData, DesignParam = DesignParam, LookInfo = LookInfo, UserParam = UserParam )
    ) )
}


#' @name AnalyzeDEPUsingModWtLogRank
#' @title Analyze Survival Dual Endpoints With a Modestly Weighted Log-Rank Test
#'
#' @description Calls the modestly weighted log-rank implementation from the common `DEPAnalysis` example. This function requires
#' the suggested `survival` package.
#'
#' @inheritParams AnalyzeDEPUsingFisherExact
#' @return A list in the format required by the DEP analysis integration point.
#' @export

AnalyzeDEPUsingModWtLogRank <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "DEPAnalysis", "AnalyzeDEPUsingModWtLogRank.R", "AnalyzeDEPUsingModWtLogRank",
        list( SimData = SimData, DesignParam = DesignParam, LookInfo = LookInfo, UserParam = UserParam )
    ) )
}


#' @name GetDEPDecisionsFSD
#' @title Generate Dual-Endpoint Decisions
#'
#' @description Calls the fixed-sequence decision implementation from the common `DEPDecisionsUsingMCP` example.
#'
#' @param SimData Data frame containing the simulated patient data.
#' @param DesignParam List of design and simulation parameters.
#' @param LookInfo Optional list describing the current analysis look.
#' @param TestStat Numeric endpoint test statistics.
#' @param OutList Optional output list from the analysis step.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the DEP decision integration point.
#' @export

GetDEPDecisionsFSD <- function( SimData, DesignParam, LookInfo = NULL, TestStat, OutList = NULL, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "DEPDecisionsUsingMCP", "GetDEPDecisionsFSD.R", "GetDEPDecisionsFSD",
        list( SimData = SimData, DesignParam = DesignParam, LookInfo = LookInfo, TestStat = TestStat,
              OutList = OutList, UserParam = UserParam )
    ) )
}


#' @name SimulatePatientOutcomeDEPSurvBinSingleHazardPiece
#' @title Simulate Correlated Survival and Binary Dual Endpoints
#'
#' @description Calls the survival/binary implementation from the common `DEPPatientSimulation` example.
#'
#' @param NumSub Integer number of subjects.
#' @param NumArm Integer number of trial arms.
#' @param ArrivalTime Numeric subject arrival times.
#' @param TreatmentID Integer treatment identifiers beginning at `0`.
#' @param EndpointType Endpoint type identifiers.
#' @param EndpointName Endpoint names.
#' @param Correlation Numeric endpoint correlation inputs.
#' @param SurvMethod Integer identifying how survival parameters are supplied.
#' @param NumPrd Integer number of survival periods.
#' @param PrdTime Numeric period start times.
#' @param SurvParam Numeric survival parameters.
#' @param PropResp Optional numeric binary response probabilities.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the DEP response integration point.
#' @export

SimulatePatientOutcomeDEPSurvBinSingleHazardPiece <- function( NumSub, NumArm, ArrivalTime = NULL, TreatmentID,
                                                               EndpointType, EndpointName, Correlation, SurvMethod,
                                                               NumPrd, PrdTime, SurvParam, PropResp = NULL,
                                                               UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "DEPPatientSimulation", "SimulatePatientOutcomeDEPSurvBinSingleHazardPiece.R",
        "SimulatePatientOutcomeDEPSurvBinSingleHazardPiece",
        list( NumSub = NumSub, NumArm = NumArm, ArrivalTime = ArrivalTime, TreatmentID = TreatmentID,
              EndpointType = EndpointType, EndpointName = EndpointName, Correlation = Correlation,
              SurvMethod = SurvMethod, NumPrd = NumPrd, PrdTime = PrdTime, SurvParam = SurvParam,
              PropResp = PropResp, UserParam = UserParam )
    ) )
}


#' @name SimulatePatientOutcomeDEPSurvSurvSingleHazardPiece
#' @title Simulate Correlated Survival Dual Endpoints
#'
#' @description Calls the survival/survival implementation from the common `DEPPatientSimulation` example.
#'
#' @inheritParams SimulatePatientOutcomeDEPSurvBinSingleHazardPiece
#' @return A list in the format required by the DEP response integration point.
#' @export

SimulatePatientOutcomeDEPSurvSurvSingleHazardPiece <- function( NumSub, NumArm, ArrivalTime = NULL, TreatmentID,
                                                                EndpointType, EndpointName, Correlation, SurvMethod,
                                                                NumPrd, PrdTime, SurvParam, PropResp = NULL,
                                                                UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "DEPPatientSimulation", "SimulatePatientOutcomeDEPSurvSurvSingleHazardPiece.R",
        "SimulatePatientOutcomeDEPSurvSurvSingleHazardPiece",
        list( NumSub = NumSub, NumArm = NumArm, ArrivalTime = ArrivalTime, TreatmentID = TreatmentID,
              EndpointType = EndpointType, EndpointName = EndpointName, Correlation = Correlation,
              SurvMethod = SurvMethod, NumPrd = NumPrd, PrdTime = PrdTime, SurvParam = SurvParam,
              PropResp = PropResp, UserParam = UserParam )
    ) )
}
