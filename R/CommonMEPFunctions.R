#################################################################################################### .
#   Description: Common multiple-endpoint arrival, simulation, and decision functions.
#################################################################################################### .


#' @name GeneratePoissonArrivalMEP
#' @title Generate Patient Arrival Times for an MEP Design
#'
#' @description Calls the multiple-endpoint implementation from the common `GeneratePoissonArrival` example.
#'
#' @param NumPat Integer number of patients.
#' @param NumPrd Integer number of accrual periods.
#' @param PrdStart Numeric period start times.
#' @param AccrRate Numeric accrual rates by period.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the arrival integration point.
#' @export

GeneratePoissonArrivalMEP <- function( NumPat, NumPrd, PrdStart, AccrRate, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "GeneratePoissonArrival", "GeneratePoissonArrivalMEP.R", "GeneratePoissonArrivalMEP",
        list( NumPat = NumPat, NumPrd = NumPrd, PrdStart = PrdStart, AccrRate = AccrRate,
              UserParam = UserParam )
    ) )
}


#' @name GenerateMEPResponse
#' @title Generate Correlated Multiple-Endpoint Responses
#'
#' @description Calls the response implementation from the common `MEPPatientSimulation` example.
#'
#' @param NumPat Integer number of patients.
#' @param NumArms Integer number of trial arms.
#' @param TreatmentID Integer treatment identifiers beginning at `0`.
#' @param ArrivalTime Numeric subject arrival times.
#' @param EndpointType Endpoint type identifiers.
#' @param EndpointName Endpoint names.
#' @param RespParams Response parameters arranged by endpoint and arm.
#' @param Correlation Numeric endpoint correlation inputs.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the MEP response integration point.
#' @export

GenerateMEPResponse <- function( NumPat, NumArms, TreatmentID, ArrivalTime, EndpointType, EndpointName,
                                 RespParams, Correlation, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "MEPPatientSimulation", "GenerateMEPResponse.R", "GenerateMEPResponse",
        list( NumPat = NumPat, NumArms = NumArms, TreatmentID = TreatmentID, ArrivalTime = ArrivalTime,
              EndpointType = EndpointType, EndpointName = EndpointName, RespParams = RespParams,
              Correlation = Correlation, UserParam = UserParam )
    ) )
}


#' @name GetMEPDecision
#' @title Generate Multiple-Endpoint Decisions
#'
#' @description Calls the decision implementation from the common `MEPDesign` example.
#'
#' @param SimData Data frame containing the simulated patient data.
#' @param AnalysisData Analysis data supplied to the design integration point.
#' @param DataSummary Endpoint data summaries.
#' @param LookInfo List describing the current analysis look.
#' @param DesignParam List of design and simulation parameters.
#' @param OutList Output list from the analysis step.
#' @param UserParam List of user-defined parameters.
#'
#' @return A list in the format required by the MEP design integration point.
#' @export

GetMEPDecision <- function( SimData, AnalysisData, DataSummary, LookInfo, DesignParam, OutList, UserParam )
{
    return( .CallCommonExampleFunction(
        "MEPDesign", "GetMEPDecision.R", "GetMEPDecision",
        list( SimData = SimData, AnalysisData = AnalysisData, DataSummary = DataSummary,
              LookInfo = LookInfo, DesignParam = DesignParam, OutList = OutList, UserParam = UserParam )
    ) )
}
