#################################################################################################### .
#   Description: Common dropout functions for binary, continuous, repeated measures, and time-to-event endpoints.
#################################################################################################### .


#' @name GenerateDropoutTimeForRM
#' @title Generate Dropout Times for Repeated Measures Outcomes
#'
#' @description Calls the implementation from the common `2ArmPatientDropout` example.
#'
#' @param NumSub Integer number of subjects.
#' @param NumArm Integer number of trial arms.
#' @param NumVisit Integer number of visits.
#' @param VisitTime Numeric vector of visit times.
#' @param TreatmentID Integer treatment identifiers beginning at `0`.
#' @param DropMethod Integer identifying the dropout model.
#' @param ByTime Logical or integer indicator for time-specific dropout inputs.
#' @param DropParamControl Numeric control-arm dropout parameters.
#' @param DropParamTrt Numeric treatment-arm dropout parameters.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the dropout integration point.
#' @export

GenerateDropoutTimeForRM <- function( NumSub, NumArm, NumVisit, VisitTime, TreatmentID, DropMethod, ByTime,
                                      DropParamControl, DropParamTrt, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmPatientDropout", "GenerateDropoutTimeForRM.R", "GenerateDropoutTimeForRM",
        list( NumSub = NumSub, NumArm = NumArm, NumVisit = NumVisit, VisitTime = VisitTime,
              TreatmentID = TreatmentID, DropMethod = DropMethod, ByTime = ByTime,
              DropParamControl = DropParamControl, DropParamTrt = DropParamTrt, UserParam = UserParam )
    ) )
}


#' @name GenerateDropoutTimeForSurvival
#' @title Generate Dropout Times for Survival Outcomes
#'
#' @description Calls the survival implementation from the common `2ArmPatientDropout` example.
#'
#' @param NumSub Integer number of subjects.
#' @param NumArm Integer number of trial arms.
#' @param TreatmentID Integer treatment identifiers beginning at `0`.
#' @param DropMethod Integer identifying the dropout model.
#' @param NumPrd Integer number of dropout periods.
#' @param PrdTime Numeric period start times.
#' @param DropParam Numeric dropout parameters.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the dropout integration point.
#' @export

GenerateDropoutTimeForSurvival <- function( NumSub, NumArm, TreatmentID, DropMethod, NumPrd, PrdTime,
                                            DropParam, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "2ArmPatientDropout", "GenerateDropoutTimeForSurvival.R", "GenerateDropoutTimeForSurvival",
        list( NumSub = NumSub, NumArm = NumArm, TreatmentID = TreatmentID, DropMethod = DropMethod,
              NumPrd = NumPrd, PrdTime = PrdTime, DropParam = DropParam, UserParam = UserParam )
    ) )
}


#' @name GenerateCensoringUsingBinomialProportion
#' @title Generate Dropout Indicators for Binary or Continuous Outcomes
#'
#' @description Calls the implementation from the common `2ArmPatientDropout` example. Generates an independent censoring
#' indicator for each subject in binary or continuous outcome designs using one dropout probability. A value of `1` indicates
#' a completer and `0` indicates a dropout.
#'
#' @param NumSub Integer number of subjects.
#' @param ProbDrop Numeric dropout probability shared by both arms.
#' @param UserParam Optional list of user-defined parameters.
#'
#' @return A list in the format required by the dropout integration point.
#' @export

GenerateCensoringUsingBinomialProportion <- function( NumSub, ProbDrop, UserParam = NULL )
{
    if( NumSub < 1 || length( ProbDrop ) != 1 || !is.finite( ProbDrop ) || ProbDrop < 0 || ProbDrop > 1 )
        stop( "NumSub must be positive and ProbDrop must be between 0 and 1.", call. = FALSE )

    return( .CallCommonExampleFunction(
        "2ArmPatientDropout", "GenerateCensoringUsingBinomialProportion.R", "GenerateCensoringUsingBinomialProportion",
        list( NumSub = NumSub, ProbDrop = ProbDrop, UserParam = UserParam )
    ) )
}
