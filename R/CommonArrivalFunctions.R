#################################################################################################### .
#   Description: Common patient arrival functions.
#################################################################################################### .
#' @name GeneratePoissonArrival
#' @title Generate Patient Arrival Times
#'
#' @description Calls the implementation from the common `GeneratePoissonArrival` example. Generates patient arrival times
#' according to a Poisson process. When `UserParam` is supplied, its named rates `dRate1`, `dRate2`, and so on define a
#' one-time-unit accrual ramp-up. Otherwise, `PrdStart` and `AccrRate` define the accrual periods and rates.
#'
#' @param NumSub Integer number of subjects to simulate.
#' @param NumPrd Integer number of accrual periods.
#' @param PrdStart Numeric vector containing the start time of each accrual period; the first value should be `0`.
#' @param AccrRate Numeric vector containing the accrual rate in each period.
#' @param UserParam Optional list of user-defined rates named `dRate1`, `dRate2`, and so on.
#'
#' @return A list in the format required by the arrival integration point.
#' @export
#################################################################################################### .

GeneratePoissonArrival <- function( NumSub, NumPrd, PrdStart, AccrRate, UserParam = NULL )
{
    return( .CallCommonExampleFunction(
        "GeneratePoissonArrival", "GeneratePoissonArrival.R", "GeneratePoissonArrival",
        list( NumSub = NumSub, NumPrd = NumPrd, PrdStart = PrdStart, AccrRate = AccrRate, UserParam = UserParam )
    ) )
}


# Internal helper mirroring the bundled GeneratePoissonArrival example, kept here for direct unit testing.
SimulateAccrualTimesWithConstantRate <- function( dPatsPerUnitTime, dPeriodStartTime, dQtyOfUnitsOfTime = 1 )
{
    nMaxQtyPatsInThisTimeUnit <- stats::qpois( 0.9999, dPatsPerUnitTime ) + 10
    vIntraArrivalTime         <- stats::rexp( dQtyOfUnitsOfTime * nMaxQtyPatsInThisTimeUnit, dPatsPerUnitTime )
    vTimes                    <- cumsum( vIntraArrivalTime )
    vTimes                    <- vTimes[ vTimes < dQtyOfUnitsOfTime ]
    vTimes                    <- vTimes + dPeriodStartTime

    return( vTimes )
}
