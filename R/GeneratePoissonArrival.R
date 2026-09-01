#################################################################################################### .
#   Program/Function Name: GeneratePoissonArrival
#   Description: Generate patient arrival times according to a piecewise Poisson process.
#################################################################################################### .
#' @name GeneratePoissonArrival
#' @title Generate Patient Arrival Times
#'
#' @description Generates patient arrival times according to a Poisson process. When `UserParam` is supplied, its named rates
#' `dRate1`, `dRate2`, and so on define a one-time-unit accrual ramp-up. Otherwise, `PrdStart` and `AccrRate` define the accrual
#' periods and rates.
#'
#' @param NumSub Integer number of subjects to simulate.
#' @param NumPrd Integer number of accrual periods. Retained for compatibility with the arrival integration point.
#' @param PrdStart Numeric vector containing the start time of each accrual period; the first value should be `0`.
#' @param AccrRate Numeric vector containing the accrual rate in each period.
#' @param UserParam Optional list of user-defined rates named `dRate1`, `dRate2`, and so on.
#'
#' @return A list containing `ArrivalTime`, a numeric vector of length `NumSub`, and integer `ErrorCode` equal to `0`.
#'
#' @export
#################################################################################################### .

GeneratePoissonArrival <- function( NumSub, NumPrd, PrdStart, AccrRate, UserParam = NULL )
{
    Error               <- 0
    vPatientArrivalTime <- c()

    if( is.null( UserParam ) )
    {
        vPeriodStartTime <- PrdStart
        vRates           <- AccrRate
        nQtyOfRates      <- length( vRates )
    }
    else
    {
        nQtyOfRates      <- length( UserParam )
        vRates           <- rep( NA, nQtyOfRates )
        vPeriodStartTime <- 0:( nQtyOfRates - 1 )
        for( i in 1:nQtyOfRates )
            vRates[ i ] <- UserParam[[ paste0( "dRate", i ) ]]
    }

    if( NumSub < 1 || nQtyOfRates < 1 || length( vPeriodStartTime ) != nQtyOfRates ||
        any( !is.finite( vRates ) ) || any( vRates <= 0 ) )
        stop( "NumSub and all accrual rates must be positive, and period starts and rates must have equal lengths.", call. = FALSE )

    vPeriodWidth <- c( diff( vPeriodStartTime ), 1 )
    nTimeIndex   <- 1

    while( length( vPatientArrivalTime ) < NumSub )
    {
        vPatientArrivalTime <- c( vPatientArrivalTime,
                                  SimulateAccrualTimesWithConstantRate( vRates[ nTimeIndex ],
                                                                        vPeriodStartTime[ nTimeIndex ],
                                                                        vPeriodWidth[ nTimeIndex ] ) )
        nTimeIndex <- nTimeIndex + 1
        if( nTimeIndex > nQtyOfRates )
        {
            nTimeIndex <- nQtyOfRates
            vPeriodStartTime[ nTimeIndex ] <- vPeriodStartTime[ nTimeIndex ] + 1
        }
    }

    vPatientArrivalTime <- vPatientArrivalTime[ 1:NumSub ]

    return( list( ArrivalTime = as.double( vPatientArrivalTime ), ErrorCode = as.integer( Error ) ) )
}


SimulateAccrualTimesWithConstantRate <- function( dPatsPerUnitTime, dPeriodStartTime, dQtyOfUnitsOfTime = 1 )
{
    nMaxQtyPatsInThisTimeUnit <- stats::qpois( 0.9999, dPatsPerUnitTime ) + 10
    vIntraArrivalTime         <- stats::rexp( dQtyOfUnitsOfTime * nMaxQtyPatsInThisTimeUnit, dPatsPerUnitTime )
    vTimes                    <- cumsum( vIntraArrivalTime )
    vTimes                    <- vTimes[ vTimes < dQtyOfUnitsOfTime ]
    vTimes                    <- vTimes + dPeriodStartTime

    return( vTimes )
}
