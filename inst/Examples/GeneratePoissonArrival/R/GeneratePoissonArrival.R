######################################################################################################################## .
#' @name GeneratePoissonArrival
#' @title Generate Patient Arrival Times Using a Poisson Process
#' @description Generates patient arrival times using period-specific Poisson accrual rates. When `UserParam` is
#' supplied, its named rates override `PrdStart` and `AccrRate` and provide a ramp-up schedule.
#' @author J. Kyle Wathen
#' @param NumSub Integer number of subjects in the trial.
#' @param NumPrd Integer number of accrual periods.
#' @param PrdStart Numeric vector of length `NumPrd`, indicating the start time of each accrual period; `PrdStart[ 1 ] = 0`.
#' @param AccrRate Numeric vector of length `NumPrd`, indicating the accrual rate in each period.
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'   \describe{
#'     \item{UserParam$dRate1, ..., UserParam$dRateN}{Poisson accrual rate for each successive one-unit period. The final rate continues after period `N`.}
#'   }
#'   When `UserParam` is `NULL`, the function uses `PrdStart` and `AccrRate`.
#' @return A list containing `ArrivalTime`, a numeric vector of length `NumSub`, and `ErrorCode`, an integer status
#' code where 0 indicates success.
######################################################################################################################## .

GeneratePoissonArrival <- function( NumSub, NumPrd, PrdStart, AccrRate, UserParam = NULL )
{
    # Step 1 - Initialize the return variables or other variables needed ####
    nError               <- 0
    vPatientArrivalTime <- c()

    # Step 2 - Validate custom variable input and set defaults ####
    if( missing( UserParam ) == TRUE || is.null( UserParam ) )
    {
        # Step 2.1 - The default will be to use the supplied input NumPrd, PrdStart, AccrRate rather than UserParam

        vPeriodStartTime <- PrdStart
        vRates           <- AccrRate
        nQtyOfRates      <- length( vRates )
    }
    else
    {
        # Step 2.2 - Pull the rates of and create a vector ####
        nQtyOfRates      <- length( UserParam )
        vRates           <- rep( NA, nQtyOfRates )
        vPeriodStartTime <- 0:( nQtyOfRates - 1 )
        for( i in 1:nQtyOfRates )
        {
            vRates[ i ] <- UserParam[[ paste0( "dRate", i ) ] ]
        }
    }

    vPeriodWidth <- c( diff( vPeriodStartTime ), 1 )
    # Step 3 - Loop over the patients and simulate the patient arrival times in the trial ####

    nTimeIndex <- 1
    while( length( vPatientArrivalTime ) < NumSub )
    {
        vPatientArrivalTime <- c(
            vPatientArrivalTime,
            SimulateAccrualTimesWithConstantRate(
                vRates[ nTimeIndex ],
                vPeriodStartTime[ nTimeIndex ],
                vPeriodWidth[ nTimeIndex ]
            )
        )
        nTimeIndex <- nTimeIndex + 1
        if( nTimeIndex > nQtyOfRates )
        {
            nTimeIndex <- nQtyOfRates
            vPeriodStartTime[ nTimeIndex ] <- vPeriodStartTime[ nTimeIndex ] + 1
        }
    }

    # If the last replication generated too many arrival times, retain only those needed.
    vPatientArrivalTime <- vPatientArrivalTime[ 1:NumSub ]

    return( list(
        ArrivalTime = as.double( vPatientArrivalTime ),
        ErrorCode = as.integer( nError )
    ) )
}

SimulateAccrualTimesWithConstantRate <- function( dPatsPerUnitTime, dPeriodStartTime, dQtyOfUnitsOfTime = 1 )
{
    nMaxQtyPatsInThisTimeUnit <- qpois( 0.9999, dPatsPerUnitTime ) + 10
    vIntraArrivalTime <- rexp( dQtyOfUnitsOfTime * nMaxQtyPatsInThisTimeUnit, dPatsPerUnitTime )

    vTimes <- cumsum( vIntraArrivalTime )
    vTimes <- vTimes[ vTimes < dQtyOfUnitsOfTime ]
    vTimes <- vTimes + dPeriodStartTime

    return( vTimes )
}
