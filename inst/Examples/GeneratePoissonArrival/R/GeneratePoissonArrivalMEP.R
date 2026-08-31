######################################################################################################################## .
#' @name GeneratePoissonArrivalMEP
#' @title Generate Multi-Endpoint Patient Arrival Times Using a Poisson Process
#' @description Generates patient arrival times using period-specific Poisson accrual rates. When `UserParam` is
#' supplied, its named rates override `PrdStart` and `AccrRate` and provide a ramp-up schedule.
#' @author J. Kyle Wathen
#' @param NumPat Integer. Number of patients to simulate.
#' @param NumPrd Integer. Number of accrual periods.
#' @param PrdStart Numeric vector containing the start time of each accrual period. The first value must be 0.
#' @param AccrRate Numeric vector containing the accrual rate for each period.
#' @param UserParam Optional list of user-defined accrual rates named `dRate1`, `dRate2`, and so on. The rate with
#' the largest index is used after the ramp-up. Defaults to `NULL`.
#' @return A list containing `ArrivalTime`, a numeric vector of length `NumPat`; `nQtyOfRates`, an integer vector
#' reporting the number of rates; and `ErrorCode`, an integer status code where 0 indicates success.
######################################################################################################################## .

GeneratePoissonArrivalMEP <- function( NumPat, NumPrd, PrdStart, AccrRate, UserParam = NULL )
{
    # Error = 0 --> No Error;
    # Error > 0 --> Nonfatal error; the current simulation will be aborted, but the next simulation will run
    # Error < 0 --> Fatal Error - No further simulation will be attempted. We suggest that user should classify error in these categories depending on the context.
    # Step 1 - Initialize the return variables or other variables needed ####
    nError               <- 0
    vPatientArrivalTime <- c() # Note, as you simulate the patient data put in in this vector so it can be returned

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
    while( length( vPatientArrivalTime ) < NumPat )
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
    vPatientArrivalTime <- vPatientArrivalTime[ 1:NumPat ]

    return( list(
        ArrivalTime = as.double( vPatientArrivalTime ),
        nQtyOfRates = rep( nQtyOfRates, NumPat ),
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
