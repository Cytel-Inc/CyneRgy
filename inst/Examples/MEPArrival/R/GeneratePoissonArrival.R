#' @name GeneratePoissonArrival
#' @title Generate patient arrival time according to a Poisson process.
#' @param NumPat The number of participants that need to be simulated, integer value
#' @param NumPrd Number of time periods that are provided.
#' @param PrdStart Vector with start of a time interval, PrdStarr[ 1 ] = 0
#' @param AccrRate the accrual rate in each period.
#' @param UserParam A list of user defined parameters that may be provided in East or East Horizon. You must have a default of NULL, as in this example.
#' The user may supplies rates names Rate1, Rate2, ...., RateX to represent the per unit time accrual rate where the maximum RateX is used after the ramp-up.
#'    \describe{
#'      \item{Rate1}{The rate in the first unit of time}
#'      \item{Rate2}{The rate in the first second of time}
#'    }
#' @description
#' This function allows for patient arrival time in the clinical trial according to a Poisson process.  If the UserParam is provided then PrdStart and AccrRate are ignored.
#' If the UserParam is supplied, a ramp-up in accrual is obtained by
#' supplying more than one Rate parameter. The rate is per unit time and the Rate with the largest index will be used after the ramp up.
#' If UserParam is not supplied, then PrdStart, AccrRate are used to simulate arrival times according to a Poisson process.
GeneratePoissonArrival  <- function(NumPat, NumPrd, PrdStart, AccrRate, UserParam = NULL )
{
    # Error = 0 --> No Error;
    # Error > 0 --> Non Fatal Error Particular Simulation will be aborted but Next Simulation will be performed
    # Error < 0 --> Fatal Error - No further simulation will be attempted. We suggest that user should classify error in these categories depending on the context.
    # Step 1 - Initialize the return variables or other variables needed ####
    Error 	            <- 0
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
        vPeriodStartTime <- 0:(nQtyOfRates - 1)
        for( i in 1:nQtyOfRates )
        {
            vRates[ i ] <- UserParam[[ paste0( "dRate", i ) ]]
        }
    }

    vPeriodWidth <- c( diff( vPeriodStartTime ), 1)   # The 1 will assume to simulate 1 unit of time after the periods run out
    # Step 3 - Loop over the patients and simulate the patient arrival times in the trial ####

    nTimeIndex        <- 1
    while( length( vPatientArrivalTime ) < NumPat )
    {
        vPatientArrivalTime <- c( vPatientArrivalTime, SimulateAccrualTimesWithConstantRate( vRates[ nTimeIndex ], vPeriodStartTime[ nTimeIndex ], vPeriodWidth[ nTimeIndex ] ) )
        nTimeIndex  <- nTimeIndex + 1
        if( nTimeIndex > nQtyOfRates )
        {
            nTimeIndex <- nQtyOfRates
            vPeriodStartTime[ nTimeIndex ] <- vPeriodStartTime[ nTimeIndex ] + 1
        }
    }

    # If the last rep generated too many arrival times, subset to only what we need.
    vPatientArrivalTime <- vPatientArrivalTime[ 1:NumPat]




	return(list(ArrivalTime = as.double(vPatientArrivalTime), nQtyOfRates=rep(nQtyOfRates,NumPat), ErrorCode =as.integer(Error)))
}



SimulateAccrualTimesWithConstantRate <- function( dPatsPerUnitTime , dPeriodStartTime, dQtyOfUnitsOfTime = 1  )
{
    nMaxQtyPatsInThisTimeUnit <- qpois(0.9999,dPatsPerUnitTime)+10
    vIntraArrivalTime         <- rexp( dQtyOfUnitsOfTime * nMaxQtyPatsInThisTimeUnit, dPatsPerUnitTime)

    vTimes <- cumsum( vIntraArrivalTime )
    vTimes <- vTimes[ vTimes < dQtyOfUnitsOfTime ]
    vTimes <- vTimes + dPeriodStartTime
    return( vTimes )

}


