######################################################################################################################## .
#' @name GeneratePoissonArrivalMEP
#' @title Generate patient arrival time according to a Poisson process.
#' @description Simulate patient arrival times in a clinical trial according to a Poisson process. When UserParam
#'   is supplied, its consecutively numbered dRate1 through dRateN parameters specify accrual rates for successive
#'   units of time. Each rate is per unit time, and the final rate continues after the ramp-up. When UserParam is
#'   NULL, PrdStart and AccrRate define the accrual schedule.
#' @author J. Kyle Wathen
#' @param NumPat Integer number of subjects in the trial.
#' @param Type Integer enrollment type. This function supports global enrollment (0), the default.
#' @param NumPrd Integer number of accrual periods.
#' @param PrdStart Numeric vector of accrual-period starting times of length NumPrd. The first period starts at 0.
#' @param AccrRate Numeric vector of accrual rates (subjects per unit time), with one element per accrual period.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' \describe{
#'   \item{dRate1}{Numeric accrual rate (subjects per unit time) during the first unit of time.}
#'   \item{dRate2}{Numeric accrual rate (subjects per unit time) during the second unit of time.}
#'   \item{dRateN}{Additional consecutively numbered rates for later units of time. Supply dRate1 through dRateN
#'     without gaps; the final rate continues after the ramp-up. Rates must be nonnegative, with a positive
#'     final rate to ensure that all requested subjects can enroll.}
#' }
#' @return Named list containing the outputs returned by this example.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{nQtyOfRates}{Integer vector repeating the number of accrual rates for each subject; an example-specific
#'     custom output.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details This function uses the global enrollment inputs NumPrd, PrdStart, and AccrRate.
#'   Invalid custom rate inputs return ErrorCode = -1.

######################################################################################################################## .

GeneratePoissonArrivalMEP <- function( NumPat, NumPrd, PrdStart, AccrRate, UserParam = NULL, Type = 0 ) {
    # Error = 0 --> No Error;
    # Error > 0 --> Nonfatal error; the current simulation will be aborted, but the next simulation will run
    # Error < 0 --> Fatal Error - No further simulation will be attempted. We suggest that user should classify error in
    #   these categories depending on the context.
    # Step 1 - Initialize the return variables or other variables needed ####
    nErrorCode <- 0
    vPatientArrivalTime <- c( ) # Store the simulated patient arrival times in this vector.

    # Step 2 - Validate custom variable input and set defaults ####
    if ( missing( UserParam ) == TRUE || is.null( UserParam ) ) {
        # Step 2.1 - The default will be to use the supplied input NumPrd, PrdStart, AccrRate rather than UserParam

        vPeriodStartTime <- PrdStart
        vRates <- AccrRate
        nQtyOfRates <- length( vRates )
    } else {
        # Step 2.2 - Read the named rates and create a vector ####
        nQtyOfRates <- length( UserParam )
        vExpectedRateNames <- paste0( "dRate", seq_len( nQtyOfRates ) )
        if ( nQtyOfRates == 0 || !setequal( names( UserParam ), vExpectedRateNames ) ) {
            return( list( ArrivalTime = numeric( 0 ), ErrorCode = -1L ) )
        }
        vRates <- rep( NA, nQtyOfRates )
        vPeriodStartTime <- 0:( nQtyOfRates - 1 )
        for ( nRateIndex in seq_len( nQtyOfRates ) ) {
            vRates[ nRateIndex ] <- UserParam[[ paste0( "dRate", nRateIndex ) ]]
        }
    }

    if ( !is.null( UserParam ) &&
         ( any( !is.finite( vRates ) | vRates < 0 ) || vRates[ nQtyOfRates ] == 0 ) ) {
        return( list( ArrivalTime = numeric( 0 ), ErrorCode = -1L ) )
    }

    vPeriodWidth <- c( diff( vPeriodStartTime ), 1 )
    # Step 3 - Loop over the patients and simulate the patient arrival times in the trial ####

    nTimeIndex <- 1
    while ( length( vPatientArrivalTime ) < NumPat ) {
        vPatientArrivalTime <- c(
            vPatientArrivalTime,
            SimulateAccrualTimesWithConstantRate(
                vRates[ nTimeIndex ],
                vPeriodStartTime[ nTimeIndex ],
                vPeriodWidth[ nTimeIndex ]
            )
        )
        nTimeIndex <- nTimeIndex + 1
        if ( nTimeIndex > nQtyOfRates ) {
            nTimeIndex <- nQtyOfRates
            vPeriodStartTime[ nTimeIndex ] <- vPeriodStartTime[ nTimeIndex ] + 1
        }
    }

    # If the last replication generated too many arrival times, retain only those needed.
    vPatientArrivalTime <- vPatientArrivalTime[ seq_len( NumPat ) ]

    return( list(
        ArrivalTime = as.double( vPatientArrivalTime ),
        nQtyOfRates = rep( nQtyOfRates, NumPat ),
        ErrorCode = as.integer( nErrorCode )
    ) )
}

SimulateAccrualTimesWithConstantRate <- function( dPatsPerUnitTime, dPeriodStartTime, dQtyOfUnitsOfTime = 1 ) {
    if ( dPatsPerUnitTime == 0 ) {
        return( numeric( 0 ) )
    }

    # Conditional on the Poisson count, arrivals are sorted uniform times within this period.
    nSubjectsInPeriod <- stats::rpois( 1, dPatsPerUnitTime * dQtyOfUnitsOfTime )
    vTimes <- sort( stats::runif( nSubjectsInPeriod, min = 0, max = dQtyOfUnitsOfTime ) )
    vTimes <- vTimes + dPeriodStartTime

    return( vTimes )
}
