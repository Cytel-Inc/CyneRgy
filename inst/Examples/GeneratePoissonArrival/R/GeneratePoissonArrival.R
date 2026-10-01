######################################################################################################################## .
#' @name GeneratePoissonArrival
#'
#' @title Generate patient arrival time according to a Poisson process.
#'
#' @description This function allows for patient arrival time in the clinical trial according to a Poisson process.
#'   If the UserParam is provided then PrdStart and AccrRate are ignored. If the UserParam is supplied, a ramp-up
#'   in accrual is obtained by supplying more than one Rate parameter. The rate is per unit time and the Rate with
#'   the largest index will be used after the ramp up. If UserParam is not supplied, then PrdStart, AccrRate are
#'   used to simulate arrival times according to a Poisson process.
#'
#' @author J. Kyle Wathen
#'
#' @param NumSub Integer number of subjects in the trial.
#'
#' @param Type Integer enrollment type: 0 = global enrollment; 1 = regional enrollment. This function
#'   implements global enrollment and defaults to 0. Use the regional arguments described below for a regional
#'   implementation.
#'
#' @param NumPrd Integer number of accrual periods.
#'
#' @param PrdStart Numeric vector of accrual-period starting times of length NumPrd. The first period starts at 0.
#'
#' @param AccrRate Numeric vector of accrual rates (subjects per unit time), with one element per accrual period.
#'   For regional enrollment, one element per region.
#'
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
#'
#' @return Named list of supported output elements. Return the fields needed by the chosen analysis or generation
#'   method; additional custom outputs may also be included.
#' \describe{
#'   \item{ArrivalTime}{Numeric vector of subject arrival times on the calendar scale, with one element per
#'     subject, in the same order as TreatmentID.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' @details These functions use global enrollment inputs. Regional enrollment can also provide Type (0 = global, 1
#'   = regional), RegionName (character region names), RegionStart (numeric region start times), and
#'   EnrollmentCapPcnt (numeric enrollment caps in percent), with one element per region. Add these engine-supplied
#'   arguments to the function signature when implementing regional enrollment. For global enrollment, use NumPrd,
#'   PrdStart, and AccrRate. Unsupported enrollment types or invalid rate inputs return ErrorCode = -1.
######################################################################################################################## .

GeneratePoissonArrival <- function( NumSub, NumPrd, PrdStart, AccrRate, UserParam = NULL, Type = 0 ) {
    # Step 1 - Initialize the return variables or other variables needed ####
    nErrorCode <- 0
    vPatientArrivalTime <- c( )

    # Step 2 - Validate custom variable input and set defaults ####
    if ( missing( UserParam ) == TRUE || is.null( UserParam ) ) {
        # Step 2.1 - The default will be to use the supplied input NumPrd, PrdStart, AccrRate rather than UserParam

        vPeriodStartTime <- PrdStart
        vRates <- AccrRate
        nQtyOfRates <- length( vRates )
    } else {
        # Step 2.2 - Pull the rates of and create a vector ####
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

    # A zero final rate cannot enroll the requested subjects and would leave the loop running forever.
    if ( Type != 0 || nQtyOfRates == 0 || length( vPeriodStartTime ) != nQtyOfRates ||
         any( !is.finite( vRates ) | vRates < 0 ) || vRates[ nQtyOfRates ] == 0 ||
         any( !is.finite( vPeriodStartTime ) ) || any( diff( vPeriodStartTime ) <= 0 ) ) {
        return( list( ArrivalTime = numeric( 0 ), ErrorCode = -1L ) )
    }

    vPeriodWidth <- c( diff( vPeriodStartTime ), 1 )
    # Step 3 - Loop over the patients and simulate the patient arrival times in the trial ####

    nTimeIndex <- 1
    while ( length( vPatientArrivalTime ) < NumSub ) {
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
    vPatientArrivalTime <- vPatientArrivalTime[ seq_len( NumSub ) ]

    return( list(
        ArrivalTime = as.double( vPatientArrivalTime ),
        ErrorCode = as.integer( nErrorCode )
    ) )
}

SimulateAccrualTimesWithConstantRate <- function( dPatsPerUnitTime, dPeriodStartTime, dQtyOfUnitsOfTime = 1 ) {
    if ( dPatsPerUnitTime == 0 ) {
        return( numeric( 0 ) )
    }

    nMaxQtyPatsInThisTimeUnit <- stats::qpois( 0.9999, dPatsPerUnitTime ) + 10
    vIntraArrivalTime <- stats::rexp( dQtyOfUnitsOfTime * nMaxQtyPatsInThisTimeUnit, dPatsPerUnitTime )

    vTimes <- cumsum( vIntraArrivalTime )
    vTimes <- vTimes[ vTimes < dQtyOfUnitsOfTime ]
    vTimes <- vTimes + dPeriodStartTime

    return( vTimes )
}
