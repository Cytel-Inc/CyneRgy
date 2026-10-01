######################################################################################################################## .
#' @name Simulate2EndpointTTEWithMultiState
#' @title Simulate Trial Data for Two Time-to-Event Endpoints Using a Multi-State Model
#' @description This function generates simulated trial data for two time-to-event (TTE) endpoints,
#'   progression-free survival (PFS) and overall survival (OS), using a multi-state model. The simulation utilizes
#'   input parameters such as the number of subjects, number of arms, and user-defined survival parameters.
#' @author Gabriel Potvin, Valeria A. G. Mazzanti, J. Kyle Wathen
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#' @param SurvMethod Integer survival input method: 1 = hazard rates; 2 = cumulative survival percentages; 3 =
#'   median survival times.
#' @param NumPrd Integer number of survival periods. Equals 1 for multi-arm confirmatory designs and stratified
#'   survival generation.
#' @param PrdTime Times used to specify survival parameters: starting times of hazard pieces for SurvMethod = 1;
#'   times at which cumulative survival percentages are specified for SurvMethod = 2; 0 for SurvMethod = 3. Legacy
#'   East Horizon inputs may be vectors; East Horizon inputs may be period-by-arm arrays (stratum-by-arm arrays with
#'   stratification). The control-arm entries may be NA in engine-supplied arrays.
#' @param SurvParam Array of survival parameters with NumPrd rows and NumArm columns, or one row per stratum when
#'   stratification is enabled. Column 1 is control; subsequent columns are experimental arms. Values are hazard
#'   rates for SurvMethod = 1, cumulative survival percentages for SurvMethod = 2, and median survival times for
#'   SurvMethod = 3. Without stratification, the median-survival method has one row.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' Can contain the following named elements:
#'                  \describe{
#'                      \item{UserParam$dMedianPFS0}{Median time to PFS event for the control group.}
#'                      \item{UserParam$dMedianPFS1}{Median time to PFS event for the treatment group.}
#'                      \item{UserParam$dMedianOS0}{Median time to OS event for the control group.}
#'                      \item{UserParam$dMedianOS1}{Median time to OS event for the treatment group.}
#'                      \item{UserParam$dProbOfDeathBeforeProgression0}{Probability of death before PFS for the
#'                        control group.}
#'                      \item{UserParam$dProbOfDeathBeforeProgression1}{Probability of death before PFS for the
#'                        treatment group.}
#'
#'                      \item{UserParam$dMedianPFS0PriorShape}{Shape parameter for the median time to PFS event for
#'                        the control group.}
#'                      \item{UserParam$dMedianPFS0PriorRate}{Rate parameter for the median time to PFS event for
#'                        the control group.}
#'                      \item{UserParam$dMedianOS0PriorShape}{Shape parameter for the median time to OS event for
#'                        the control group.}
#'                      \item{UserParam$dMedianOS0PriorRate}{Rate parameter for the median time to OS event for the
#'                        control group.}
#'                      \item{UserParam$dMedianPFS1PriorShape}{Shape parameter for the median time to PFS event for
#'                        the treatment group.}
#'                      \item{UserParam$dMedianPFS1PriorRate}{Rate parameter for the median time to PFS event for
#'                        the treatment group.}
#'                      \item{UserParam$dMedianOS1PriorShape}{Shape parameter for the median time to OS event for
#'                        the treatment group.}
#'                      \item{UserParam$dMedianOS1PriorRate}{Rate parameter for the median time to OS event for the
#'                        treatment group.}
#'                      \item{UserParam$dProbOfDeathBeforeProgression0Param1}{Alpha parameter for probability of
#'                        death before PFS for the control group.}
#'                      \item{UserParam$dProbOfDeathBeforeProgression0Param2}{Beta parameter for probability of
#'                        death before PFS for the control group.}
#'                      \item{UserParam$dProbOfDeathBeforeProgression1Param1}{Alpha parameter for probability of
#'                        death before PFS for the treatment group.}
#'                      \item{UserParam$dProbOfDeathBeforeProgression1Param2}{Beta parameter for probability of
#'                        death before PFS for the treatment group.}
#'                  }
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{SurvivalTime}{Numeric vector of generated time-to-event outcomes measured from each subject's
#'     enrollment, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' Example-specific additional output elements:
#' \describe{
#'   \item{OS}{Numeric vector of subject overall-survival times measured from enrollment, with one element
#'     per subject.}
#' }
#' @details This example uses the single-survival response integration point: SurvivalTime contains PFS,
#'   and OS is a custom output for the accompanying analysis. NumArm must be 2.
#'
#' Supply one complete set of UserParam fields. The six fixed-value fields specify control and
#'   experimental median PFS, median OS, and probabilities of death before progression. The twelve
#'   prior fields instead specify Gamma shape and rate parameters for the four medians and Beta
#'   shape1 and shape2 parameters for the two death-before-progression probabilities. The sampled
#'   parameters are shared by all subjects on an arm within a simulation. When both sets are supplied,
#'   the fixed-value set takes precedence. All medians and prior parameters must be positive, median OS
#'   must exceed median PFS, and fixed death-before-progression probabilities must lie strictly between
#'   0 and 1. A missing, incomplete, or all-zero parameter set returns ErrorCode = -1 or -2.
#'
#' Prior sampling retries combinations with median OS below median PFS or incompatible transition
#'   rates up to 100 times. ErrorCode values 1 and 2 indicate unsuccessful control-arm sampling or
#'   calibration; values 3 and 4 indicate the corresponding experimental-arm failures. The fixed-value
#'   option returns ErrorCode = 1 when its requested parameters cannot generate finite PFS and OS times.
#'   These positive codes abort only the current simulation. Reset the random seed before direct R
#'   calls when reproducibility is required; the numerical median calibration also uses simulation.
######################################################################################################################## .

Simulate2EndpointTTEWithMultiState <- function( NumSub, NumArm, ArrivalTime, TreatmentID,
                                               SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) {
    # Step 1 - Initialize the return variables or other variables needed ####
    nErrorCode <- 0

    # Step 2 - Validate custom variable input and set defaults ####
    if ( is.null( UserParam ) ) {
        # Return fatal error if no user param
        return( list(
            ErrorCode = as.integer( -1 ),
            SurvivalTime = as.integer( 0 ),
            OS = as.double( 0 )
        ) )
    }

    # Step 3 - Simulate the patient data ####
    # There are two Options:
    vValuesOption1 <- unlist( UserParam[ c(
        "dMedianPFS0", "dMedianOS0", "dProbOfDeathBeforeProgression0",
        "dMedianPFS1", "dMedianOS1", "dProbOfDeathBeforeProgression1"
    ) ], use.names = FALSE )

    vValuesOption2 <- unlist( UserParam[ c(
        "dMedianPFS0PriorShape", "dMedianPFS0PriorRate", "dProbOfDeathBeforeProgression0Param1",
        "dMedianOS0PriorShape", "dMedianOS0PriorRate", "dProbOfDeathBeforeProgression0Param2",
        "dMedianPFS1PriorShape", "dMedianPFS1PriorRate", "dProbOfDeathBeforeProgression1Param1",
        "dMedianOS1PriorShape", "dMedianOS1PriorRate", "dProbOfDeathBeforeProgression1Param2"
    ) ], use.names = FALSE )

    # Option 1: directly input the median times and probabilities of death before progression. In this case, vValuesOption1 are used and vValuesOption2 ignored.
    if ( length( vValuesOption1 ) == 6 && !( all( vValuesOption1 == 0 ) ) ) {
        # User provided values that are fixed for the multistate model
        dMedianPFS0 <- UserParam$dMedianPFS0
        dMedianOS0 <- UserParam$dMedianOS0
        dProbOfDeathBeforeProgression0 <- UserParam$dProbOfDeathBeforeProgression0

        dMedianPFS1 <- UserParam$dMedianPFS1
        dMedianOS1 <- UserParam$dMedianOS1
        dProbOfDeathBeforeProgression1 <- UserParam$dProbOfDeathBeforeProgression1

        vPatsPerArm <- c( sum( TreatmentID == 0 ), sum( TreatmentID == 1 ) )
        dfControlPats <- SimulateDualMultiStateTTE( vPatsPerArm[ 1 ], dMedianPFS0, dMedianOS0, dProbOfDeathBeforeProgression0 )
        dfExpPats <- SimulateDualMultiStateTTE( vPatsPerArm[ 2 ], dMedianPFS1, dMedianOS1, dProbOfDeathBeforeProgression1 )
    }
    # Option 2: customize how patient data is simulated by building a more realistic model for both PFS and OS outcomes
    # using prior distributions. In this case, vValuesOption2 are used and vValuesOption1 are ignored.

    else if ( length( vValuesOption2 ) == 12 && !( all( vValuesOption2 == 0 ) ) ) {
        vPatsPerArm <- c( sum( TreatmentID == 0 ), sum( TreatmentID == 1 ) )

        # First need to sample the prior for control
        dfControlPats <- data.frame( vPFS = NA, vOS = NA )
        nAttempt2 <- 1
        while ( any( is.na( dfControlPats$vPFS ) ) & nAttempt2 <= 100 ) {
            dMedianOS0 <- 1
            dMedianPFS0 <- 2
            nAttempt <- 1
            while ( dMedianOS0 < dMedianPFS0 & nAttempt <= 100 ) {
                dMedianPFS0 <- stats::rgamma( 1, UserParam$dMedianPFS0PriorShape, UserParam$dMedianPFS0PriorRate )
                dMedianOS0 <- stats::rgamma( 1, UserParam$dMedianOS0PriorShape, UserParam$dMedianOS0PriorRate )
                dProbOfDeathBeforeProgression0 <- stats::rbeta( 1, UserParam$dProbOfDeathBeforeProgression0Param1, UserParam$dProbOfDeathBeforeProgression0Param2 )

                nAttempt <- nAttempt + 1
            }
            if ( nAttempt > 100 ) {
                # Error could not sample a OS that is greater than PFS median
                nErrorCode <- 1 # Non-fatal error throw this set out, but if this happens a lot then the user should reconsider the parameters
                return( list(
                    SurvivalTime = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                    OS = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                    ErrorCode = as.integer( nErrorCode )
                ) )
            }

            dfControlPats <- SimulateDualMultiStateTTE( vPatsPerArm[ 1 ], dMedianPFS0, dMedianOS0, dProbOfDeathBeforeProgression0 )
            nAttempt2 <- nAttempt2 + 1
        }

        if ( nAttempt2 > 100 ) {
            # Error could not sample a OS that is greater than PFS median
            nErrorCode <- 2 # Non-fatal error throw this set out, but if this happens a lot then the user should reconsider the parameters
            return( list(
                SurvivalTime = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                OS = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                ErrorCode = as.integer( nErrorCode )
            ) )
        }

        # Sample median PFS, OS and prob  from the experimental arm
        dfExpPats <- data.frame( vPFS = NA, vOS = NA )
        nAttempt2 <- 1
        while ( any( is.na( dfExpPats$vPFS ) ) & nAttempt2 <= 100 ) {
            dMedianOS1 <- 1
            dMedianPFS1 <- 2
            nAttempt <- 1
            while ( dMedianOS1 < dMedianPFS1 & nAttempt <= 100 ) {
                dMedianPFS1 <- stats::rgamma( 1, UserParam$dMedianPFS1PriorShape, UserParam$dMedianPFS1PriorRate )
                dMedianOS1 <- stats::rgamma( 1, UserParam$dMedianOS1PriorShape, UserParam$dMedianOS1PriorRate )
                dProbOfDeathBeforeProgression1 <- stats::rbeta( 1, UserParam$dProbOfDeathBeforeProgression1Param1, UserParam$dProbOfDeathBeforeProgression1Param2 )
                nAttempt <- nAttempt + 1
            }

            if ( nAttempt > 100 ) {
                # Error could not sample a OS that is greater than PFS median
                nErrorCode <- 3 # Non-fatal error throw this set out, but if this happens a lot then the user should reconsider the parameters
                return( list(
                    SurvivalTime = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                    OS = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                    ErrorCode = as.integer( nErrorCode )
                ) )
            }

            dfExpPats <- SimulateDualMultiStateTTE( vPatsPerArm[ 2 ], dMedianPFS1, dMedianOS1, dProbOfDeathBeforeProgression1 )

            nAttempt2 <- nAttempt2 + 1
        }

        if ( nAttempt2 > 100 ) {
            # Error could not sample a OS that is greater than PFS median
            nErrorCode <- 4 # Non-fatal error throw this set out, but if this happens a lot then the user should reconsider the parameters
            return( list(
                SurvivalTime = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                OS = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                ErrorCode = as.integer( nErrorCode )
            ) )
        }
    } else {
        # Return fatal error if UserParam variables are partially present for either option or all are zeros.
        return( list(
            ErrorCode = as.integer( -2 ),
            SurvivalTime = as.integer( 0 ),
            OS = as.double( 0 )
        ) )
    }

    vPFS <- rep( NA, NumSub )
    vOS <- rep( NA, NumSub )

    vPFS[ TreatmentID == 0 ] <- dfControlPats$vPFS
    vPFS[ TreatmentID == 1 ] <- dfExpPats$vPFS

    vOS[ TreatmentID == 0 ] <- dfControlPats$vOS
    vOS[ TreatmentID == 1 ] <- dfExpPats$vOS

    if ( any( !is.finite( vPFS ) ) || any( !is.finite( vOS ) ) ) {
        nErrorCode <- 1L
    }

    return( list( SurvivalTime = as.double( vPFS ), OS = as.double( vOS ), ErrorCode = as.integer( nErrorCode ) ) )
}

# Simulate dual multi-state time-to-event data ####

######################################################################################################################## .
#' @name SimulateDualMultiStateTTE
#' @title Simulate Dual Multi-State Time-to-Event Data
#' @description This function simulates progression-free survival (PFS) and overall survival (OS) using a
#'   multi-state model. Patients can transition between states: progression-free, progression, and death. It uses
#'   exponential distributions to model time-to-event transitions based on specified median survival times and
#'   probabilities of death before progression.
#' @author Gabriel Potvin, Valeria A. G. Mazzanti, J. Kyle Wathen
#' @param nQtyOfPatients Integer number of subjects to simulate.
#' @param dMedianPFS Positive numeric median progression-free survival time (PFS), including progression and death
#'   before progression.
#' @param dMedianOS Positive numeric median overall survival time (OS).
#' @param dProbOfDeathBeforeProgression Numeric probability of death before progression, strictly between 0 and 1.
#' @return A data frame containing two columns:
#'         \describe{
#'             \item{vPFS}{Simulated progression-free survival times.}
#'             \item{vOS}{Simulated overall survival times.}
#'         }
######################################################################################################################## .

SimulateDualMultiStateTTE <- function( nQtyOfPatients, dMedianPFS, dMedianOS, dProbOfDeathBeforeProgression ) {
    if ( nQtyOfPatients == 0 ) {
        return( data.frame( vPFS = numeric( 0 ), vOS = numeric( 0 ) ) )
    }

    # Get alphas using ComputeAlphasForMultiStateModel function
    lAlphas <- ComputeAlphasForMultiStateModel( dMedianPFS, dMedianOS, dProbOfDeathBeforeProgression )

    if ( lAlphas$Error == -1 ) {
        dfRet <- data.frame( vPFS = NA, vOS = NA )
        return( dfRet )
    }

    dAlpha01 <- lAlphas$dRateTimeToProgression
    dAlpha02 <- lAlphas$dRateTimeToDeath
    dAlpha12 <- lAlphas$dRateTimeFromProgressionToDeath

    # Generate time to progression (X1) using alpha1
    vTimeToProgression <- stats::rexp( nQtyOfPatients, dAlpha01 )
    # Generate time to death (X2) using alpha2
    vTimeToDeath <- stats::rexp( nQtyOfPatients, dAlpha02 )
    # Generate time from progression to death (X3) using alpha12
    vTimeFromProgressionToDeath <- stats::rexp( nQtyOfPatients, dAlpha12 )

    # Initialize vectors to capture PFS and OS
    vPFS <- c( )
    vOS <- c( )
    for ( nPatientIndex in seq_len( nQtyOfPatients ) ) {
        if ( vTimeToProgression[ nPatientIndex ] < vTimeToDeath[ nPatientIndex ] ) {
            vPFS <- c( vPFS, vTimeToProgression[ nPatientIndex ] )
            vOS <- c( vOS, vTimeToProgression[ nPatientIndex ] + vTimeFromProgressionToDeath[ nPatientIndex ] )
        } else {
            vPFS <- c( vPFS, vTimeToDeath[ nPatientIndex ] )
            vOS <- c( vOS, vTimeToDeath[ nPatientIndex ] )
        }
    }

    dfRet <- data.frame( vPFS, vOS )
    return( dfRet )
}

# Compute transition rates for the multi-state model ####

######################################################################################################################## .
#' @name ComputeAlphasForMultiStateModel
#' @title Compute Transition Rates for Multi-State Model
#' @description This function calculates transition rates (alphas) for a multi-state model based on input
#'   parameters. The model transitions include time to progression, time to death, and time from progression to
#'   death. The rates are derived from median survival times and the probability of death before progression.
#' @author Gabriel Potvin, Valeria A. G. Mazzanti, J. Kyle Wathen
#' @param dMedianPFS Positive numeric median progression-free survival time (PFS), including progression and death
#'   before progression.
#' @param dMedianOS Positive numeric median overall survival time (OS).
#' @param dProbOfDeathBeforeProgression Numeric probability of death before progression, strictly between 0 and 1.
#' @return A list containing:
#'         \describe{
#'   \item{dAlpha01}{Rate for time to progression.}
#'   \item{dAlpha02}{Rate for time to death without progression.}
#'   \item{dAlpha12}{Rate for time from progression to death.}
#'   \item{Error}{Error code, where 0 indicates success and -1 indicates failure.}
#'   \item{dRateTimeToProgression}{Alias of dAlpha01.}
#'   \item{dRateTimeToDeath}{Alias of dAlpha02.}
#'   \item{dRateTimeFromProgressionToDeath}{Alias of dAlpha12.}
#' }
######################################################################################################################## .

ComputeAlphasForMultiStateModel <- function( dMedianPFS, dMedianOS, dProbOfDeathBeforeProgression ) {
    dMedianProgToDeath <- ComputeMedianProgToDeath( dMedianPFS, dMedianOS, dProbOfDeathBeforeProgression )

    if ( is.na( dMedianProgToDeath ) ) {
        return( list( Error = -1 ) )
    }
    dOneMinusPDivP <- ( ( 1 - dProbOfDeathBeforeProgression ) / dProbOfDeathBeforeProgression )
    dAlpha02 <- log( 2 ) / ( dMedianPFS * ( dOneMinusPDivP + 1 ) )
    dAlpha01 <- dOneMinusPDivP * dAlpha02
    dAlpha12 <- log( 2 ) / dMedianProgToDeath

    lRet <- list(
        dAlpha01 = dAlpha01,
        dAlpha02 = dAlpha02,
        dAlpha12 = dAlpha12,
        dRateTimeToProgression = dAlpha01,
        dRateTimeToDeath = dAlpha02,
        dRateTimeFromProgressionToDeath = dAlpha12,
        Error = 0
    )
    return( lRet ) # Use an explicit return
}

# Compute median time from progression to death ####

######################################################################################################################## .
#' @name ComputeMedianProgToDeath
#' @title Compute Median Time from Progression to Death
#' @description This function computes the median time from progression to death in a multi-state model. It uses
#'   input parameters such as median progression-free survival (PFS), median overall survival (OS), and the
#'   probability of death before progression to derive the median progression-to-death survival time.
#' @author Gabriel Potvin, Valeria A. G. Mazzanti, J. Kyle Wathen
#' @param dMedianPFS Positive numeric median progression-free survival time (PFS), including progression and death
#'   before progression.
#' @param dMedianOS Positive numeric median overall survival time (OS).
#' @param dProbDeathB4Prog Numeric probability of death before progression, strictly between 0 and 1.
#' @return Numeric value representing the median progression-to-death time. Returns `NA` if computation fails.
######################################################################################################################## .

ComputeMedianProgToDeath <- function( dMedianPFS, dMedianOS, dProbDeathB4Prog ) {
    dMedianProgToDeath <- NA

    f <- function( x, dMedianPFS ) {
        return( ComputeMedianOS( dMedianPFS, x, dProbDeathB4Prog ) - dMedianOS )
    }
    tryCatch(
        {
            dMedianProgToDeath <- stats::uniroot( f, lower = 0.01, upper = dMedianOS, dMedianPFS = dMedianPFS )$root
        },
        error = function( e ) {
            dMedianProgToDeath <- NA
            return( dMedianProgToDeath )
        }
    )

    return( dMedianProgToDeath )
}

# Compute median overall survival using simulated data ####

######################################################################################################################## .
#' @name ComputeMedianOS
#' @title Compute Median Overall Survival Using Simulated Data
#' @description This function simulates progression-free survival (PFS) and overall survival (OS) times for a large
#'   number of patients. It calculates the median overall survival based on these simulations. The function uses
#'   specified median PFS, median progression-to-death times, and the probability of death before progression to
#'   simulate survival times.
#' @author Gabriel Potvin, Valeria A. G. Mazzanti, J. Kyle Wathen
#' @param dMedianPFS Positive numeric median progression-free survival time (PFS), including progression and death
#'   before progression.
#' @param dMedianProgToDeath Positive numeric median time from progression to death.
#' @param dProbDeathB4Prog Numeric probability of death before progression, strictly between 0 and 1.
#' @return Numeric value representing the median overall survival (OS).
######################################################################################################################## .

ComputeMedianOS <- function( dMedianPFS, dMedianProgToDeath, dProbDeathB4Prog ) {
    nPatients <- 10000

    vPFS <- stats::rexp( nPatients, log( 2 ) / dMedianPFS )
    vOS <- vPFS + stats::rexp( nPatients, log( 2 ) / dMedianProgToDeath )
    vDeathB4Prog <- stats::rbinom( nPatients, 1, dProbDeathB4Prog )
    vOS <- ifelse( vDeathB4Prog == 1, vPFS, vOS )

    dMedianOS <- stats::median( vOS )
    return( dMedianOS )
}
