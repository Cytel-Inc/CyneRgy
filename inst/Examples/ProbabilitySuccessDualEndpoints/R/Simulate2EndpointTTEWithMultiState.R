######################################################################################################################## .
#' @name Simulate2EndpointTTEWithMultiState
#' @title Simulate Trial Data for Two Time-to-Event Endpoints Using a Multi-State Model
#' @description
#' This function generates simulated trial data for two time-to-event (TTE) endpoints, progression-free survival (PFS)
#' and overall survival (OS), using a multi-state model. The simulation utilizes input parameters such as the number
#' of subjects, number of arms, and user-defined survival parameters.
#' @author Gabriel Potvin, Valeria A. G. Mazzanti, J. Kyle Wathen
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArm Integer number of arms in the trial, including placebo/control and experimental arms.
#' @param ArrivalTime Numeric vector of length `NumSub`, indicating the arrival time for each subject.
#' @param TreatmentID Integer vector of length `NumSub`, indicating subject allocation to trial arms. Index `0` represents placebo/control; indices `1` and above represent experimental arms.
#'                    The length of this vector must equal NumSub.
#' @param SurvMethod A numeric value specifying the survival method:
#'                   \describe{
#'                       \item{1}{Hazard Rate}
#'                       \item{2}{Cumulative \% Survival}
#'                       \item{3}{Medians}
#'                   }
#' @param NumPrd The number of time periods that are provided.
#' @param PrdTime A vector defining the time periods:
#'                \describe{
#'                    \item{If SurvMethod = 1}{Start times of hazard pieces.}
#'                    \item{If SurvMethod = 2}{Times at which cumulative survival percentages are specified.}
#'                    \item{If SurvMethod = 3}{Defaults to 0.}
#'                }
#' @param SurvParam A 2-D array of survival parameters:
#'                  \describe{
#'                      \item{If SurvMethod = 1}{A NumPrd x NumArm array specifying hazard rates for each arm and time period.}
#'                      \item{If SurvMethod = 2}{A NumPrd x NumArm array specifying cumulative survival percentages for each arm and time period.}
#'                      \item{If SurvMethod = 3}{A 1x2 array specifying median survival times for each arm (control in column 1, experimental in column 2).}
#'                  }
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'                  \describe{
#'                      \item{UserParam$dMedianPFS0}{Median time to PFS event for the control group.}
#'                      \item{UserParam$dMedianPFS1}{Median time to PFS event for the treatment group.}
#'                      \item{UserParam$dMedianOS0}{Median time to OS event for the control group.}
#'                      \item{UserParam$dMedianOS1}{Median time to OS event for the treatment group.}
#'                      \item{UserParam$dProbOfDeathBeforeProgression0}{Probability of death before PFS for the control group.}
#'                      \item{UserParam$dProbOfDeathBeforeProgression1}{Probability of death before PFS for the treatment group.}
#'
#'                      \item{UserParam$dMedianPFS0PriorShape}{Shape parameter for the median time to PFS event for the control group.}
#'                      \item{UserParam$dMedianPFS0PriorRate}{Rate parameter for the median time to PFS event for the control group.}
#'                      \item{UserParam$dMedianOS0PriorShape}{Shape parameter for the median time to OS event for the control group.}
#'                      \item{UserParam$dMedianOS0PriorRate}{Rate parameter for the median time to OS event for the control group.}
#'                      \item{UserParam$dMedianPFS1PriorShape}{Shape parameter for the median time to PFS event for the treatment group.}
#'                      \item{UserParam$dMedianPFS1PriorRate}{Rate parameter for the median time to PFS event for the treatment group.}
#'                      \item{UserParam$dMedianOS1PriorShape}{Shape parameter for the median time to OS event for the treatment group.}
#'                      \item{UserParam$dMedianOS1PriorRate}{Rate parameter for the median time to OS event for the treatment group.}
#'                      \item{UserParam$dProbOfDeathBeforeProgression0Param1}{Alpha parameter for probability of death before PFS for the control group.}
#'                      \item{UserParam$dProbOfDeathBeforeProgression0Param2}{Beta parameter for probability of death before PFS for the control group.}
#'                      \item{UserParam$dProbOfDeathBeforeProgression1Param1}{Alpha parameter for probability of death before PFS for the treatment group.}
#'                      \item{UserParam$dProbOfDeathBeforeProgression1Param2}{Beta parameter for probability of death before PFS for the treatment group.}
#'                  }
#'
#' @return A list containing the following elements:
#'         \describe{
#'             \item{SurvivalTime}{A vector of simulated PFS times for each subject.}
#'             \item{OS}{A vector of simulated OS times for each subject.}
#'             \item{ErrorCode}{Optional integer value:
#'                      \describe{
#'                        \item{0}{No error.}
#'                        \item{> 0}{Non-fatal error; current simulation is aborted but subsequent simulations continue.}
#'                        \item{< 0}{Fatal error; no further simulations are attempted.}
#'                      }}
#'             }
######################################################################################################################## .

Simulate2EndpointTTEWithMultiState <- function( NumSub, NumArm, ArrivalTime, TreatmentID,
                                                SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{
    # Step 1 - Initialize the return variables or other variables needed ####
    nError          <- 0

    # Step 2 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {
        # Return fatal error if no user param
        return( list( ErrorCode     = as.integer( -1 ),
                      SurvivalTime  = as.integer( 0 ),
                      OS            = as.double( 0 ) ) )
    }

    # Step 3 - Simulate the patient data ####
    # There are two Options:
    vValuesOption1 <- unlist( UserParam[ c( "dMedianPFS0", "dMedianOS0", "dProbOfDeathBeforeProgression0",
                                            "dMedianPFS1", "dMedianOS1", "dProbOfDeathBeforeProgression1"
    ) ], use.names = FALSE )

    vValuesOption2 <- unlist( UserParam[ c( "dMedianPFS0PriorShape", "dMedianPFS0PriorRate", "dProbOfDeathBeforeProgression0Param1",
                                            "dMedianOS0PriorShape", "dMedianOS0PriorRate", "dProbOfDeathBeforeProgression0Param2",
                                            "dMedianPFS1PriorShape", "dMedianPFS1PriorRate", "dProbOfDeathBeforeProgression1Param1",
                                            "dMedianOS1PriorShape", "dMedianOS1PriorRate", "dProbOfDeathBeforeProgression1Param2"
    ) ], use.names = FALSE )

    # Option 1: directly input the median times and probabilities of death before progression. In this case, vValuesOption1 are used and vValuesOption2 ignored.
    if( length( vValuesOption1 ) == 6 && !( all( vValuesOption1 == 0 ) ) )
    {
        # User provided values that are fixed for the multistate model
        dMedianPFS0 <- UserParam$dMedianPFS0
        dMedianOS0  <- UserParam$dMedianOS0
        dProbOfDeathBeforeProgression0 <- UserParam$dProbOfDeathBeforeProgression0

        dMedianPFS1 <- UserParam$dMedianPFS1
        dMedianOS1  <- UserParam$dMedianOS1
        dProbOfDeathBeforeProgression1 <- UserParam$dProbOfDeathBeforeProgression1

        vPatsPerArm   <- table( TreatmentID )
        dfControlPats <- SimulateDualMultiStateTTE( vPatsPerArm[ 1 ], dMedianPFS0, dMedianOS0, dProbOfDeathBeforeProgression0 )
        dfExpPats     <- SimulateDualMultiStateTTE( vPatsPerArm[ 2 ], dMedianPFS1, dMedianOS1, dProbOfDeathBeforeProgression1 )

    }
    # Option 2: customize how patient data is simulated by building a more realistic model for both PFS and OS outcomes
    # using prior distributions. In this case, vValuesOption2 are used and vValuesOption1 are ignored.

    else if( length( vValuesOption2 ) == 12 && !( all( vValuesOption2 == 0 ) ) )
    {
        vPatsPerArm   <- table( TreatmentID )

        # First need to sample the prior for control
        dfControlPats <-  data.frame( vPFS = NA, vOS = NA )
        nAttempt2     <- 1
        while( any( is.na( dfControlPats$vPFS ) ) & nAttempt2 <= 100 )
        {
            dMedianOS0  <- 1
            dMedianPFS0 <- 2
            nAttempt    <- 1
            while( dMedianOS0 < dMedianPFS0 & nAttempt <= 100 )
            {
                dMedianPFS0 <- rgamma( 1, UserParam$dMedianPFS0PriorShape, UserParam$dMedianPFS0PriorRate )
                dMedianOS0  <- rgamma( 1, UserParam$dMedianOS0PriorShape, UserParam$dMedianOS0PriorRate )
                dProbOfDeathBeforeProgression0 <- rbeta( 1, UserParam$dProbOfDeathBeforeProgression0Param1, UserParam$dProbOfDeathBeforeProgression0Param2 )

                nAttempt <- nAttempt + 1
            }
            if( nAttempt > 100 )
            {
                # Error could not sample a OS that is greater than PFS median
                nError <- 1  # Non-fatal error throw this set out, but if this happens a lot then the user should reconsider the parameters
                return( list( SurvivalTime = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                               OS = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                               ErrorCode = as.integer( nError ) ) )
            }

            dfControlPats <- SimulateDualMultiStateTTE( vPatsPerArm[ 1 ], dMedianPFS0, dMedianOS0, dProbOfDeathBeforeProgression0 )
            nAttempt2     <- nAttempt2 + 1
        }

        if( nAttempt2 > 100 )
        {
            # Error could not sample a OS that is greater than PFS median
            nError <- 2  # Non-fatal error throw this set out, but if this happens a lot then the user should reconsider the parameters
            return( list( SurvivalTime = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                          OS = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                          ErrorCode = as.integer( nError ) ) )
        }

        # Sample median PFS, OS and prob  from the experimental arm
        dfExpPats     <-  data.frame( vPFS = NA, vOS = NA )
        nAttempt2     <- 1
        while( any( is.na( dfExpPats$vPFS ) ) & nAttempt2 <= 100 )
        {
            dMedianOS1  <- 1
            dMedianPFS1 <- 2
            nAttempt    <- 1
            while( dMedianOS1 < dMedianPFS1 & nAttempt <= 100 )
            {
                dMedianPFS1 <- rgamma( 1, UserParam$dMedianPFS1PriorShape, UserParam$dMedianPFS1PriorRate )
                dMedianOS1  <- rgamma( 1, UserParam$dMedianOS1PriorShape, UserParam$dMedianOS1PriorRate )
                dProbOfDeathBeforeProgression1 <- rbeta( 1, UserParam$dProbOfDeathBeforeProgression1Param1, UserParam$dProbOfDeathBeforeProgression1Param2 )
                nAttempt <- nAttempt + 1
            }

            if( nAttempt > 100 )
            {
                # Error could not sample a OS that is greater than PFS median
                nError <- 3  # Non-fatal error throw this set out, but if this happens a lot then the user should reconsider the parameters
                return( list( SurvivalTime = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                               OS = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                               ErrorCode = as.integer( nError ) ) )
            }

            dfExpPats     <- SimulateDualMultiStateTTE( vPatsPerArm[ 2 ], dMedianPFS1, dMedianOS1, dProbOfDeathBeforeProgression1 )

            nAttempt2     <- nAttempt2 + 1
        }

        if( nAttempt2 > 100 )
        {
            # Error could not sample a OS that is greater than PFS median
            nError <- 4  # Non-fatal error throw this set out, but if this happens a lot then the user should reconsider the parameters
            return( list( SurvivalTime = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                         OS = as.double( rep( 1, vPatsPerArm[ 1 ] + vPatsPerArm[ 2 ] ) ),
                         ErrorCode = as.integer( nError ) ) )
        }
    }
    else
    {
        # Return fatal error if UserParam variables are partially present for either option or all are zeros.
        return( list( ErrorCode     = as.integer( -2 ),
                      SurvivalTime  = as.integer( 0 ),
                      OS            = as.double( 0 ) ) )

    }

    vPFS          <- rep( NA, NumSub )
    vOS           <- rep( NA, NumSub )

    vPFS[ TreatmentID == 0 ] <- dfControlPats$vPFS
    vPFS[ TreatmentID == 1 ] <- dfExpPats$vPFS

    vOS[ TreatmentID == 0 ] <- dfControlPats$vOS
    vOS[ TreatmentID == 1 ] <- dfExpPats$vOS

    return( list( SurvivalTime = as.double( vPFS ), OS = as.double( vOS ), ErrorCode = as.integer( nError ) ) )
}

# Simulate dual multi-state time-to-event data ####

SimulateDualMultiStateTTE <- function( nQtyOfPatients, dMedianPFS, dMedianOS, dProbOfDeathBeforeProgression )
{
    # Get alphas using ComputeAlphasForMultiStateModel function
    lAlphas <- ComputeAlphasForMultiStateModel( dMedianPFS, dMedianOS, dProbOfDeathBeforeProgression )

    if( lAlphas$Error == -1 )
    {
        dfRet <- data.frame( vPFS = NA, vOS = NA )
        return( dfRet )
    }

    dAlpha01 <- lAlphas$dRateTimeToProgression
    dAlpha02 <- lAlphas$dRateTimeToDeath
    dAlpha12 <- lAlphas$dRateTimeFromProgressionToDeath

    # Generate time to progression (X1) using alpha1
    vTimeToProgression <- rexp( nQtyOfPatients, dAlpha01 )
    # Generate time to death (X2) using alpha2
    vTimeToDeath <- rexp( nQtyOfPatients, dAlpha02 )
    # Generate time from progression to death (X3) using alpha12
    vTimeFromProgressionToDeath <- rexp( nQtyOfPatients, dAlpha12 )

    # Initialize vectors to capture PFS and OS
    vPFS <- c()
    vOS  <- c()
    for( iPat in 1:nQtyOfPatients )
    {
        if( vTimeToProgression[ iPat ] < vTimeToDeath[ iPat ] )
        {
            vPFS <- c( vPFS, vTimeToProgression[ iPat ] )
            vOS  <- c( vOS, vTimeToProgression[ iPat ] + vTimeFromProgressionToDeath[ iPat ] )
        }
        else
        {
            vPFS <- c( vPFS, vTimeToDeath[ iPat ] )
            vOS  <- c( vOS, vTimeToDeath[ iPat ] )
        }
    }

    dfRet <- data.frame( vPFS, vOS )
    return( dfRet )
}

# Compute transition rates for the multi-state model ####

ComputeAlphasForMultiStateModel <- function( dMedianPFS, dMedianOS, dProbOfDeathBeforeProgression )
{
    dMedianProgToDeath <- ComputeMedianProgToDeath( dMedianPFS, dMedianOS, dProbOfDeathBeforeProgression )

    if( is.na( dMedianProgToDeath ) )
    {
        return( list( Error = -1 ) )
    }
    dOneMinusPDivP     <- ( ( 1 - dProbOfDeathBeforeProgression ) / dProbOfDeathBeforeProgression )
    dAlpha02           <- log( 2 ) / ( dMedianPFS * ( dOneMinusPDivP + 1 ) )
    dAlpha01           <- dOneMinusPDivP * dAlpha02
    dAlpha12           <- log( 2 ) / dMedianProgToDeath

    lRet <- list( dAlpha01 = dAlpha01,
                  dAlpha02 = dAlpha02,
                  dAlpha12 = dAlpha12,
                  dRateTimeToProgression          = dAlpha01,
                  dRateTimeToDeath                = dAlpha02,
                  dRateTimeFromProgressionToDeath = dAlpha12,
                  Error = 0 )
    return( lRet )   # Use an explicit return
}

# Compute median time from progression to death ####

ComputeMedianProgToDeath <- function( dMedianPFS, dMedianOS, dProbDeathB4Prog )
{
    dMedianProgToDeath <- NA

    f <- function( x, dMedianPFS ){ return( ComputeMedianOS( dMedianPFS, x, dProbDeathB4Prog ) - dMedianOS ) }
            tryCatch(
            {
        dMedianProgToDeath <- uniroot( f, lower = 0.01, upper = dMedianOS, dMedianPFS = dMedianPFS )$root
    }, error = function( e )
    {
        dMedianProgToDeath <- NA
        return( dMedianProgToDeath )
    } )

    return( dMedianProgToDeath )
}

# Compute median overall survival using simulated data ####

ComputeMedianOS <- function( dMedianPFS, dMedianProgToDeath, dProbDeathB4Prog )
{
    n <- 10000

    vPFS <- rexp( n, log( 2 ) / dMedianPFS )
    vOS  <- vPFS  + rexp( n, log( 2 ) / dMedianProgToDeath )
    vDeathB4Prog <- rbinom( n, 1, dProbDeathB4Prog )
    vOS <- ifelse( vDeathB4Prog == 1, vPFS, vOS )

    dMedianOS <- median( vOS )
    return( dMedianOS )
}
