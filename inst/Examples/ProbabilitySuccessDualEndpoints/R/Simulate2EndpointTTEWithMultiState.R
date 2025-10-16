######################################################################################################################## .
#  Last Modified Date: 11/20/2024
#' @name Simulate2EndpointTTEWithMultiState
#' @title Simulate Trial Data for Two Time-to-Event Endpoints Using a Multi-State Model
#' 
#' @description
#' This function generates simulated trial data for two time-to-event (TTE) endpoints, progression-free survival (PFS) 
#' and overall survival (OS), using a multi-state model. The simulation utilizes input parameters such as the number 
#' of subjects, number of arms, and user-defined survival parameters.
#' 
#' @param NumSub The number of subjects to simulate for the trial. A single numeric value, e.g., 250.
#' @param NumArm The number of arms in the trial, a single numeric value. For a two-arm trial, this will be 2.
#' @param TreatmentID A vector of treatment IDs, where 0 corresponds to control and 1 corresponds to experimental. 
#'                    The length of this vector must equal NumSub.
#' @param SurvMethod A numeric value specifying the survival method:
#'                   \describe{
#'                       \item{1}{Hazard Rate}
#'                       \item{2}{Cumulative % Survival}
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
#' @param UserParam A list of user-defined parameters. Can contain the following named elements:
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

Simulate2EndpointTTEWithMultiState <- function( NumSub, NumArm, TreatmentID,  
                                                SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{
    # Step 1 - Initialize the return variables or other variables needed ####
	Error 	        <- 0
	vPatientOutcome <- rep( 0, NumSub )  # Note, as you simulate the patient data put in in this vector so it can be returned

	# Step 2 - Validate custom variable input and set defaults ####
	if( is.null( UserParam ) )
	{
	    # Return fatal error if no user param
	    return( list( ErrorCode     = as.integer( -1 ), 
	                  SurvivalTime  = as.integer( 0 ),
	                  OS            = as.double( 0 ) ) )
	}
	
	# Step 3 - Simulate the patient data and store in vPatientOutcome ####
	
    if( !is.null( UserParam$dMedianPFS0 ) )
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
	else if( !is.null( UserParam$dMedianPFS0PriorShape ) )
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
        	    dMedianPFS0 <- rgamma( 1, UserParam$dMedianPFS0PriorShape,  UserParam$dMedianPFS0PriorRate )
        	    dMedianOS0  <- rgamma( 1, UserParam$dMedianOS0PriorShape, UserParam$dMedianOS0PriorRate )
        	    dProbOfDeathBeforeProgression0 <- rbeta( 1,  UserParam$dProbOfDeathBeforeProgression0Param1,  UserParam$dProbOfDeathBeforeProgression0Param2 )
        	    
        	    nAttempt <- nAttempt + 1
    	    }
    	    if( nAttempt > 100 )
    	    {
    	        # Error could not sample a OS that is greater than PFS median
    	        ErrorCode <- -1  # Non-fatal error throw this set out, but if this happens alot then the user should reconsider the parameters
    	        return(  list( SurvivalTime = as.double(  rep( 1,  vPatsPerArm[ 1 ] +  vPatsPerArm[ 2 ] ) ), OS =  as.double(  rep( 1,  vPatsPerArm[ 1 ] +  vPatsPerArm[ 2 ] ) ), ErrorCode = as.integer( Error ) ) )
    	    }
    	    
    	    
    	    dfControlPats <- SimulateDualMultiStateTTE( vPatsPerArm[ 1 ], dMedianPFS0, dMedianOS0, dProbOfDeathBeforeProgression0 )
    	    nAttempt2     <- nAttempt2 + 1 
	    }
	    
	    if( nAttempt2 > 100 )
	    {
	        # Error could not sample a OS that is greater than PFS median
	        ErrorCode <- -2  # Non-fatal error throw this set out, but if this happens alot then the user should reconsider the parameters
	        return(  list( SurvivalTime = as.double(  rep( 1,  vPatsPerArm[ 1 ] +  vPatsPerArm[ 2 ]) ), OS =  as.double(  rep( 1,  vPatsPerArm[ 1 ] +  vPatsPerArm[ 2 ]) ), ErrorCode = as.integer( Error ) ) )
	    }
	    
	    # Sample median PFS, OS and prob  from the experimental arm 
	    dfExpPats     <-  data.frame( vPFS = NA, vOS = NA )
	    nAttempt2     <- 1
	    while( any( is.na(dfExpPats$vPFS ) ) & nAttempt2 <= 100 ) 
	    {
    	    dMedianOS1  <- 1 
    	    dMedianPFS1 <- 2
    	    nAttempt    <- 1
    	    while( dMedianOS1 < dMedianPFS1 & nAttempt <= 100 )
    	    {
        	    dMedianPFS1 <- rgamma( 1, UserParam$dMedianPFS1PriorShape, UserParam$dMedianPFS1PriorRate )
        	    dMedianOS1  <- rgamma( 1, UserParam$dMedianOS1PriorShape, UserParam$dMedianOS1PriorRate )
        	    dProbOfDeathBeforeProgression1 <- rbeta( 1,  UserParam$dProbOfDeathBeforeProgression1Param1,  UserParam$dProbOfDeathBeforeProgression1Param2 )
        	    nAttempt <- nAttempt + 1
    	    }
    	    
    	    if( nAttempt > 100 )
    	    {
    	        # Error could not sample a OS that is greater than PFS median
    	        ErrorCode <- -1  # Non-fatal error throw this set out, but if this happens alot then the user should reconsider the parameters
    	        return(  list( SurvivalTime = as.double(  rep( 1,  vPatsPerArm[ 1 ] +  vPatsPerArm[ 2 ] ) ), OS =  as.double(  rep( 1,  vPatsPerArm[ 1 ] +  vPatsPerArm[ 2 ] ) ), ErrorCode = as.integer( Error ) ) )
    	    }
    	    
    	    dfExpPats     <- SimulateDualMultiStateTTE( vPatsPerArm[ 2 ], dMedianPFS1, dMedianOS1, dProbOfDeathBeforeProgression1 )
    	    
    	    nAttempt2     <- nAttempt2 + 1 
	    }
	    
	    if( nAttempt2 > 100 )
	    {
	        # Error could not sample a OS that is greater than PFS median
	        ErrorCode <- -2  # Non-fatal error throw this set out, but if this happens alot then the user should reconsider the parameters
	        return(  list( SurvivalTime = as.double(  rep( 1,  vPatsPerArm[ 1 ] +  vPatsPerArm[ 2 ]) ), OS =  as.double(  rep( 1,  vPatsPerArm[ 1 ] +  vPatsPerArm[ 2 ] ) ), ErrorCode = as.integer( Error ) ) )
	    }
	}
	
	nQtyPatsSim   <- c( 0, 0 )
	vPFS          <- rep( NA, NumSub )
	vOS           <- rep( NA, NumSub )
	
	vPFS[ TreatmentID == 0 ] <- dfControlPats$vPFS
	vPFS[ TreatmentID == 1 ] <- dfExpPats$vPFS
	
	vOS[ TreatmentID == 0 ] <- dfControlPats$vOS
	vOS[ TreatmentID == 1 ] <- dfExpPats$vOS

	return( list( SurvivalTime = as.double( vPFS ), OS = as.double( vOS ), ErrorCode = as.integer( Error ) ) )
}


######################################################################################################################## .
# Additional helper functions ####
######################################################################################################################## .


#################################################################################################### .
#' @name SimulateDualMultiStateTTE
#' @title Simulate Dual Multi-State Time-to-Event Data
#' 
#' @description
#' This function simulates progression-free survival (PFS) and overall survival (OS) using a multi-state model. 
#' Patients can transition between states: progression-free, progression, and death. It uses exponential distributions 
#' to model time-to-event transitions based on specified median survival times and probabilities of death before progression.
#' 
#' @param nQtyOfPatients Number of patients to simulate.
#' @param dMedianPFS Median time for progression-free survival (PFS). PFS includes patients who progress and patients who die before progression.
#' @param dMedianOS Median time for overall survival (OS).
#' @param dProbOfDeathBeforeProgression Probability that a patient dies before the progression event is observed.
#' @return A data frame containing two columns:
#'         \describe{
#'             \item{vPFS}{Simulated progression-free survival times.}
#'             \item{vOS}{Simulated overall survival times.}
#'         }
#################################################################################################### .

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
        if ( vTimeToProgression[ iPat ] < vTimeToDeath[ iPat ] )
        {
            vPFS <- c( vPFS, vTimeToProgression[ iPat ] )
            vOS  <- c( vOS, vTimeToProgression[ iPat ] + vTimeFromProgressionToDeath[ iPat ] )
        } else
        {
            vPFS <- c( vPFS, vTimeToDeath[ iPat ] )
            vOS  <- c( vOS, vTimeToDeath[ iPat ] )
        }
    }
    
    dfRet <- data.frame( vPFS, vOS )
    return( dfRet )
}


#################################################################################################### .
#' @name ComputeAlphasForMultiStateModel
#' @title Compute Transition Rates for Multi-State Model
#' 
#' @description
#' This function calculates transition rates (alphas) for a multi-state model based on input parameters. 
#' The model transitions include time to progression, time to death, and time from progression to death. 
#' The rates are derived from median survival times and the probability of death before progression.
#' 
#' @param dMedianPFS Median time for progression-free survival (PFS).
#' @param dMedianOS Median time for overall survival (OS).
#' @param dProbOfDeathBeforeProgression Probability that a patient dies before the progression event is observed.
#' @return A list containing:
#'         \describe{
#'             \item{dAlpha01}{Rate for time to progression.}
#'             \item{dAlpha02}{Rate for time to death without progression.}
#'             \item{dAlpha12}{Rate for time from progression to death.}
#'             \item{Error}{Error code, where 0 indicates success and -1 indicates failure.}
#'         }
#################################################################################################### .

ComputeAlphasForMultiStateModel <- function( dMedianPFS, dMedianOS, dProbOfDeathBeforeProgression )
{
    dMedianProgToDeath <- ComputeMedianProgToDeath( dMedianPFS, dMedianOS, dProbOfDeathBeforeProgression )
    
    if( is.na( dMedianProgToDeath ) )
    {
        return( list( Error = -1) )
    }
    dOneMinusPDivP     <- ((1-dProbOfDeathBeforeProgression)/dProbOfDeathBeforeProgression)
    dAlpha02           <- log( 2 )/( dMedianPFS * (dOneMinusPDivP + 1 ))
    dAlpha01           <- dOneMinusPDivP * dAlpha02
    dAlpha12           <- log( 2 )/dMedianProgToDeath
    
    lRet <- list( dAlpha01 = dAlpha01,
                  dAlpha02 = dAlpha02,
                  dAlpha12 = dAlpha12,
                  dRateTimeToProgression          = dAlpha01,
                  dRateTimeToDeath                = dAlpha02,
                  dRateTimeFromProgressionToDeath = dAlpha12,
                  Error = 0 )
    return( lRet )   # Use an explicit return
}


#################################################################################################### .
#' @name ComputeMedianProgToDeath
#' @title Compute Median Time from Progression to Death
#' 
#' @description
#' This function computes the median time from progression to death in a multi-state model. 
#' It uses input parameters such as median progression-free survival (PFS), median overall survival (OS), 
#' and the probability of death before progression to derive the median progression-to-death survival time.
#' 
#' @param dMedianPFS Median time for progression-free survival (PFS).
#' @param dMedianOS Median time for overall survival (OS).
#' @param dProbDeathB4Prog Probability of death before progression.
#' @return Numeric value representing the median progression-to-death time. Returns `NA` if computation fails.
#################################################################################################### .

ComputeMedianProgToDeath <- function( dMedianPFS, dMedianOS, dProbDeathB4Prog )
{
    dMedianProgToDeath <- NA
    
    f <- function( x, dMedianPFS ){ return( ComputeMedianOS( dMedianPFS, x, dProbDeathB4Prog ) - dMedianOS) }
    tryCatch({
        dMedianProgToDeath <- uniroot( f, lower=.01, upper = dMedianOS, dMedianPFS = dMedianPFS )$root
    }, error = function(e){
        dMedianProgToDeath <- NA
        return( dMedianProgToDeath )
    })
    
    return( dMedianProgToDeath)
}


#################################################################################################### .
#' @name ComputeMedianOS
#' @title Compute Median Overall Survival Using Simulated Data
#' 
#' @description
#' This function simulates progression-free survival (PFS) and overall survival (OS) times for a large number of patients. 
#' It calculates the median overall survival based on these simulations. The function uses specified median PFS, median 
#' progression-to-death times, and the probability of death before progression to simulate survival times.
#' 
#' @param dMedianPFS Median time for progression-free survival (PFS).
#' @param dMedianProgToDeath Median time from progression to death.
#' @param dProbDeathB4Prog Probability of death before progression.
#' @return Numeric value representing the median overall survival (OS).
#################################################################################################### .

ComputeMedianOS <- function( dMedianPFS, dMedianProgToDeath, dProbDeathB4Prog )
{
    n <- 10000
    
    vPFS <- rexp( n, log(2)/dMedianPFS )
    vOS  <- vPFS  +rexp( n, log(2)/dMedianProgToDeath)
    vDeathB4Prog <- rbinom( n, 1,  dProbDeathB4Prog )
    vOS <- ifelse( vDeathB4Prog == 1, vPFS, vOS )
    
    dMedianOS <- median( vOS )
    return( dMedianOS )
}
