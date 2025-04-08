#  Last Modified Date: 11/20/2024
#' @name Simulate2EndpointTTEWithMultiState
#' @title Template for simulating patient data in R when the outcome time is time-to-event. 
#' @param NumSub The number of patient times to generate for the trial.  This is a single numeric value, eg 250.
#' @param NumArm  The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param SurvMethod - This values is pulled from the Input Method drop-down list. This will be 1 (Hazard Rate), 2 (Cumulative % survival), 3 (Medians)
#' @param NumPrd Number of time periods that are provided. 
#' @param PrdTime \describe{ 
#'      \item{If SurvMethod = 1}{PrdTime is a vector of starting times of hazard pieces.}
#'      \item{If SurvMethod = 2}{Times at which the cumulative % survivals are specified.}
#'      \item{If SurvMethod = 3}{Period time is 0 by default}
#'      }
#' @param SurvParam \describe{Depends on the table in the Response Generation tab. 2‐D array of parameters to generate the survival times
#'    \item{If SurvMethod is 1}{SurvParam is an array (NumPrd rows, NumArm columns) that specifies arm by arm hazard rates (one rate per arm per piece). 
#'    Thus SurvParam [i, j] specifies hazard rate in ith period for jth arm.
#'    Arms are in columns with column 1 is control, column 2 is experimental
#'    Time periods are in rows, row 1 is time period 1, row 2 is time period 2...}
#'    \item{If SurvMethod is 2}{SurvParam is an array (NumPrd rows,NumArm columns) specifies arm by arm the Cum % Survivals (one value per arm per piece). Thus, SurvParam [i, j] specifies Cum % Survivals in ith period for jth arm.}
#'    \item{If SurvMethod is 3}{SurvParam will be a 1 x 2 array with median survival times on each arms. Column 1 is control, column 2 is experimental }
#'  }
#' @param  UserParam A list of user defined parameters in East.   You must have a default = NULL, as in this example.
#' If UseParam are supplied in East or Solara, they will be an element in the list, eg UserParam$ParameterName.  
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed. 
#'             \describe{
#'             \item{SurvivalTime}{Required numeric value. A vector of generated time to response values for each subject.}
#'             \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Non fatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'             }  
#' @description
#' This template can be used as a starting point for developing custom functionality.  The function signature must remain the same.  
#' However, you may choose to ignore the parameters SurvMethod, NumPrd, PrdTime, and SurvParam if the patient simulator
#' you are creating only requires use of parameters the user will add to UserParam
Simulate2EndpointTTEWithMultiState <- function( NumSub, NumArm, TreatmentID,  
                                                SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL )
{
    # Step 1 - Initialize the return variables or other variables needed ####
	Error 	        <- 0
	vPatientOutcome <- rep( 0, NumSub )  # Note, as you simulate the patient data put in in this vector so it can be returned

	# Step 2 - Validate custom variable input and set defaults ####
	if( is.null( UserParam ) )
	{
	    
	    # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
	    # take care of any issues.   Also, if there is a default value for the parameters you may want to set them here.  Default values usually
	    # are applied to have the same functionality as East, see the first example
	    
	    # EXMAPLE - Set the default if needed
	    #UserParam <- list( dProbOfZeroOutcomeCtrl = 0, dProbOfZeroOutcomeExp = 0 )
	}
	
	# Step 3 - Simulate the patient data and store in vPatientOutcome ####
	
	# TODO Check all params 
    if( !is.null( UserParam$dMedianPFS0 ))
    {
        # User provided values that are fixed for the multistate model
        dMedianPFS0 <- UserParam$dMedianPFS0
        dMedianOS0  <- UserParam$dMedianOS0
        dProbOfDeathBeforeProgression0 <- UserParam$dProbOfDeathBeforeProgression0
        
        dMedianPFS1 <- UserParam$dMedianPFS1
        dMedianOS1  <- UserParam$dMedianOS1
        dProbOfDeathBeforeProgression1 <- UserParam$dProbOfDeathBeforeProgression1
        
        vPatsPerArm   <- table( TreatmentID )
        dfControlPats <- SimulateDualMultiStateTTE( vPatsPerArm[1], dMedianPFS0, dMedianOS0, dProbOfDeathBeforeProgression0 )
        dfExpPats     <- SimulateDualMultiStateTTE( vPatsPerArm[2], dMedianPFS1, dMedianOS1, dProbOfDeathBeforeProgression1 )
        
    }
	else if( !is.null( UserParam$dMedianPFS0PriorShape ) )
	{
	    vPatsPerArm   <- table( TreatmentID )
	    
	    # First need to sample the prior for control 
	    dfControlPats <-  data.frame( vPFS = NA, vOS = NA )
	    nAttempt2     <- 1
	    while( any( is.na(dfControlPats$vPFS ) ) & nAttempt2 <= 100  ) 
	    {
    	    dMedianOS0  <- 1
    	    dMedianPFS0 <- 2
    	    nAttempt    <- 1 
    	    while( dMedianOS0 < dMedianPFS0 & nAttempt <= 100 )
    	    {
    	        #if( nAttempt > 1 )
    	        #    print( paste( "Experimental Attempt ", nAttempt, "dMedian PFS", dMedianPFS0, " dMedianOS", dMedianOS0 ))
        	    dMedianPFS0 <- rgamma( 1, UserParam$dMedianPFS0PriorShape,  UserParam$dMedianPFS0PriorRate )
        	    dMedianOS0  <- rgamma( 1, UserParam$dMedianOS0PriorShape, UserParam$dMedianOS0PriorRate )
        	    dProbOfDeathBeforeProgression0 <- rbeta( 1,  UserParam$dProbOfDeathBeforeProgression0Param1,  UserParam$dProbOfDeathBeforeProgression0Param2 )
        	    
        	    nAttempt <- nAttempt + 1
        	   
    	    }
    	    if( nAttempt > 100 )
    	    {
    	        # Error could not sample a OS that is greater than PFS median
    	        ErrorCode <- -1  # Non-fatal error throw this set out, but if this happens alot then the user should reconsider the parameters
    	        return(  list( SurvivalTime = as.double(  rep( 1,  vPatsPerArm[1] +  vPatsPerArm[2]) ), OS =  as.double(  rep( 1,  vPatsPerArm[1] +  vPatsPerArm[2]) ), ErrorCode = as.integer( Error )) )
    	    }
    	    
    	    
    	    dfControlPats <- SimulateDualMultiStateTTE( vPatsPerArm[1], dMedianPFS0, dMedianOS0, dProbOfDeathBeforeProgression0 )
    	    nAttempt2     <- nAttempt2 + 1 
	    }
	    
	    if( nAttempt2 > 100 )
	    {
	        # Error could not sample a OS that is greater than PFS median
	        ErrorCode <- -2  # Non-fatal error throw this set out, but if this happens alot then the user should reconsider the parameters
	        return(  list( SurvivalTime = as.double(  rep( 1,  vPatsPerArm[1] +  vPatsPerArm[2]) ), OS =  as.double(  rep( 1,  vPatsPerArm[1] +  vPatsPerArm[2]) ), ErrorCode = as.integer( Error )) )
	    }
	    
	    # Sample median PFS, OS and prob  from the experimental arm 
	    dfExpPats <-  data.frame( vPFS = NA, vOS = NA )
	    nAttempt2     <- 1
	    while( any( is.na(dfExpPats$vPFS ) ) & nAttempt2 <= 100  ) 
	    {
    	    dMedianOS1  <- 1 
    	    dMedianPFS1 <- 2
    	    nAttempt    <- 1
    	    while( dMedianOS1 < dMedianPFS1 & nAttempt <= 100 )
    	    {
    	        #if( nAttempt > 1 )
    	        #    print( paste( "Experimental Attempt ", nAttempt, "dMedian PFS", dMedianPFS1, " dMedianOS", dMedianOS1 ))
        	    dMedianPFS1 <- rgamma( 1, UserParam$dMedianPFS1PriorShape, UserParam$dMedianPFS1PriorRate)
        	    dMedianOS1  <- rgamma( 1, UserParam$dMedianOS1PriorShape, UserParam$dMedianOS1PriorRate  )
        	    dProbOfDeathBeforeProgression1 <- rbeta( 1,  UserParam$dProbOfDeathBeforeProgression1Param1,  UserParam$dProbOfDeathBeforeProgression1Param2 )
        	    nAttempt <- nAttempt + 1
    	    }
    	    
    	    if( nAttempt > 100 )
    	    {
    	        # Error could not sample a OS that is greater than PFS median
    	        ErrorCode <- -1  # Non-fatal error throw this set out, but if this happens alot then the user should reconsider the parameters
    	        return(  list( SurvivalTime = as.double(  rep( 1,  vPatsPerArm[1] +  vPatsPerArm[2]) ), OS =  as.double(  rep( 1,  vPatsPerArm[1] +  vPatsPerArm[2]) ), ErrorCode = as.integer( Error )) )
    	        
    
    	    }
    	    
    	    dfExpPats     <- SimulateDualMultiStateTTE( vPatsPerArm[2], dMedianPFS1, dMedianOS1, dProbOfDeathBeforeProgression1 )
    	    
    	    nAttempt2     <- nAttempt2 + 1 
	    }
	    
	    if( nAttempt2 > 100 )
	    {
	        # Error could not sample a OS that is greater than PFS median
	        ErrorCode <- -2  # Non-fatal error throw this set out, but if this happens alot then the user should reconsider the parameters
	        return(  list( SurvivalTime = as.double(  rep( 1,  vPatsPerArm[1] +  vPatsPerArm[2]) ), OS =  as.double(  rep( 1,  vPatsPerArm[1] +  vPatsPerArm[2]) ), ErrorCode = as.integer( Error )) )
	    }
	    
	    
	}
	
	
	
	nQtyPatsSim   <- c(0, 0 )
	vPFS          <- rep( NA, NumSub )
	vOS           <- rep( NA, NumSub )
	
	vPFS[ TreatmentID == 0 ] <- dfControlPats$vPFS
	vPFS[ TreatmentID == 1 ] <- dfExpPats$vPFS
	
	
	vOS[ TreatmentID == 0 ] <- dfControlPats$vOS
	vOS[ TreatmentID == 1 ] <- dfExpPats$vOS
	
	
	# for( nPatIndx in 1:NumSub )
	# {
	#     nTreatmentID                 <- TreatmentID[ nPatIndx ] + 1 # The TreatmentID vector sent from East/Solara has the treatments as 0, 1 so need to add 1 to get a vector index
	#     vPatientOutcome[ nPatIndx ]  <- rexp( 1, vTrueRates[ nTreatmentID ] )
	# }
	# End of example block
	
	
	# Use appropriate error handling and modify the
	# Error appropriately in each of the methods

	return( list( SurvivalTime = as.double( vPFS ), OS = as.double( vOS), ErrorCode = as.integer( Error )))
}

######################################################################################################################## .
# Additional helper functions ####
######################################################################################################################## .

#################################################################################################### .
#   Description: Multi state model for Progression-free survival (PFS) and Overall survival (OS)
#################################################################################################### .
#' @name SimulateDualMultiStateTTE
#' @title SimulateDualMultiStateTTE
#' @param nQtyOfPatients Number of patients to simulate
#' @param dMedianPFS Median time for progression free survival (note PFS includes patients that progress and patients that die before progression)
#' @param dMedianOS Median time for overall survival
#' @param dProbOfDeathBeforeProgression The probability that a patient dies before the progression event is observed
#' @description { Description: Multi state model for Progression-free survival (PFS) and Overall survival (OS) }
SimulateDualMultiStateTTE <- function( nQtyOfPatients, dMedianPFS, dMedianOS, dProbOfDeathBeforeProgression )
{
    # Get alphas using ComputeAlphasForMultiStateModel function contained in ComputeAlphasForMultiStateModel.R
    lAlphas <- ComputeAlphasForMultiStateModel( dMedianPFS, dMedianOS, dProbOfDeathBeforeProgression )
    
    if( lAlphas$Error == -1 )
    {
        dfRet <- data.frame( vPFS = NA, vOS = NA )
        return( dfRet )
    }
        
    dAlpha01 <- lAlphas$dRateTimeToProgression
    dAlpha02 <- lAlphas$dRateTimeToDeath
    dAlpha12 <- lAlphas$dRateTimeFromProgressionToDeath
    
    # generate time to progression (X1) using alpha1
    vTimeToProgression <- rexp( nQtyOfPatients, dAlpha01 )
    # generate time to death (X2) using alpha2
    vTimeToDeath <- rexp( nQtyOfPatients, dAlpha02 )
    # generate time from progression to death (X3) using alpha12
    vTimeFromProgressionToDeath <- rexp( nQtyOfPatients, dAlpha12 )
    
    # initialise vectors to capture PFS and OS
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


#' @param  dMedianPFS Median time for progression free survival (note PFS includes patients that progress and patients that die before progression)
#' @param dMedianOS Median time for overall survival
#' @param dProbOfDeathBeforeProgression The probability that a patient dies before the progression event is observed
#' @description { Description: This function computes the alphas in the multi-state model given the medians and probability of death before progression.}
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

# Often we do not know the median time from progression to death but are given the median PFS and median OS and can be easy to obtain
# the probability of death before progression.  This function computes the desired median progression to death which is need in the multi-state model
ComputeMedianProgToDeath <- function( dMedianPFS, dMedianOS, dProbDeathB4Prog )
{
    dMedianProgToDeath <- NA
    
    f <- function( x, dMedianPFS ){ return( ComputeMedianOS( dMedianPFS, x, dProbDeathB4Prog ) - dMedianOS) }
    tryCatch({
        dMedianProgToDeath <- uniroot( f, lower=.01, upper = dMedianOS, dMedianPFS = dMedianPFS )$root
    }, error = function(e){
        dMedianProgToDeath <- NA
        #print(paste( "dMedianPFS=", dMedianPFS, " dMedianOS = ", dMedianOS, " dProbDeathB4Prog= ", dProbDeathB4Prog ))
        return( dMedianProgToDeath )
    })
    
    return( dMedianProgToDeath)
}

# This function simulates data from the PFS distribution, simulates the times from progression to death, and progression before death.
# Using these 3 variables we can compute the OS and ultimately the median OS which can then be used to find the desired median, or rate,
# for time to death without progression, eg the 0->2 transition in the MS model
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
