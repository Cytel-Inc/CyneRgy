######################################################################################################################## .
# THIS CODE IS JUST AN EXAMPLE FOR REPEATED MEASURE AND MAY NOT WORK, JUST AN IDEA, HENCE IN THE SANDBOX ####
######################################################################################################################## .

#' If UseParam is supplied, the list must contain the following named elements:
#' \describe{
#'    \item{UserParam$dStdDev}{True standard deviation to use when simulating patient data. }
#'    }
SimulatePatientOutcomeMultivariateNormal <- function(NumSub, TreatmentID, Mean, StdDev, UserParam = NULL)
{
    # load the MASS library for mvrnorm
    library( MASS)

    # Assume that if a type is improved it will have a treatment effect of UserParam$dImprovedMean 
    #  Assume all types are independent with variance = 1.5
     
    vMeanCtlr              <- c( 0, 0,      0, 0)
    vMeanExp               <- c( 0, 0.25, 0.5, 1)
    nQtyTimePoints         <- length( vMeanExp )
   
    dStdDev                <- UserParam$dStdDev                   # Standard deviation of the simulated data
    
  
    # Step 2 - Simulate the data, assuming the types are independent and variance = dStdDev*dStdDev ####
    
    vQtyPatientsPerArm <- table( TreatmentID )
    
    mCtrl <- mvrnorm( vQtyPatientsPerArm[ 1 ], vMeanCtrl, Sigma = diag( dStdDev*dStdDev, nQtyTimePoints) ) 
    mExp  <- mvrnorm( vQtyPatientsPerArm[ 2 ], vMeanExp,  Sigma = diag( dStdDev*dStdDev, nQtyTimePoints) ) 
    
    # Need to put the simulated patient data with the correct treatment
    # Initialize a matrix to hold the outcomes
    mOutcomes <- matrix(nrow = sum(vQtyPatientsPerArm), ncol = nQtyTimePoints)
    
    # Get outcomes for control group
    mOutcomes[TreatmentID == 0,] <- mCtrl
    
    # Get outcomes for experimental group
    mOutcomes[TreatmentID == 1,] <- mExp

    # Step 3 - Build the return list   East expects a Response variable in the return so just make it the first type #### 
    lReturn <- list( Response = as.double( mOutcomes[,1] ), ErrorCode = as.integer( 0 )  )
    
    # Add all the types to the list 
    for( nTime in 1:nQtyTimePoints )
    {
        strTypeName <- paste0( "Time", nTime )
        lReturn[[strTypeName ]] <- as.double( mOutcomes[, nTime] )
    }
    
    return( lReturn )
}


