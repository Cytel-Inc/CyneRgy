# TODO These notes need to be updated for now using the UserParams
# Parameter Description 
# NumSub - The number of patients (subjects) in the trial.  NumSub survival times need to be generated for the trial.  
#           This is a single numeric value, eg 250.
# NumArm - The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
# The SurvParam depends on input in East. In the simulation window on the Response Generation tab 
# SurvMethod - This values is pulled from the Input Method drop-down list.  
# SurvParam - Depends on the table in the Response Generation tab. 2‐D array of parameters usds to generate time of events.
# If SurvMethod is 1:
#   SurvParam is an array that specifies arm by arm hazard rates (one rate per arm per piece). Thus SurvParam [i, j] specifies hazard rate in ith period for jth arm.
#   Arms are in columns with column 1 is control, column 2 is experimental
#   Time periods are in rows
# If SurvMethod is 2:
#   SurvParam is an array specifies arm by arm the Cum % Survivals (one value per arm per piece). Thus, SurvParam [i, j] specifies Cum % Survivals in ith period for jth arm.
# If SurvMethod is 3:
#   SurvParam will be a 1 x 2 array with median survival times on each arms. Column 1 is control, column 2 is experimental 
#  
# Description: This function simulates from exponential, just included as a simple example as a starting point 
SimulatePatientSurvivalAssuranceUsingPh2Prior <- function(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL  ) 
{
    # The SurvParam depends on input in East, EAST sends the Medan (see the Simulation->Response Generation tab for what is sent)
     #setwd( "C:\\AssuranceNormal\\ExampleArgumentsFromEast\\Example5")
    # #setwd( "[ENTERED THE DESIRED LOCATION TO SAVE THE FILE]" )
    # saveRDS( NumSub, "NumSub.Rds")
    # saveRDS( NumArm, "NumArm.Rds" )
    # saveRDS( TreatmentID, "TreatmentID.Rds" )
    # saveRDS( SurvMethod, "SurvMethod.Rds" )
    # saveRDS( NumPrd, "NumPrd.Rds" )
    # saveRDS( SurvParam, "SurvParam.Rds" )
     # saveRDS( UserParam, "UserParamSim.Rds" )
    
    # Step 1 - Determine how many patients on each treatment need to be simulated ####
    vTrtAllocation <- table( TreatmentID )
    vSurvTime      <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    
    ErrorCode    <- rep( -1, NumSub ) 
    
    # Step 2: Using the true treatment difference from Ph 2, compute the log( true hazard ratio) ####
    dTrueTreatmentDiff <- vPrior[ nSimIndex ]
    nSimIndex <<- nSimIndex + 1
    
    dLogTrueHazardRatio <- UserParam$dIntercept + UserParam$dSlope * dTrueTreatmentDiff
    dTrueHazardRatio    <- exp( dLogTrueHazardRatio )
   
    # Step 3: Compute the hazard on experimental given the true hazard on control and the computed true hazard ratio####
    
    dRateCtrl        <- 1.0/UserParam$dMeanTTECtrl 
    dRateExp         <- dTrueHazardRatio*dRateCtrl
    
    vRates      <- c( dRateCtrl, dRateExp )
    
    vTrt1 <- rexp( vTrtAllocation[ 1 ], vRates[ 1 ] )
    vTrt2 <- rexp( vTrtAllocation[ 2 ], vRates[ 2 ] )
    
    vSurvTime[ TreatmentID == 0 ] <- vTrt1
    vSurvTime[ TreatmentID == 1 ] <- vTrt2
       
    
    return(list(SurvivalTime = as.double(vSurvTime), TrueHR = as.double( rep( dTrueHazardRatio, NumSub)), ErrorCode = ErrorCode) )
}

