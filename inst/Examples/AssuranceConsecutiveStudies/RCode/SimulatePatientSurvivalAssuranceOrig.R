# Version 2
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
SimulatePatientSurvivalAssurance <- function(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
{
    # The SurvParam depends on input in East, EAST sends the Medan (see the Simulation->Response Generation tab for what is sent)
    setwd( "C:\\Kyle\\Cytel\\Solara\\GSK\\Assurance\\Input\\")
    vSurvTime <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    
    vTreatmentID <- TreatmentID +1   # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1 
    ErrorCode    <- rep( -1, NumSub ) 
    
    #saveRDS( SurvMethod, "SurvMethod.Rds")
    #saveRDS( SurvParam, "SurvParam.Rds")
    #save( NumPrd, "NumPrd.Rds")
    
    if(SurvMethod == 1)   # Hazard Rates
    {
        # First sample the piece of the prior we want to use
        nPriorPart <- rbinom( 1, 1, 0.25 )
        if( nPriorPart == 1 )
        {
            # Sample the normal part 
            dLogTrueHazard <- rnorm( 1, 0, 0.02 )
        }
        else
        {
            # Comes from Beta(2,2) scaled to -4, 0
            dLogTrueHazard <- rbeta( 1, 2, 2 )
            dLogTrueHazard <- 0.4* (dLogTrueHazard)-0.4
            
        }
        dTrueHazard <- exp( dLogTrueHazard )
        SurvParam[1, 2] <- dTrueHazard * SurvParam[1, 1]
        
            vRates <- SurvParam[1, ]
            
            vTrt1 <- rexp( 300, vRates[ 1 ] )
            vTrt2 <- rexp( 300, vRates[ 2 ] )
            
            vSurvTime[ vTreatmentID == 1 ] <- vTrt1
            vSurvTime[ vTreatmentID == 2 ] <- vTrt2
            # Simulate the patient survival times based on the treatment
            # For the Hazard Rate input with 1 piece, this is just simulating from an exponential distribution as an example and results will match
            # East if you used the build hazard option.
            # for( nPatIndx in 1:NumSub)
            # {
            #     nPatientTreatment     <- vTreatmentID[ nPatIndx ]
            #     vSurvTime[ nPatIndx ] <- rexp( 1, vRates[ nPatientTreatment ] )
            #     
            # }
        
    }
    else if(SurvMethod == 2)   # Cumulative % Survivals
    {
        ErrorCode <- ERROR1 # ERROR1 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
        
    }
    else if(SurvMethod == 3)   # Median Survival Times
    {
        ErrorCode <- ERROR3   # ERROR2 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
    }
    
    return(list(SurvivalTime = as.double(vSurvTime), TrueHR = as.double( rep( dTrueHazard, 600)), ErrorCode = ErrorCode) )
}


SimulatePatientSurvivalAssuranceUsingPh2 <- function(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
{
    # The SurvParam depends on input in East, EAST sends the Medan (see the Simulation->Response Generation tab for what is sent)
    #setwd( "C:\\Kyle\\Cytel\\Solara\\GSK\\Assurance\\Input\\")
    vSurvTime <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    
    vTreatmentID <- TreatmentID +1   # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1 
    ErrorCode    <- rep( -1, NumSub ) 
    
    #saveRDS( SurvMethod, "SurvMethod.Rds")
    #saveRDS( SurvParam, "SurvParam.Rds")
    #save( NumPrd, "NumPrd.Rds")
    
    if(SurvMethod == 1)   # Hazard Rates
    {
        
        # For the Example with TTE we sampled the bimodal distribution first.
        # nPriorPart <- rbinom( 1, 1, 0.25 )
        # if( nPriorPart == 1 )
        # {
        #     # Sample the normal part 
        #     dLogTrueHazard <- rnorm( 1, 0, 0.02 )
        # }
        # else
        # {
        #     # Comes from Beta(2,2) scaled to -4, 0
        #     dLogTrueHazard <- rbeta( 1, 2, 2 )
        #     dLogTrueHazard <- 0.4* (dLogTrueHazard)-0.4
        #     
        # }
        
        vPh3PriorHR 
        dTrueHazard <- exp( vPh3PriorHR[ 1 ] )
        vPh3PriorHR <<- vPh3PriorHR[ -1]  # Remove the sample value that was used
        
        SurvParam[1, 2] <- dTrueHazard * SurvParam[1, 1]
        
        vRates <- SurvParam[1, ]
        
        vTrt1 <- rexp( 300, vRates[ 1 ] )
        vTrt2 <- rexp( 300, vRates[ 2 ] )
        
        vSurvTime[ vTreatmentID == 1 ] <- vTrt1
        vSurvTime[ vTreatmentID == 2 ] <- vTrt2
        # Simulate the patient survival times based on the treatment
        # For the Hazard Rate input with 1 piece, this is just simulating from an exponential distribution as an example and results will match
        # East if you used the build hazard option.
        # for( nPatIndx in 1:NumSub)
        # {
        #     nPatientTreatment     <- vTreatmentID[ nPatIndx ]
        #     vSurvTime[ nPatIndx ] <- rexp( 1, vRates[ nPatientTreatment ] )
        #     
        # }
        
    }
    else if(SurvMethod == 2)   # Cumulative % Survivals
    {
        ErrorCode <- ERROR1 # ERROR1 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
        
    }
    else if(SurvMethod == 3)   # Median Survival Times
    {
        ErrorCode <- ERROR3   # ERROR2 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
    }
    
    return(list(SurvivalTime = as.double(vSurvTime), TrueHR = as.double( rep( dTrueHazard, 600)), ErrorCode = ErrorCode) )
}




SimulatePatientSurvivalAssuranceUsingPh2 <- function(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
{
    # The SurvParam depends on input in East, EAST sends the Medan (see the Simulation->Response Generation tab for what is sent)
    #setwd( "C:\\Kyle\\Cytel\\Solara\\GSK\\Assurance\\Input\\")
    vSurvTime <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    
    vTreatmentID <- TreatmentID +1   # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1 
    ErrorCode    <- rep( -1, NumSub ) 
    
    #saveRDS( SurvMethod, "SurvMethod.Rds")
    #saveRDS( SurvParam, "SurvParam.Rds")
    #save( NumPrd, "NumPrd.Rds")
    
    if(SurvMethod == 1)   # Hazard Rates
    {
        
        # For the Example with TTE we sampled the bimodal distribution first.
        # nPriorPart <- rbinom( 1, 1, 0.25 )
        # if( nPriorPart == 1 )
        # {
        #     # Sample the normal part 
        #     dLogTrueHazard <- rnorm( 1, 0, 0.02 )
        # }
        # else
        # {
        #     # Comes from Beta(2,2) scaled to -4, 0
        #     dLogTrueHazard <- rbeta( 1, 2, 2 )
        #     dLogTrueHazard <- 0.4* (dLogTrueHazard)-0.4
        #     
        # }
        
        vPh3PriorHR 
        dTrueHazard <- exp( vPh3PriorHR[ 1 ] )
        vPh3PriorHR <<- vPh3PriorHR[ -1]  # Remove the sample value that was used
        
        SurvParam[1, 2] <- dTrueHazard * SurvParam[1, 1]
        
        vRates <- SurvParam[1, ]
        
        vTrt1 <- rexp( 300, vRates[ 1 ] )
        vTrt2 <- rexp( 300, vRates[ 2 ] )
        
        vSurvTime[ vTreatmentID == 1 ] <- vTrt1
        vSurvTime[ vTreatmentID == 2 ] <- vTrt2
        # Simulate the patient survival times based on the treatment
        # For the Hazard Rate input with 1 piece, this is just simulating from an exponential distribution as an example and results will match
        # East if you used the build hazard option.
        # for( nPatIndx in 1:NumSub)
        # {
        #     nPatientTreatment     <- vTreatmentID[ nPatIndx ]
        #     vSurvTime[ nPatIndx ] <- rexp( 1, vRates[ nPatientTreatment ] )
        #     
        # }
        
    }
    else if(SurvMethod == 2)   # Cumulative % Survivals
    {
        ErrorCode <- ERROR1 # ERROR1 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
        
    }
    else if(SurvMethod == 3)   # Median Survival Times
    {
        ErrorCode <- ERROR3   # ERROR2 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
    }
    
    return(list(SurvivalTime = as.double(vSurvTime), TrueHR = as.double( rep( dTrueHazard, 600)), ErrorCode = ErrorCode) )
}


SimulatePatientSurvivalAssuranceUsingPh2Prior <- function(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
{
    # The SurvParam depends on input in East, EAST sends the Medan (see the Simulation->Response Generation tab for what is sent)
    #setwd( "C:\\Kyle\\Cytel\\Solara\\GSK\\Assurance\\Input\\")
    vSurvTime <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    
    vTreatmentID <- TreatmentID +1   # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1 
    ErrorCode    <- rep( -1, NumSub ) 
    
    #saveRDS( SurvMethod, "SurvMethod.Rds")
    #saveRDS( SurvParam, "SurvParam.Rds")
    #save( NumPrd, "NumPrd.Rds")
    
    if(SurvMethod == 1)   # Hazard Rates
    {
        
        # For the Example with TTE we sampled the bimodal distribution first from ph 2 first
        # then we use the link function to get the true HR
        # This will allow us to get the unconditional using the prior on the treatment different in Ph2
        nPriorPart <- rbinom( 1, 1, 0.25 )
        if( nPriorPart == 1 )
        {
            # Sample the normal part
            dTrueDelta <- rnorm( 1, 0, 0.05 )
        }
        else
        {
            # Comes from Beta(2,2) scaled to -4, 0
            dTrueDelta <- rnorm( 1, 0.7, 0.3 )

        }
        dTrueHR <- 0.1 - 0.4*dTrueDelta
         
        dTrueHazard <- exp( dTrueHR )
        
        SurvParam[1, 2] <- dTrueHazard * SurvParam[1, 1]
        
        vRates <- SurvParam[1, ]
        
        vTrt1 <- rexp( 300, vRates[ 1 ] )
        vTrt2 <- rexp( 300, vRates[ 2 ] )
        
        vSurvTime[ vTreatmentID == 1 ] <- vTrt1
        vSurvTime[ vTreatmentID == 2 ] <- vTrt2
        # Simulate the patient survival times based on the treatment
        # For the Hazard Rate input with 1 piece, this is just simulating from an exponential distribution as an example and results will match
        # East if you used the build hazard option.
        # for( nPatIndx in 1:NumSub)
        # {
        #     nPatientTreatment     <- vTreatmentID[ nPatIndx ]
        #     vSurvTime[ nPatIndx ] <- rexp( 1, vRates[ nPatientTreatment ] )
        #     
        # }
        
    }
    else if(SurvMethod == 2)   # Cumulative % Survivals
    {
        ErrorCode <- ERROR1 # ERROR1 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
        
    }
    else if(SurvMethod == 3)   # Median Survival Times
    {
        ErrorCode <- ERROR3   # ERROR2 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
    }
    
    return(list(SurvivalTime = as.double(vSurvTime), TrueHR = as.double( rep( dTrueHazard, 600)), ErrorCode = ErrorCode) )
}

