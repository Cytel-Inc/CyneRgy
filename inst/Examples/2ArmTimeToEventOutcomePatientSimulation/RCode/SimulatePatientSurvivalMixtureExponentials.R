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
SimulatePatientSurvivalMixtureExponentials <- function(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
{
    # The SurvParam depends on input in East, EAST sends the Medan (see the Simulation->Response Generation tab for what is sent)
    
    vSurvTime <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    
    vTreatmentID <- TreatmentID +1   # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1 
    ErrorCode    <- rep( -1, NumSub ) 
    
    
    if(SurvMethod == 1)   # Hazard Rates
    {
        
        if( NumPrd == 1 )
        {
            vRates <- SurvParam[1, ]
            
            # Simulate the patient survival times based on the treatment
            # For the Hazard Rate input with 1 piece, this is just simulating from an exponential distribution as an example and results will match
            # East if you used the build hazard option.
            for( nPatIndx in 1:NumSub)
            {
                nPatientTreatment     <- vTreatmentID[ nPatIndx ]
                vSurvTime[ nPatIndx ] <- rexp( 1, vRates[ nPatientTreatment ] )
                
            }
        }
        else 
        {
            # This version is using the input to generate a mixture distribution.  
            # The SurvParam matrix is used to simulate a mixture distribution.
            # Assuming the values in the SurvParam is a matrix of MEDIAN survival times, treatments in columns, groups in rows.
            # Column 1 is control, Column 2 is experimental
            # The PrdTime is used to provide the probability of each group.  Since East fixes PrdTime[1] = 0 in this case the probability of this being in this group
            # would be 1 - sum( PrdTime ) 
            # Note: Even though East sent the SurvMethod = 1 this example is treating the SurvParam matrix as medians.  This was done so 
            #       so the PrdTime could provide the group probabilities and the matrix the median but could have easily used the matrix as rates
            vProbs            <- PrdTime
            vProbs[1]         <- 1-sum( vProbs[2:length(vProbs)])
            nQtyPatientGroups <- length( vProbs )
            
            if( nQtyPatientGroups == NumPrd )
            {
                
                mMedians  <- SurvParam
                mMeans    <- mMedians/log(2)
                mRates    <- 1/mMeans
                
                # Now we have the mRates matrix that has a row for each group, a column for each treatment, Column 1 is Control, column 2 is treatment
                
                # Simulate the patient groups
                vPatientGroup <- sample( c(1:nQtyPatientGroups), NumSub, replace = TRUE, prob = vProbs )
                
                
                # Simulate the patient survival times based on the patient group and treatment
                for( nPatIndx in 1:NumSub)  
                {
                    nPatientTreatment     <- vTreatmentID[ nPatIndx ]
                    nPatientGroup         <- vPatientGroup[ nPatIndx ]
                    dRate                 <- mRates[ nPatientGroup, nPatientTreatment ]
                    vSurvTime[ nPatIndx ] <- rexp( 1, dRate )
                }
                ErrorCode    <- rep( 0, NumSub ) 
            }
            else
            {
                #ErrorCode <- 1
                ErrorCode <- ERROR1   # CAUSE_AN_ERROR is not defined so shoudl cause in error
                
            }
        }
        
    }
    else if(SurvMethod == 2)   # Cumulative % Survivals
    {
        ErrorCode <- ERROR1 # ERROR1 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
        
    }
    else if(SurvMethod == 3)   # Median Survival Times
    {
        ErrorCode <- ERROR3   # ERROR2 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
    }
    
    return(list(SurvivalTime = as.double(vSurvTime), ErrorCode = ErrorCode) )
}


