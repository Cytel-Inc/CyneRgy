#' Simulate time-to-event patient data from a Weibull distribution
# Parameter Description 
# NumSub - The number of patients (subjects) in the trial.  NumSub survival times need to be generated for the trial.  
#           This is a single numeric value, eg 250.
# NumArm - The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
# The SurvParam depends on input in East. In the simulation window on the Response Generation tab 
# SurvMethod - This values is pulled from the Input Method drop-down list.  
# SurvParam - Depends on the table in the Response Generation tab. 2‐D array of parameters uses to generate time of events.
# If SurvMethod is 1 (Hazard Rates):
#   SurvParam is an array that specifies arm by arm hazard rates (one rate per arm per piece). Thus SurvParam [i, j] specifies hazard rate in ith period for jth arm.
#   Arms are in columns with column 1 is control, column 2 is experimental
#   Time periods are in rows
# If SurvMethod is 2:
#   SurvParam is an array specifies arm by arm the Cum % Survivals (one value per arm per piece). Thus, SurvParam [i, j] specifies Cum % Survivals in ith period for jth arm.
# If SurvMethod is 3:
#   SurvParam will be a 1 x 2 array with median survival times on each arms. Column 1 is control, column 2 is experimental 
#  
# Description: This function simulates from exponential, just included as a simple example as a starting point 

#' @param NumSub NumSub - The number of patients (subjects) in the trial.  NumSub survival times need to be generated for the trial.  
#'           This is a single numeric value, eg 250.
#' @param NumArm The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
#'           The SurvParam depends on input in East. In the simulation window on the Response Generation tab 
#' @export
SimulatePatientSurvivalWeibull<- function(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
{
    #TODO: Need to test that the paths that hit an error actually stop
    
    # The SurvParam depends on input in East, EAST sends the Median (see the Simulation->Response Generation tab for what is sent)
    setwd( "C:\\Kyle\\Cytel\\Software\\East-R\\EastRExamples\\Examples\\2ArmTimeToEventOutcomePatientSimulation\\ExampleOutput")
 
    # If you wanted to save the input objects you could use the following to save the files to your working directory
    
    # Example of how to save the data sent from East for each look.
    # If the DesignParam.Rds exists, then don't save it again.
    if( !file.exists(  "DesignParam.Rds" ))
    {
        saveRDS( SurvParam, paste0( "SurvParam.Rds") )
        # Use the same function as previous line if you want to save the other objects
    }

    # For this example, in East the user must set the Input Method to Hazard Rate and have the # of pieces = 2. 
    # This will cause SurvParam to be a 2x2 matrix. 
    # For this example we will assume that column 1 are the 2 Weibull parameters for Standard of Care and column 2 are the two Weibull parameters for Experimental 
    
    vSurvTime <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    
    vTreatmentID <- TreatmentID +1   # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1 
    ErrorCode    <- rep( -1, NumSub ) 
    
    
    if(SurvMethod == 1)   # Hazard Rates
    {
        
        if( NumPrd == 2 )
        {
            vShapes <- SurvParam[ 1, ]   # Row 1 is the shape parameters
            vScales <- SurvParam[ 2, ]   # Row 2 is the scale parameters
            # Simulate the patient survival times based on the treatment
            # For the Hazard Rate input with 1 piece, this is just simulating from an exponential distribution as an example and results will match
            # East if you used the build hazard option.
            for( nPatIndx in 1:NumSub)
            {
                nPatientTreatment     <- vTreatmentID[ nPatIndx ]
                vSurvTime[ nPatIndx ] <- rweibull( 1, vShapes[ nPatientTreatment ], vScales[ nPatientTreatment ] )
                
            }
        }
        else 
        {
            ErrorCode <- ERROR1   # CAUSE_AN_ERROR is not defined so should cause in error
        }
        
    }
    else if(SurvMethod == 2)   # Cumulative % Survivals
    {
        ErrorCode <- ERROR1 # ERROR1 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
        
    }
    else if(SurvMethod == 3)   # Median Survival Times
    {
        ErrorCode <- ERROR3   # ERROR3 is not defined so this will cause an error if the users selects anything besides Hazard Rate and uses this function
    }
    
    return( list(SurvivalTime = as.double( vSurvTime ), ErrorCode = ErrorCode) )
}


