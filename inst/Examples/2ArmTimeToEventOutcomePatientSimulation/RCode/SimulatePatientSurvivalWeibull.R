#' Simulate patient outcomes from a Weibull distribution 
#' @param NumSub The number of subjects that need to be simulated, integer value. NumSub survival times need to be generated for the trial.  
#'           This is a single numeric value, eg 250.
#' @param NumArm - The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2. length( TreatmentID ) = NumSub
#' @param SurvMethod If SurvMethod is 1 (Hazard Rates):
#'   SurvParam is an array that specifies arm by arm hazard rates (one rate per arm per piece). Thus SurvParam [i, j] specifies hazard rate in ith period for jth arm.
#'   Arms are in columns with column 1 is control, column 2 is experimental
#'   Time periods are in rows
#'   If SurvMethod is 2:
#'         SurvParam is an array specifies arm by arm the Cum % Survivals (one value per arm per piece). Thus, SurvParam [i, j] specifies Cum % Survivals in ith period for jth arm.
#'   If SurvMethod is 3:
#'         SurvParam will be a 1 x 2 array with median survival times on each arms. Column 1 is control, column 2 is experimental 
#' @param NumPrd The number of periods in the East input.
#' @param PrdTime TODO - Get this from East
#' @param SurvParam - Depends on the table in the Response Generation tab. 2‐D array of parameters uses to generate time of events.
#' @param UserParam A list of user defined parameters in East. The default must be NULL.
#'  If UserParam is supplied, TODO What parameter are we sending and what are we using:
#'  \describe{
#'       \item{UserParam$dShapeCtrl}{The shape parameter in the Weibull distribution for the control treatment}  
#'       \item{UserParam$dScaleCtrl}{The scale parameter in the Weibull distribution for the control treatment}
#'       \item{UserParam$dShapeExp}{The shape parameter in the Weibull distribution for the experimental treatment}  
#'       \item{UserParam$dScaleExp}{The scale parameter in the Weibull distribution for the experimental treatment}
#'  }
#'  @description
#'  This function simulates patient data from a Weibull( shape, scale ) distribution.   The rweibull function in the stats package
#'  is used to simulate the survival time.  See help on rweibull.  
#'  The required function signature for integration with East includes the SurvMethod, NumPrd, PrdTime and SurvParam which are ignored in this function
#'  and only the parameters in UserParam are utilized.  
#'  @export
SimulatePatientSurvivalWeibull<- function(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
{
    #TODO: Need to test that the paths that hit an error actually stop
    
    # The SurvParam depends on input in East, EAST sends the Median (see the Simulation->Response Generation tab for what is sent)
    # setwd( "C:\\Kyle\\Cytel\\Software\\CyneRgy\\inst\\Examples\\2ArmTimeToEventOutcomePatientSimulation\\ExampleOutput")
    # saveRDS( UserParam, "UserParams.Rds")
    # saveRDS( TreatmentID, "TreatmentID.Rds")
    # If you wanted to save the input objects you could use the following to save the files to your working directory
    
    # Example of how to save the data sent from East for each look.
    # If the DesignParam.Rds exists, then don't save it again.
    if( !file.exists(  "DesignParam.Rds" ))
    {
        #saveRDS( SurvParam, paste0( "SurvParam.Rds") )
        # Use the same function as previous line if you want to save the other objects
    }

    # For this example, in East the user must set the Input Method to Hazard Rate and have the # of pieces = 2. 
    # This will cause SurvParam to be a 2x2 matrix. 
    # For this example we will assume that column 1 are the 2 Weibull parameters for Standard of Care and column 2 are the two Weibull parameters for Experimental 
    
    vSurvTime    <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    vTreatmentID <- TreatmentID +1   # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1 
    ErrorCode    <- rep( -1, NumSub ) 
    

    vShapes <- c( UserParam$dShapeCtrl, UserParam$dShapeExp )  
    vScales <- c( UserParam$dScaleCtrl, UserParam$dScaleExp ) 
    
    # Simulate the patient survival times based on the treatment
    # For the Hazard Rate input with 1 piece, this is just simulating from an exponential distribution as an example and results will match
    # East if you used the build hazard option.
    for( nPatIndx in 1:NumSub)
    {
        nPatientTreatment     <- vTreatmentID[ nPatIndx ]
        vSurvTime[ nPatIndx ] <- rweibull( 1, vShapes[ nPatientTreatment ], vScales[ nPatientTreatment ] )
        
    }
        

    
    return( list(SurvivalTime = as.double( vSurvTime ), ErrorCode = ErrorCode) )
}


