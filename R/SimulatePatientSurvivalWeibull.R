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
#'       \item {UserParam$dShapeCtrl} {The shape parameter in the Weibull distribution for the control treatment}  
#'       \item {UserParam$dScaleCtrl} {The scale parameter in the Weibull distribution for the control treatment}
#'       \item {UserParam$dShapeExp} {The shape parameter in the Weibull distribution for the experimental treatment}  
#'       \item {UserParam$dScaleExp} {The scale parameter in the Weibull distribution for the experimental treatment}
#'  }
#'  @description
#'  This function simulates patient data from a Weibull( shape, scale ) distribution.   The rweibull function in the stats package
#'  is used to simulate the survival time.  See help on rweibull.  The exponetial with mean = scale is a special case with the shape = 1   
#'  The required function signature for integration with East includes the SurvMethod, NumPrd, PrdTime and SurvParam which are ignored in this function
#'  and only the parameters in UserParam are utilized.  
#'  @export
SimulatePatientSurvivalWeibull<- function(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
{
    #TODO: Need to test that the paths that hit an error actually stop
 
    #It can often be helpful to save the objects that East send.   This can be done by setting the working directory to a location
    # of your choice and the using SaveRDS
    #setwd( "[ENTERED THE DESIRED LOCATION TO SAVE THE FILE]" )
    #saveRDS( NumSub, "NumSub.Rds")
    #saveRDS( NumArm, "NumArm.Rds" )
    #saveRDS( TreatmentID, "TreatmentID.Rds" )
    #saveRDS( SurvMethod, "SurvMethod.Rds" )
    #saveRDS( NumPrd, "NumPrd.Rds" )
    #saveRDS( SurvParam, "SurvParam.Rds" )
    #saveRDS( UserParam, "UserParam.Rds" )
    
    # Step 1 - Check the user parameters to verify the specify the shape and scale of each treatment ####
    # TODO - Check for input error and set an error if that is the case
    
    ErrorCode    <- 0
    
    
    # Step 2 - Create vectors with the parameters so they can be used more efficiently when simulating patient data ####
    vShape <- c( UserParam$dShapeCtrl, UserParam$dShapeExp )
    vScale <- c( UserParam$dScaleCtrl, UserParam$dScaleExp )
    
    # Step 3 - TreatmentID = 0 then it is control, 1 is experimental adding one since vectors are index by 1  ####
    vTreatmentID <- TreatmentID + 1   
    
    # Initialize the vector to store the patient surival times. 
    vSurvTime <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    
  
    # Step 4 - Simulate the patient survival times ####
    # Simulate the patient survival times based on the treatment
    for( nPatIndx in 1:NumSub)
    {
        nPatientTreatment     <- vTreatmentID[ nPatIndx ]
        vSurvTime[ nPatIndx ] <- rweibull( 1, vShape[ nPatientTreatment ], vScale[ nPatientTreatment ] )

    }

    lRet <- list(SurvivalTime = as.double( vSurvTime ), ErrorCode = ErrorCode)
    return( lRet )
}


