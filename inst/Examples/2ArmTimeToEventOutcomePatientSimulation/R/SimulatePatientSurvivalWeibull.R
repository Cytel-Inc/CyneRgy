#' Simulate patient outcomes from a Weibull distribution 
#' @param NumSub The number of patient times to generate for the trial.  This is a single numeric value, eg 250.
#' @param NumArm  The number of arms in the trial, a single numeric value.  For a two arm trial, this will be 2. 
#' @param TreatmentID A vector of treatment ids, 0 = treatment 1, 1 = Treatment 2, length( TreatmentID ) = NumSub
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
#' @param UserParam A list of user defined parameters in East or East Horizon. The default must be NULL.
#'  If UserParam is suppled it must contain the following:
#'  \describe{
#'       \item{UserParam$dShapeCtrl}{The shape parameter in the Weibull distribution for the control treatment}  
#'       \item{UserParam$dScaleCtrl}{The scale parameter in the Weibull distribution for the control treatment}
#'       \item{UserParam$dShapeExp}{The shape parameter in the Weibull distribution for the experimental treatment}  
#'       \item{UserParam$dScaleExp}{The scale parameter in the Weibull distribution for the experimental treatment}
#'  }
#'  @description
#'  This function simulates patient data from a Weibull( shape, scale ) distribution. The rweibull function in the stats package
#'  is used to simulate the survival time. See help on rweibull.  
#'  The required function signature for integration with East includes the SurvMethod, NumPrd, PrdTime and SurvParam which are ignored in this function
#'  and only the parameters in UserParam are utilized.  
#'  @export
SimulatePatientSurvivalWeibull<- function(NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
{
   

    # Step 1 - Initialize the return variables or other variables needed ####    
    vSurvTime    <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    vTreatmentID <- TreatmentID +1   # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1 
    ErrorCode    <- as.integer( 0 )
    
    # Step 2 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ) )
    {
        
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues. Also, if there is a default value for the parameters you may want to set them here. Default values usually
        # are applied to have the same functionality as East, see the first example
        
        # EXMAPLE - Set the default if needed
        UserParam <- list( dShapeCtrl = 1, dShapeExp = 12, dScaleCtrl = 1, dScaleExp = 12 )
    }
    
    # Step 2 - Read the user parameters into a vector to make it easier to simulate outcomes ####
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
