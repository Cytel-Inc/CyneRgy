#' @param SimulatePatientSurvivalMultiArmWeibull
#' @title Simulate patient time-to-event outcomes from a Weibull distribution for multi-arm trials
#' @param NumSub The number of patient times to generate for the trial. This is a single numeric value, eg 250.
#' @param NumArm  The number of arms in the trial, a single numeric value. For a two arm trial, this will be 2. 
#' @param ArrivalTime Arrival times of the subjects, numeric vector, length( ArrivalTime ) = NumSub.
#' @param TreatmentID A vector specifying the arm index for each subject. The index for the placebo/control arm is 0.
#' @param SurvMethod - This values is pulled from the Input Method drop-down list. This will be 1 (Hazard Rate), 2 (Cumulative % survival), 3 (Medians)
#' @param NumPrd Number of time periods that are provided. 
#' @param PrdTime \describe{ 
#'      \item{If SurvMethod = 1}{PrdTime is a vector of starting times of hazard pieces.}
#'      \item{If SurvMethod = 2}{Times at which the cumulative % survivals are specified.}
#'      \item{If SurvMethod = 3}{Period time is 0 by default}
#'      }
#' @param SurvParam \describe{Depends on the table in the Response Generation tab. 2‐D matrix of parameters to generate the survival times
#'    \item{If SurvMethod is 1}{SurvParam is a matrix (NumPrd rows, NumArm columns) that specifies arm by arm hazard rates (one rate per arm per piece). 
#'    Thus SurvParam [i, j] specifies hazard rate in ith period for jth arm.
#'    Arms are in columns with column 1 is control, column 2 is experimental arm 1, column 3 is experimental arm 2 and so on
#'    Time periods are in rows, row 1 is time period 1, row 2 is time period 2...}
#'    \item{If SurvMethod is 2}{SurvParam is a matrix (NumPrd rows,NumArm columns) that specifies arm by arm the Cum % Survivals (one value per arm per piece). Thus, SurvParam [i, j] specifies Cum % Survivals in ith period for jth arm.}
#'    \item{If SurvMethod is 3}{SurvParam will be a 1 x n matrix with median survival times on each arms. Column 1 is control and the rest are experimental arms.  }
#'  }
#' @param UserParam A list of user defined parameters in East Horizon. The default must be NULL.
#'  If UserParam is supplied it must contain the following:
#'  \describe{
#'       \item{UserParam$dShapeCtrl}{The shape parameter in the Weibull distribution for the control treatment}  
#'       \item{UserParam$dScaleCtrl}{The scale parameter in the Weibull distribution for the control treatment}
#'       \item{UserParam$dShapeExp1}{The shape parameter in the Weibull distribution for the experimental treatment 1}  
#'       \item{UserParam$dScaleExp1}{The scale parameter in the Weibull distribution for the experimental treatment 1}
#'       \item{UserParam$dShapeExp2}{The shape parameter in the Weibull distribution for the experimental treatment 2}  
#'       \item{UserParam$dScaleExp2}{The scale parameter in the Weibull distribution for the experimental treatment 2}
#'  }
#' @return The function must return a list in the return statement of the function. The information below lists 
#'             elements of the list, if the element is required or optional and a description of the return values if needed. 
#'             \describe{
#'             \item{SurvivalTime}{Required numeric value. A vector of generated time to response values for each subject.}
#'             \item{ErrorCode}{Optional integer value \describe{ 
#'                                     \item{ErrorCode = 0}{No Error}
#'                                     \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                                     \item{ErrorCode < 0}{Fatal error, no further simulation will be attempted}
#'                                     }
#'                                     }
#'             }
#'  @description
#'  This function simulates patient data from a Weibull( shape, scale ) distribution. The rweibull function in the stats package
#'  is used to simulate the survival time. See help on rweibull.  
#'  The required function signature for integration with East Horizon includes the SurvMethod, NumPrd, PrdTime and SurvParam which are ignored in this function
#'  and only the parameters in UserParam are utilized.  
#'  @export

SimulatePatientSurvivalMultiArmWeibull <- function( NumSub, NumArm, ArrivalTime, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
{
    # Step 1 - Initialize the return variables or other variables needed ####    
    vSurvTime    <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    vTreatmentID <- TreatmentID + 1    # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1 
    nErrorCode   <- as.integer( 0 )
    
    # Step 2 - Validate custom variable input and set defaults ####
    if( is.null( UserParam ))
    {
        
        # If this function requires user defined parameters to be sent via the UserParam variable check to make sure the values are valid and
        # take care of any issues. Also, if there is a default value for the parameters you may want to set them here. Default values usually
        # are applied to have the same functionality as East, see the first example
        
        # EXMAPLE - Set the default if needed
        UserParam <- list( dShapeCtrl = 1, dShapeExp1 = 12, dShapeExp2 = 12,  
                           dScaleCtrl = 1, dScaleExp1 = 12, dScaleExp2 = 12 )
    }
    
    # Step 2 - Read the user parameters into a vector to make it easier to simulate outcomes ####
    vShapes <- c( UserParam$dShapeCtrl, UserParam$dShapeExp1, UserParam$dShapeExp2 )  
    vScales <- c( UserParam$dScaleCtrl, UserParam$dScaleExp1, UserParam$dScaleExp2 ) 
    
    # Simulate the patient survival times based on the treatment
    # For the Hazard Rate input with 1 piece, this is just simulating from an exponential distribution as an example and results will match
    # East if you used the build hazard option.
    for( nPatIndx in 1:NumSub )
    {
        nPatientTreatment     <- vTreatmentID[ nPatIndx ]
        vSurvTime[ nPatIndx ] <- rweibull( 1, vShapes[ nPatientTreatment ], vScales[ nPatientTreatment ])
        
    }
    
    return( list( SurvivalTime = as.double( vSurvTime ), ErrorCode = nErrorCode ))
}
