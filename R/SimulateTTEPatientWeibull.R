######################################################################################################################## .
#' @name SimulateTTEPatientWeibull
#' @title Simulate TTE Patient Outcomes from a Weibull Distribution
#'
#' @description
#' This function simulates patient data from a Weibull (shape, scale) distribution. The `rweibull` function in the `stats` package
#' is used to simulate the survival time. See help on `rweibull`.  
#'
#' The required function signature for integration with East or East Horizon includes the `SurvMethod`, `NumPrd`, `PrdTime`, and `SurvParam`, 
#' which are ignored in this function, and only the parameters in `UserParam` are utilized.
#'
#' @param NumSub The number of patient times to generate for the trial. This is a single numeric value, e.g., 250.
#' @param NumArm The number of arms in the trial, a single numeric value. For a two-arm trial, this will be 2.
#' @param TreatmentID A vector of treatment IDs. `0 = treatment 1`, `1 = treatment 2`. The length of `TreatmentID` must be equal to `NumSub`.
#' @param SurvMethod This value is pulled from the Input Method drop-down list. 
#'   \describe{
#'      \item{1}{Hazard Rate.}
#'      \item{2}{Cumulative percentage survival.}
#'      \item{3}{Medians.}
#'   }
#' @param NumPrd Number of time periods that are provided.
#' @param PrdTime 
#'   \describe{ 
#'      \item{If `SurvMethod = 1`}{`PrdTime` is a vector of starting times of hazard pieces.}
#'      \item{If `SurvMethod = 2`}{Times at which the cumulative percentage survivals are specified.}
#'      \item{If `SurvMethod = 3`}{`PrdTime` is 0 by default.}
#'   }
#' @param SurvParam A 2-D array of parameters to generate the survival times, depending on the table in the Response Generation tab.
#'   \describe{
#'      \item{If `SurvMethod = 1`}{`SurvParam` is an array (`NumPrd` rows, `NumArm` columns) that specifies arm-by-arm hazard rates 
#'      (one rate per arm per piece). Thus, `SurvParam[i, j]` specifies the hazard rate in the `i`th period for the `j`th arm. 
#'      Arms are in columns where column 1 is control and column 2 is experimental. Time periods are in rows, where row 1 is time period 1, row 2 is time period 2, etc.}
#'      \item{If `SurvMethod = 2`}{`SurvParam` is an array (`NumPrd` rows, `NumArm` columns) that specifies arm-by-arm the cumulative percentage survivals 
#'      (one value per arm per piece). Thus, `SurvParam[i, j]` specifies the cumulative percentage survivals in the `i`th period for the `j`th arm.}
#'      \item{If `SurvMethod = 3`}{`SurvParam` will be a `1 x 2` array with median survival times for each arm. Column 1 is control, column 2 is experimental.}
#'   }
#' @param UserParam A list of user-defined parameters. Must contain the following named elements:
#'   \describe{
#'      \item{`UserParam$dShapeCtrl`}{The shape parameter in the Weibull distribution for the control treatment.}  
#'      \item{`UserParam$dScaleCtrl`}{The scale parameter in the Weibull distribution for the control treatment.}
#'      \item{`UserParam$dShapeExp`}{The shape parameter in the Weibull distribution for the experimental treatment.}  
#'      \item{`UserParam$dScaleExp`}{The scale parameter in the Weibull distribution for the experimental treatment.}
#'   }
#'
#' @return A list with the following components:
#' \item{`SurvivalTime`}{A vector of simulated survival times for patients.}
#' \item{`ErrorCode`}{Optional integer value:
#'                      \describe{
#'                        \item{0}{No error.}
#'                        \item{> 0}{Non-fatal error; current simulation is aborted but subsequent simulations continue.}
#'                        \item{< 0}{Fatal error; no further simulations are attempted.}
#'                      }}
#' @export
######################################################################################################################## .

SimulateTTEPatientWeibull <- function( NumSub, NumArm, TreatmentID, SurvMethod, NumPrd, PrdTime, SurvParam, UserParam = NULL ) 
{
    # Create a fatal error when user parameters are missing to avoid misleading results
    vRequiredParams <- c( "dShapeCtrl", "dScaleCtrl", "dShapeExp", "dScaleExp" )
    vMissingParams <- vRequiredParams[ !vRequiredParams %in% names( UserParam ) ]
    
    if( is.null( UserParam ) || length( vMissingParams ) > 0 )
    {
        return( list( Response  = as.double( 0 ), 
                      ErrorCode = as.integer( -1 ) ) )
    }

    # Step 1 - Initialize the return variables or other variables needed ####    
    vSurvTime    <- rep( -1, NumSub )  # The vector of patient survival times that will be returned.  
    vTreatmentID <- TreatmentID + 1   # If this is 0 then it is control, 1 is treatment. Adding one since vectors are index by 1 
    ErrorCode    <- as.integer( 0 )
    
    # Step 2 - Read the user parameters into a vector to make it easier to simulate outcomes ####
    vShapes <- c( UserParam$dShapeCtrl, UserParam$dShapeExp )  
    vScales <- c( UserParam$dScaleCtrl, UserParam$dScaleExp ) 
    
    # Simulate the patient survival times based on the treatment
    # For the Hazard Rate input with 1 piece, this is just simulating from an exponential distribution as an example and results will match
    # East or East Horizon if you used the build hazard option.
    for( nPatIndx in 1:NumSub )
    {
        nPatientTreatment     <- vTreatmentID[ nPatIndx ]
        vSurvTime[ nPatIndx ] <- rweibull( 1, vShapes[ nPatientTreatment ], vScales[ nPatientTreatment ] )
        
    }

    return( list( SurvivalTime = as.double( vSurvTime ), ErrorCode = ErrorCode ) )
}