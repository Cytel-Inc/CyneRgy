######################################################################################################################## .
#' @name SimulatePatientOutcomeDoseFinding.Continuous
#' @title Simulate continuous patient responses using dose-response models
#' @description Simulates continuous patient-level responses in a multi-arm clinical trial
#' using a user-selected dose-response model.
#' The user can select one of five models: Linear, Emax, SigEmax, Exponential, or Beta, and 
#' specify the corresponding model parameters through UserParam.
#' The expected mean response for each treatment arm is calculated from the selected dose-response 
#' model and the user-defined dose levels. Individual patient responses are then generated from a 
#' normal distribution using the model-derived mean and the arm-specific standard deviation provided 
#' by East Horizon.
#' @author Julija Saltane
#' @param NumSub Integer number of subjects in the trial.
#' @param NumArms Integer number of arms in the trial, including the placebo/control arm and all experimental arms.
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#' @param Mean Numeric vector of mean responses by arm, with the control arm first, followed by experimental arms
#'   in TreatmentID order.
#' @param StdDev Numeric vector of response standard deviations by arm, with the control arm first, followed by
#'   experimental arms in TreatmentID order.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' If UserParam is supplied, the list must contain the following named elements:
#'   \describe{
#'     \item{Dose1, Dose2, ..., DoseN}{ Positive numeric dose levels for the experimental treatment
#'       arms, where N = NumArms - 1. Dose1 corresponds to TreatmentID = 1, Dose2 to TreatmentID = 2,etc. 
#'       The placebo dose is automatically set to 0. Dose values must be unique.}
#'     \item{Model}{Character string specifying the dose-response model. Supported values: "Linear", 
#'     "Emax", "SigEmax", "Exponential", and "Beta" (case-insensitive).}
#'     \item{E0}{Numeric baseline mean response at dose 0 (placebo/control).}
#'     \item{EMax}{Numeric parameter controlling the treatment effect. Its precise interpretation 
#'     depends on the selected model.}
#'   }
#'
#' Additional model-specific parameters:
#'   \describe{
#'     \item{Linear}{No additional parameters required. The slope is calculated from E0, EMax, and the dose range.}
#'     \item{Emax}{ED50: Positive numeric dose producing half of the maximum treatment effect.}
#'     \item{SigEmax}{ED50: Positive numeric dose producing half of the maximum treatment effect.
#'                    Hill: Positive numeric parameter controlling the steepness of the dose-response curve.}
#'     \item{Exponential}{Delta: Positive numeric parameter controlling the curvature of the exponential dose-response relationship.}
#'     \item{Beta}{D1: Positive numeric first shape parameter.
#'                 D2: Positive numeric second shape parameter.
#'                 Scale: Positive numeric dose-scaling parameter, which must be at least as large as the maximum dose.}
#'   }
#'
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response}{Numeric vector of generated subject responses, with one element per subject.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#' @details This example supports control and any number of experimental arms. The number of user-defined dose parameters must equal NumArms - 1.
#' For example, a four-arm trial requires Dose1, Dose2, and Dose3.The selected dose-response model determines the expected mean response for each arm. 
#' Individual patient responses are generated independently according to normal distribution.

######################################################################################################################## .

SimulatePatientOutcomeDoseFinding.Continuous <- function( NumSub, NumArms, ArrivalTime, TreatmentID, Mean, StdDev, UserParam = NULL )
{
    library( DoseFinding )
    
    nError <- 0 # No errors occurred
    vResponse <- numeric( NumSub ) # Initialize the vector of patient outcomes as 0 so only the patients that do NOT have a zero response will be simulated
    
    # Ensure UserParam is not null
    if ( is.null( UserParam ) ) {
        return( list( Response = as.double( vResponse ), 
                      ErrorCode = as.integer( -1 ) ) )
    }
    
    # Step 1: Validate simulation inputs ####
    
    # Check that Dose1, Dose2, ..., Dose(n) exist, are consecutive and are consistent with the number of arms
    vDoseNames   <- names( UserParam )[ grepl( "^Dose", names( UserParam ) ) ]
    
    # Extract dose numbers and sort numerically
    vDoseNumbers <- as.integer( sub( "^Dose", "", vDoseNames ) )
    vDoseOrder   <- order( vDoseNumbers )
    
    vDoseNames   <- vDoseNames[ vDoseOrder ]
    vDoseNumbers <- vDoseNumbers[ vDoseOrder ]
    
    if ( !identical( vDoseNumbers, seq_len( NumArms - 1 ) ) ) {
        return( list( Response = as.double( vResponse ), 
                      ErrorCode = as.integer( -2 ) ) )
    }
    
    # Check if all doses are valid numeric values, all treatment doses must be positive numeric values.
    vDoseValues <- unlist( UserParam[ vDoseNames ], use.names = FALSE )
    vTrtDoses <- suppressWarnings( as.numeric( vDoseValues ) )
    
    if ( length( vTrtDoses ) != NumArms - 1 || any( is.na( vTrtDoses ) ) || any( !is.finite( vTrtDoses ) ) || any( vTrtDoses <= 0 ) ) {
        return( list( Response = as.double( vResponse ), 
                      ErrorCode = as.integer( -3 ) ) )
    }
    
    vDoses <- c( 0, vTrtDoses )
    
    # Check there are no duplicate doses
    if ( anyDuplicated( vDoses ) ) {
        return( list( Response = as.double( vResponse ), 
                      ErrorCode = as.integer( -4 ) ) )
    }
    
    # Check the required parameters
    vRequiredParams <- c( vDoseNames, "Model", "E0", "EMax" )
    
    vMissingParams <- setdiff( vRequiredParams, names( UserParam ) )
    
    if ( length( vMissingParams ) > 0) {
        return( list( Response = as.double( vResponse ), 
                      ErrorCode = as.integer( -5 ) ) )
    }
    
    # Checking model for validity
    cModelName <- tolower( UserParam$Model )
    
    vSupportedModels <- c( "linear",
                           "emax",
                           "sigemax",
                           "exponential",
                           "beta" )
    
    if ( !( cModelName %in% vSupportedModels ) ) {
        return( list( Response = as.double( vResponse ), 
                      ErrorCode = as.integer( -6 ) ) )
    }
    
    # Checking model parameters
    vModelRequiredParams <- switch( cModelName,
                                    linear      = character( 0 ),
                                    emax        = c( "ED50" ),
                                    sigemax     = c( "ED50", "Hill" ),
                                    exponential = c( "Delta" ),
                                    beta        = c( "D1", "D2", "Scale" )
    )
    
    vRequiredParams <- c( vRequiredParams, vModelRequiredParams )
    vMissingParams  <- setdiff( vRequiredParams, names( UserParam ) )
    
    if ( length( vMissingParams ) > 0 ) {
        return( list( Response = as.double( vResponse ), 
                      ErrorCode = as.integer( -7 ) ) )
    }
    
    # Check scalar model parameters
    vScalarModelRequiredParams <- c( "E0", "EMax", vModelRequiredParams )
    
    for ( dScalarParam in vScalarModelRequiredParams ) {
        dValue <- UserParam[[ dScalarParam ]]
        if ( !is.numeric( dValue ) || length( dValue ) != 1 || !is.finite( dValue ) ) {
            return( list( Response = as.double( vResponse ), 
                          ErrorCode = as.integer( -8 ) ) )
        }
    }
    
    E0   <- UserParam$E0
    EMax <- UserParam$EMax
    Dose1 <- UserParam$Dose1
    Dose2 <- UserParam$Dose2
    
    # Validate model-specific constraints
    if ( cModelName %in% c( "emax", "sigemax" ) && UserParam[[ "ED50" ]] <= 0 ) {
        return( list( Response = as.double( vResponse ), 
                      ErrorCode = as.integer( -9 ) ) )
    }
    
    if ( cModelName == "sigemax" && UserParam[[ "Hill" ]] <= 0 ) {
        return( list( Response = as.double( vResponse ), 
                      ErrorCode = as.integer( -10 ) ) )
    }
    
    if ( cModelName == "exponential" && UserParam[[ "Delta" ]] <= 0 ) {
        return( list( Response = as.double( vResponse ), 
                      ErrorCode = as.integer( -11 ) ) )
    }
    
    # Step 2: Prepare dose and model information ####
    
    lModelMeans <- GenerateMeans( Doses = vDoses, UserParam = UserParam )
    vModelMeans <- lModelMeans$vModelMeans
    nError      <- lModelMeans$nError
    
    if ( nError < 0 ) {
        return( list( Response = as.double( vResponse ), 
                      ErrorCode = as.integer( -12 ) ) )
    }
    
    # Step 3: Simulate patient-level responses ####
    
    for ( nArm in seq_len( NumArms ) ) {
        
        nTrID     <- nArm - 1
        nPatIndx  <- which( TreatmentID == nTrID )
        nPatients <- length( nPatIndx )
        
        vResponse[nPatIndx] <- rnorm( n = nPatients,
                                      mean = vModelMeans[ nArm ],
                                      sd = StdDev[ nArm ] )
    }
    
    # Step 4: Return simulation results ####
    
    return( list( Response = as.double( vResponse ), ErrorCode = as.integer( nError ) ) )
}


GenerateMeans <- function( Doses, UserParam) {
    
    vModelMeans <- rep( 0, length( Doses ) )
    nError      <- 0
    
    cModelName <- tolower( UserParam$Model )
    
    if ( cModelName == "linear") {
        
        Slope <- ( UserParam$EMax - UserParam$E0 ) / ( tail( Doses, 1 ) - Doses[ 1 ] )
        
        # Mean(d) = E0 + Slope * d
        vModelMeans <- UserParam$E0 + Slope * Doses
        
    } else if ( cModelName == "emax" ) {
        
        # Mean(d) = E0 + EMax * d / (ED50 + d)
        vModelMeans <- DoseFinding:::emax( dose = Doses,
                                           e0   = UserParam$E0,
                                           eMax = UserParam$EMax,
                                           ed50 = UserParam$ED50 )
        
    } else if ( cModelName == "sigemax" ) {
        
        # Mean(d) = E0 + EMax * d^h / (ED50^h + d^h)
        vModelMeans <- DoseFinding:::sigEmax( dose = Doses,
                                              e0   = UserParam$E0,
                                              eMax = UserParam$EMax,
                                              ed50 = UserParam$ED50,
                                              h    = UserParam$Hill )
        
    } else if ( cModelName == "exponential" ) {
        
        # Mean(d) = E0 + EMax *(exp(d/Delta)-1) / (exp(maxDose/Delta)-1)
        dMaxDose    <- max(Doses)
        vModelMeans <- UserParam$E0 + UserParam$EMax * (exp(Doses / UserParam$Delta) - 1) /
            (exp(dMaxDose / UserParam$Delta) - 1)
        
    } else if ( cModelName == "beta" ) {
        
        vX     <- Doses / UserParam$Scale
        vXstar <- UserParam$D1 / ( UserParam$D1 + UserParam$D2 )
        
        vModelMeans <- UserParam$E0 + UserParam$EMax * ( vX^UserParam$D1 * ( 1 - vX )^UserParam$D2 ) / ( vXstar^UserParam$D1 * ( 1 - vXstar )^UserParam$D2 )
        
    } else {
        nError = -10
    }
    
    if ( any( !is.finite( vModelMeans ) ) ) {
        nError = -20
    }
    
    return( list ( vModelMeans = as.numeric( vModelMeans ), nError = as.numeric( nError ) ) )
}
