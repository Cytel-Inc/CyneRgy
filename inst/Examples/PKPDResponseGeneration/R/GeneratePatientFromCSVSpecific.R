######################################################################################################################## .
#' @name GeneratePatientFromCSVSpecific
#' @title Generate Patient Responses from CSV File with Strict Formatting (Faster)
#' @description
#' This function reads pre-simulated patient data from a CSV file and returns visit-level responses for trial subjects.
#' It requires strict CSV formatting (specific column names and treatment identifiers) but runs faster than
#' GeneratePatientFromCSVGeneral. Use this function when you have control over CSV formatting and want
#' optimal performance. For more flexible formatting support, see GeneratePatientFromCSVGeneral.R.
#'
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#' @param NumSub The number of subjects to simulate, integer value.
#' @param NumVisit The number of visits, integer value.
#' @param TreatmentID A vector of treatment IDs. `0 = control`, `1 = treatment`. The length of `TreatmentID` must equal `NumSub`.
#' @param Inputmethod Method for specifying input parameters (passed from East/East Horizon, not used in this function).
#' @param VisitTime Numeric vector of visit times (passed from East/East Horizon, not used in this function).
#' @param MeanControl Numeric vector of control means for all visits (passed from East/East Horizon, not used in this function).
#' @param MeanTrt Numeric vector of treatment means for all visits (passed from East/East Horizon, not used in this function).
#' @param StdDevControl Numeric vector of control standard deviations for all visits (passed from East/East Horizon, not used in this function).
#' @param StdDevTrt Numeric vector of treatment standard deviations for all visits (passed from East/East Horizon, not used in this function).
#' @param CorrMat Correlation matrix between all visits (passed from East/East Horizon, not used in this function).
#' @param UserParam A list of user-defined parameters. Must contain the following named element:
#'     \describe{
#'            \item{`UserParam$InputFileName`}{The name of the CSV file in the Inputs folder (e.g., "SimPatientDataAlt.csv").}
#'     }
#'
#' @details
#' The CSV file must contain:
#' - A **Treatment column (exact name, case-sensitive) with treatment assignments
#' - Visit columns named exactly "Visit 1", "Visit 2", etc. (with space, case-sensitive)
#'
#' Accepted Treatment Identifiers:
#' - Control: "0" (must be integer zero or string "0")
#' - Treatment: "1" (must be integer one or string "1")
#'
#' Missing Values: "", "NA", "NaN", "na", "null", "N/A" are recognized as missing
#'
#' The function caches the CSV data globally (`gdfPatients`) for efficiency across multiple function calls.
#' Patients are randomly sampled without replacement from each treatment arm, ensuring unique patient
#' assignments within each simulation replicate.
#'
#' @return A list with the following components:
#' \item{`Response1`, `Response2`, ...}{Numeric vectors of patient responses for each visit.}
#' \item{`ErrorCode`}{Integer value:
#'                      \describe{
#'                        \item{0}{No error.}
#'                        \item{-1}{CSV file not found.}
#'                        \item{-2}{Error reading CSV file.}
#'                        \item{-3}{Treatment column not found.}
#'                        \item{-4}{Insufficient visit columns in CSV.}
#'                        \item{-5}{Insufficient patients in CSV for one or both arms.}
#'                      }}
######################################################################################################################## .

GeneratePatientFromCSVSpecific <- function( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL )
{
    # Initialize return variables and error code
    nError  <- 0
    lReturn <- list()

    # Build CSV path and confirm it exists
    strCSVPath <- paste0( "Inputs/", UserParam$InputFileName )

    if( !file.exists( strCSVPath ) )
    {
        nError <- -1
        lReturn$ErrorCode <- as.integer( nError )
        return( lReturn )
    }

    # Cache CSV across calls if available
    if( !exists( "gdfPatients", envir = .GlobalEnv ) )
    {
        dfPatients <- tryCatch( {
            read.csv( strCSVPath, check.names = FALSE, stringsAsFactors = FALSE )
        }, error = function( e ) {
            NULL
        } )
        gdfPatients <<- dfPatients
    }
    else
    {
        dfPatients <- get( "gdfPatients", envir = .GlobalEnv )
    }

    if( is.null( dfPatients ) )
    {
        nError <- -2
        lReturn$ErrorCode <- as.integer( nError )
        return( lReturn )
    }

    # Check required Treatment column (strict match)
    if( !( "Treatment" %in% colnames( dfPatients ) ) )
    {
        nError <- -3
        lReturn$ErrorCode <- as.integer( nError )
        return( lReturn )
    }

    # Coerce Treatment column strictly to integer 0/1
    vTrt <- suppressWarnings( as.integer( dfPatients[["Treatment" ] ] ) )
    vKeep <- !is.na( vTr ) & vTrt %in% c( 0, 1 )
    dfPatients <- dfPatients[ vKeep, , drop = FALSE ]
    dfPatients[["Treatment" ] ] <- vTrt[ vKeep ]

    # Validate and coerce Visit columns (Visit1..VisitK)
    vVisitCols <- paste0( "Visit ", seq_len( NumVisit ) )
    if( !all( vVisitCols %in% colnames( dfPatients ) ) )
    {
        nError <- -4
        lReturn$ErrorCode <- as.integer( nError )
        return( lReturn )
    }

    for( strCol in vVisitCols )
    {
        xChr <- as.character( dfPatients[[ strCol ] ] )
        xChr[ xChr %in% c( "", "NA", "NaN", "na", "null", "N/A" ) ] <- NA_character_
        dfPatients[[strCol ] ] <- suppressWarnings( as.double( xChr ) )
    }

    # Determine how many patients needed for each arm
    nNeedCtl <- sum( as.integer( TreatmentID ) == 0 )
    nNeedTrt <- sum( as.integer( TreatmentID ) == 1 )

    vIdxCtrl <- which( dfPatients[["Treatment" ] ] == 0 )
    vIdxTrt  <- which( dfPatients[["Treatment" ] ] == 1 )

    if( length( vIdxCtrl ) < nNeedCtl || length( vIdxTrt ) < nNeedTrt )
    {
        nError <- -5
        lReturn$ErrorCode <- as.integer( nError )
        return( lReturn )
    }

    # Randomly select unique patient rows per treatment arm
    vTakeCtrl <- if( nNeedCtl > 0 ) sample( vIdxCtrl, nNeedCtl, replace = FALSE ) else integer( 0 )
    vTakeTrt  <- if( nNeedTrt > 0 ) sample( vIdxTrt, nNeedTrt, replace = FALSE ) else integer( 0 )

    # Map selected patients to subjects by requested treatment order
    vPick <- integer( NumSub )
    nCtl  <- 0
    nTrt  <- 0

    for( iSub in seq_len( NumSub ) )
    {
        if( as.integer( TreatmentID[ iSub ] ) == 0 )
        {
            nCtl <- nCtl + 1
            vPick[ iSub ] <- vTakeCtrl[ nCtl ]
        }
        else
        {
            nTrt <- nTrt + 1
            vPick[ iSub ] <- vTakeTrt[ nTrt ]
        }
    }

    # Build Response1..ResponseK values for each subject
    for( iVisit in seq_len( NumVisit ) )
    {
        lReturn[[ paste0( "Response", iVisit ) ] ] <- as.double( dfPatients[ vPick, vVisitCols[ iVisit ] ] )
    }

    # Return assembled output with error code
    lReturn$ErrorCode <- as.integer( nError )
    return( lReturn )
}
