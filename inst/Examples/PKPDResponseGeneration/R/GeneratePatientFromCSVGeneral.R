######################################################################################################################## .
#' @name GeneratePatientFromCSVGeneral
#' @title Generate Patient Responses from CSV File with Flexible Formatting
#' @description
#' This function reads pre-simulated patient data from a CSV file and returns visit-level responses for trial subjects.
#' It accepts flexible column naming conventions and treatment identifiers (case-insensitive), making it compatible with
#' various CSV formatting styles. Use this function when you have externally generated patient data (e.g., from complex
#' PK/PD models) that you want to integrate into your trial simulation. For faster performance with stricter formatting
#' requirements, see GeneratePatientFromCSVSpecific.R.
#'
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#' @param NumSub The number of subjects to simulate, integer value.
#' @param NumVisit The number of visits, integer value.
#' @param TreatmentID A vector of treatment IDs. `0 = control`, `1 = treatment`. The length of `TreatmentID` must equal `NumSub`.
#' @param Inputmethod Method for specifying input parameters (passed from East Horizon, not used in this function).
#' @param VisitTime Numeric vector of visit times (passed from East Horizon, not used in this function).
#' @param MeanControl Numeric vector of control means for all visits (passed from East Horizon, not used in this function).
#' @param MeanTrt Numeric vector of treatment means for all visits (passed from East Horizon, not used in this function).
#' @param StdDevControl Numeric vector of control standard deviations for all visits (passed from East Horizon, not used in this function).
#' @param StdDevTrt Numeric vector of treatment standard deviations for all visits (passed from East Horizon, not used in this function).
#' @param CorrMat Correlation matrix between all visits (passed from East Horizon, not used in this function).
#' @param UserParam A list of user defined parameters in East Horizon. You must have a default = NULL, as in this example. If UserParam values are supplied in East Horizon, they will be elements of the list, e.g., UserParam$ParameterName.
#'   \describe{
#'      \item{`UserParam$InputFileName`}{The name of the CSV file in the Inputs folder (e.g., "SimPatientDataAlt.csv").}
#'   }
#'
#' @details
#' The CSV file must contain:
#' - A Treatment column with treatment assignments (accepts "Treatment", "TreatmentID", "Trt" or "TrtID" format, case-insensitive)
#' - Visit columns for each visit (accepts "Visit X", "Visit.X", or "VisitX" format, case-insensitive)
#'
#' Accepted Treatment Identifiers:
#' - Control: "0", "c", "ctl", "control", "placebo", "cntl" (case-insensitive)
#' - Treatment: "1", "t", "trt", "treatment", "active" (case-insensitive)
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
#'                        \item{-6}{Specific visit column not found in CSV.}
#'                      }}
######################################################################################################################## .

GeneratePatientFromCSVGeneral <- function( NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL )
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
    if( !exists( "gdfPatients" ) )
    {
        dfPatients <- tryCatch(
        {
            read.csv( strCSVPath, check.names = FALSE, stringsAsFactors = FALSE )
        }, error = function( e )
        {
            NULL
        } )

        gdfPatients <<- dfPatients
    }
    else
    {
        dfPatients <- gdfPatients
    }

    if( is.null( dfPatients ) )
    {
        nError <<- -2
        lReturn$ErrorCode <- as.integer( nError )
        return( lReturn )
    }

    # Ensure Treatment column exists and is 0/1
    vColNames   <- colnames( dfPatients )
    vNormCol    <- NormalizeName( vColNames )
    strTreatCol <- GetColInsensitive( c( "Treatment", "TreatmentID", "Trt", "TrtID" ), vNormCol, vColNames )

    if( is.na( strTreatCol ) )
    {
        nError <- -3
        lReturn$ErrorCode <- as.integer( nError )
        return( lReturn )
    }

    vTrt <- vapply( dfPatients[[ strTreatCol ] ], CoerceGroup01, integer( 1 ) )

    if( any( is.na( vTr ) ) )
    {
        vNum <- suppressWarnings( as.integer( dfPatients[[ strTreatCol ] ] ) )
        vTrt[ is.na( vTr ) ] <- vNum[ is.na( vTr ) ]
    }
    vKeep <- vTrt %in% c( 0, 1 )
    dfPatients <- dfPatients[ vKeep, , drop = FALSE ]
    dfPatients[[ strTreatCol ] ] <- as.integer( vTrt[ vKeep ] )

    # Identify visit columns and coerce to numeric
    # Find normalized names that match visit pattern, then get actual column names
    vNormVisit <- vNormCol[ grepl( "^visit[0-9]+$", vNormCol ) ]

    if( length( vNormVisit ) == 0 )
    {
        # Fallback: find any column starting with "visit" (case-insensitive)
        vNormVisit <- vNormCol[ grepl( "^visit", vNormCol ) ]
    }
    # Get actual column names (not normalized) for the visit columns
    vVisitCols <- vColNames[ vNormCol %in% vNormVisit ]

    # Check if there are enough visit columns for the requested number of visits
    if( length( vVisitCols ) < NumVisit )
    {
        nError <- -4
        lReturn$ErrorCode <- as.integer( nError )
        return( lReturn )
    }

    # Convert all visit columns to numeric using actual column names
    for( strCol in vVisitCols )
    {
        dfPatients[[ strCol ] ] <- CoerceVisitNumeric( dfPatients[[ strCol ] ] )
    }

    # Per-arm availability
    nNeedCtl <- sum( as.integer( TreatmentID ) == 0 )
    nNeedTrt <- sum( as.integer( TreatmentID ) == 1 )

    # Create a vector of indexes for the control patients and treatment patients
    vIdxCtrl <- which( dfPatients[[ strTreatCol ] ] == 0 )
    vIdxTrt  <- which( dfPatients[[ strTreatCol ] ] == 1 )

    if( length( vIdxCtrl ) < nNeedCtl || length( vIdxTrt ) < nNeedTrt )
    {
        # If there are not enough control or treatment patients then this is an error
        nError <- -5
        lReturn$ErrorCode <- as.integer( nError )
        return( lReturn )
    }

    # Sample unique rows per arm (no replacement), then map to subjects in East Horizon order
    vTakeCtrl <- integer( 0 )
    if( nNeedCtl > 0 )
        vTakeCtrl <- sample( vIdxCtrl, nNeedCtl, replace = FALSE )

    vTakeTrt  <- integer( 0 )
    if( nNeedTrt > 0 )
        vTakeTrt  <- sample( vIdxTrt, nNeedTrt, replace = FALSE )

    # vPick will contain the index of the patient to use from the CSV that was treated with TreatmentID
    vPick <- integer( NumSub )
    nCtl  <- 0  # Index for which control patient to select
    nTrt  <- 0  # Index for which treatment patient to select

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

    # Build Response1..ResponseK (numeric) directly from selected rows
    for( iVisit in seq_len( NumVisit ) )
    {
        vCandidates <- c( paste0( "visit", iVisit ),
                  paste0( "visit ", iVisit ),
                  paste0( "visit.", iVisit ) )
        strFound <- GetColInsensitive( vCandidates, vNormCol, vColNames )

        if( is.na( strFound ) )
        {
            nError <- -6
            lReturn$ErrorCode <- as.integer( nError )
            return( lReturn )
        }
        lReturn[[ paste0( "Response", iVisit ) ] ] <- as.double( dfPatients[ vPick, strFound ] )
    }

    lReturn$ErrorCode <- as.integer( nError )
    return( lReturn )
}

# ---------------- Local helpers ----------------

# Case/space/underscore/dot?insensitive column matching (for column names only)
NormalizeName <- function( str )
{
    vStr <- tolower( as.character( str ) )
    vStr <- gsub( "[[:space:]_.]+", "", vStr )

    return( vStr )
}

GetColInsensitive <- function( vCandidates, vNormCol, vColNames )
{
    vNormCand <- NormalizeName( vCandidates )
    for( i in seq_along( vNormCand ) )
    {
        iMatch <- match( vNormCand[ i ], vNormCol )
        if( !is.na( iMatch ) ) return( vColNames[ iMatch ] )
    }

    return( NA_character_ )
}

CoerceGroup01 <- function( x )
{
    v <- suppressWarnings( as.numeric( x ) )
    if( !is.na( v ) )
    {
        if( v == 0 ) return( 0 )
        if( v == 1 ) return( 1 )
    }
    str <- tolower( trimws( as.character( x ) ) )
    if( str %in% c( "0", "c", "ctl", "control", "placebo", "cntl" ) )
    {
        return( 0 )
    }
    if( str %in% c( "1", "t", "trt", "treatment", "active" ) )
    {
        return( 1 )
    }

    return( NA_integer_ )
}

CoerceVisitNumeric <- function( v )
{
    vChr <- as.character( v )
    vChr[ vChr %in% c( "", "NA", "NaN", "na", "null", "N/A" ) ] <- NA_character_

    return( suppressWarnings( as.double( vChr ) ) )
}
