######################################################################################################################## .
#' @name GeneratePatientFromCSVGeneral
#' @title Generate Patient Responses from CSV File with Flexible Formatting
#' @description This function reads pre-simulated patient data from a CSV file and returns visit-level responses
#'   for trial subjects. It accepts flexible column naming conventions and treatment identifiers
#'   (case-insensitive), making it compatible with various CSV formatting styles. Use this function when you have
#'   externally generated patient data (e.g., from complex PK/PD models) that you want to integrate into your trial
#'   simulation. For faster performance with stricter formatting requirements, see
#'   GeneratePatientFromCSVSpecific.R.
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#' @param NumSub Integer number of subjects in the trial.
#' @param NumVisit Integer number of visits.
#' @param ArrivalTime Numeric vector of subject arrival times on the calendar scale, with one element per subject,
#'   in the same order as TreatmentID.
#' @param TreatmentID Integer vector of treatment assignments, with one element per subject: 0 = placebo/control, 1
#'   = first experimental arm, 2 = second experimental arm, and so on.
#' @param Inputmethod Integer input method: 0 = actual means and standard deviations at each visit; 1 = expected
#'   changes from baseline at each visit. Preserve this engine-supplied spelling.
#' @param VisitTime Numeric vector of visit times measured from enrollment, of length NumVisit and ordered by
#'   visit.
#' @param MeanControl Numeric vector of control-arm mean responses of length NumVisit, ordered by visit.
#' @param MeanTrt Numeric vector of experimental-arm mean responses of length NumVisit, ordered by visit.
#' @param StdDevControl Numeric vector of control-arm response standard deviations of length NumVisit, ordered by
#'   visit.
#' @param StdDevTrt Numeric vector of experimental-arm response standard deviations of length NumVisit, ordered by
#'   visit.
#' @param CorrMat Numeric correlation matrix between visits, with NumVisit rows and NumVisit columns.
#' @param UserParam Optional named list of user-defined parameters supplied through East Horizon. The default is
#'   NULL. Access elements by name, for example `UserParam$ParameterName`, rather than by position. User-defined
#'   scalar parameters may be integer, numeric, or character values. Pass the individual named elements to helper
#'   functions so East Horizon can identify and populate the required parameters.
#'
#' Example-specific parameters and requirements:
#' \describe{
#'   \item{UserParam$InputFileName}{Required character name of the CSV file in the Inputs folder, for example
#'     "SimPatientDataAlt.csv".}
#'   }
#' @return Named list containing the generated responses and optional ErrorCode execution status. Additional
#'   custom outputs may also be included.
#' \describe{
#'   \item{Response1, ..., ResponseNumVisit}{Numeric response vectors, one per visit, with one element per subject.
#'     Replace NumVisit by the actual number of visits.}
#'   \item{ErrorCode}{Optional integer execution status: 0 = no error; a positive value aborts the current
#'     simulation but allows subsequent simulations to run; a negative value is fatal and stops all further
#'     simulations.}
#' }
#'
#' Example-specific ErrorCode values:
#' \describe{
#'   \item{0}{No error.}
#'   \item{-1}{InputFileName is missing or invalid, or the CSV file was not found.}
#'   \item{-2}{Error reading CSV file.}
#'   \item{-3}{Treatment column not found.}
#'   \item{-4}{Insufficient visit columns in CSV.}
#'   \item{-5}{Insufficient patients in CSV for one or both arms.}
#'   \item{-6}{Specific visit column not found in CSV.}
#' }
#' @details Return each visit response as a separate named list element: Response1, Response2, ...,
#'   ResponseNumVisit.
#'
#' The CSV file must contain:
#' - A Treatment column with treatment assignments (accepts "Treatment", "TreatmentID", "Trt" or "TrtID" format,
#'   case-insensitive)
#' - Visit columns for each visit (accepts "Visit X", "Visit.X", or "VisitX" format, case-insensitive)
#'
#' Accepted Treatment Identifiers:
#' - Control: "0", "c", "ctl", "control", "placebo", "cntl" (case-insensitive)
#' - Treatment: "1", "t", "trt", "treatment", "active" (case-insensitive)
#'
#' Missing Values: "", "NA", "NaN", "na", "null", "N/A" are recognized as missing
#'
#' The function caches the CSV data globally (`gdfPatients`) with its normalized file path for efficiency
#'   across multiple calls. The cache is reloaded when the requested file changes.
#'   Patients are randomly sampled without replacement from each treatment arm, ensuring unique patient assignments
#'   within each simulation replicate.
######################################################################################################################## .

GeneratePatientFromCSVGeneral <- function( NumSub, NumVisit, ArrivalTime, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL ) {
    # Initialize return variables and error code
    nErrorCode <- 0
    lReturn <- list( )

    # Require a single file name before building the CSV path.
    if ( is.null( UserParam$InputFileName ) || !is.character( UserParam$InputFileName ) ||
        length( UserParam$InputFileName ) != 1 || is.na( UserParam$InputFileName ) ||
        !nzchar( UserParam$InputFileName ) ) {
        return( list( ErrorCode = -1L ) )
    }

    # Build CSV path and confirm it exists
    strCSVPath <- paste0( "Inputs/", UserParam$InputFileName )

    if ( !file.exists( strCSVPath ) ) {
        nErrorCode <- -1
        lReturn$ErrorCode <- as.integer( nErrorCode )
        return( lReturn )
    }

    # Cache only the requested file, even when simulation scenarios use different CSV inputs.
    strCSVPath <- normalizePath( strCSVPath )
    dfPatients <- NULL
    if ( exists( "gdfPatients", envir = .GlobalEnv, inherits = FALSE ) ) {
        dfPatients <- get( "gdfPatients", envir = .GlobalEnv )
    }
    if ( is.null( dfPatients ) || !identical( attr( dfPatients, "CyneRgyInputFile" ), strCSVPath ) ) {
        dfPatients <- tryCatch(
            {
                utils::read.csv( strCSVPath, check.names = FALSE, stringsAsFactors = FALSE )
            },
            error = function( e ) {
                NULL
            }
        )
        if ( !is.null( dfPatients ) ) {
            attr( dfPatients, "CyneRgyInputFile" ) <- strCSVPath
            assign( "gdfPatients", dfPatients, envir = .GlobalEnv )
        }
    }

    if ( is.null( dfPatients ) ) {
        nErrorCode <- -2
        lReturn$ErrorCode <- as.integer( nErrorCode )
        return( lReturn )
    }

    # Ensure Treatment column exists and is 0/1
    vColNames <- colnames( dfPatients )
    vNormCol <- NormalizeName( vColNames )
    strTreatCol <- GetColInsensitive( c( "Treatment", "TreatmentID", "Trt", "TrtID" ), vNormCol, vColNames )

    if ( is.na( strTreatCol ) ) {
        nErrorCode <- -3
        lReturn$ErrorCode <- as.integer( nErrorCode )
        return( lReturn )
    }

    vTrt <- vapply( dfPatients[[ strTreatCol ]], CoerceGroup01, integer( 1 ) )

    if ( any( is.na( vTrt ) ) ) {
        vNum <- suppressWarnings( as.integer( dfPatients[[ strTreatCol ]] ) )
        vTrt[ is.na( vTrt ) ] <- vNum[ is.na( vTrt ) ]
    }
    vKeep <- vTrt %in% c( 0, 1 )
    dfPatients <- dfPatients[ vKeep, , drop = FALSE ]
    dfPatients[[ strTreatCol ]] <- as.integer( vTrt[ vKeep ] )

    # Identify visit columns and coerce to numeric
    # Find normalized names that match visit pattern, then get actual column names
    vNormVisit <- vNormCol[ grepl( "^visit[0-9]+$", vNormCol ) ]

    if ( length( vNormVisit ) == 0 ) {
        # Fallback: find any column starting with "visit" (case-insensitive)
        vNormVisit <- vNormCol[ grepl( "^visit", vNormCol ) ]
    }
    # Get actual column names (not normalized) for the visit columns
    vVisitCols <- vColNames[ vNormCol %in% vNormVisit ]

    # Check if there are enough visit columns for the requested number of visits
    if ( length( vVisitCols ) < NumVisit ) {
        nErrorCode <- -4
        lReturn$ErrorCode <- as.integer( nErrorCode )
        return( lReturn )
    }

    # Convert all visit columns to numeric using actual column names
    for ( strCol in vVisitCols ) {
        dfPatients[[ strCol ]] <- CoerceVisitNumeric( dfPatients[[ strCol ]] )
    }

    # Per-arm availability
    nNeedCtl <- sum( as.integer( TreatmentID ) == 0 )
    nNeedTrt <- sum( as.integer( TreatmentID ) == 1 )

    # Create a vector of indexes for the control patients and treatment patients
    vIdxCtrl <- which( dfPatients[[ strTreatCol ]] == 0 )
    vIdxTrt <- which( dfPatients[[ strTreatCol ]] == 1 )

    if ( length( vIdxCtrl ) < nNeedCtl || length( vIdxTrt ) < nNeedTrt ) {
        # If there are not enough control or treatment patients then this is an error
        nErrorCode <- -5
        lReturn$ErrorCode <- as.integer( nErrorCode )
        return( lReturn )
    }

    # Sample unique rows per arm (no replacement), then map to subjects in East Horizon order
    vTakeCtrl <- integer( 0 )
    if ( nNeedCtl > 0 ) {
        vTakeCtrl <- vIdxCtrl[ sample.int( length( vIdxCtrl ), nNeedCtl, replace = FALSE ) ]
    }

    vTakeTrt <- integer( 0 )
    if ( nNeedTrt > 0 ) {
        vTakeTrt <- vIdxTrt[ sample.int( length( vIdxTrt ), nNeedTrt, replace = FALSE ) ]
    }

    # vPick will contain the index of the patient to use from the CSV that was treated with TreatmentID
    vPick <- integer( NumSub )
    nCtl <- 0 # Index for which control patient to select
    nTrt <- 0 # Index for which treatment patient to select

    for ( nSubIndx in seq_len( NumSub ) ) {
        if ( as.integer( TreatmentID[ nSubIndx ] ) == 0 ) {
            nCtl <- nCtl + 1
            vPick[ nSubIndx ] <- vTakeCtrl[ nCtl ]
        } else {
            nTrt <- nTrt + 1
            vPick[ nSubIndx ] <- vTakeTrt[ nTrt ]
        }
    }

    # Build Response1..ResponseK (numeric) directly from selected rows
    for ( nVisitIndx in seq_len( NumVisit ) ) {
        vCandidates <- c(
            paste0( "visit", nVisitIndx ),
            paste0( "visit ", nVisitIndx ),
            paste0( "visit.", nVisitIndx )
        )
        strFound <- GetColInsensitive( vCandidates, vNormCol, vColNames )

        if ( is.na( strFound ) ) {
            nErrorCode <- -6
            lReturn$ErrorCode <- as.integer( nErrorCode )
            return( lReturn )
        }
        lReturn[[ paste0( "Response", nVisitIndx ) ]] <- as.double( dfPatients[ vPick, strFound ] )
    }

    lReturn$ErrorCode <- as.integer( nErrorCode )
    return( lReturn )
}

# ---------------- Local helpers ----------------

# Match column names regardless of case, spaces, underscores, or dots.
NormalizeName <- function( strName ) {
    vStr <- tolower( as.character( strName ) )
    vStr <- gsub( "[[:space:]_.]+", "", vStr )

    return( vStr )
}

GetColInsensitive <- function( vCandidates, vNormCol, vColNames ) {
    vNormCand <- NormalizeName( vCandidates )
    for ( nCandidateIndex in seq_along( vNormCand ) ) {
        nMatch <- match( vNormCand[ nCandidateIndex ], vNormCol )
        if ( !is.na( nMatch ) ) {
            return( vColNames[ nMatch ] )
        }
    }

    return( NA_character_ )
}

CoerceGroup01 <- function( x ) {
    v <- suppressWarnings( as.numeric( x ) )
    if ( !is.na( v ) ) {
        if ( v == 0 ) {
            return( 0L )
        }
        if ( v == 1 ) {
            return( 1L )
        }
    }
    str <- tolower( trimws( as.character( x ) ) )
    if ( str %in% c( "0", "c", "ctl", "control", "placebo", "cntl" ) ) {
        return( 0L )
    }
    if ( str %in% c( "1", "t", "trt", "treatment", "active" ) ) {
        return( 1L )
    }

    return( NA_integer_ )
}

CoerceVisitNumeric <- function( v ) {
    vChr <- as.character( v )
    vChr[ vChr %in% c( "", "NA", "NaN", "na", "null", "N/A" ) ] <- NA_character_

    return( suppressWarnings( as.double( vChr ) ) )
}
