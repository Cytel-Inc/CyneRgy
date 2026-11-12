######################################################################################################################## .
#' @name GeneratePatientFromCSVSpecific
#' @title Generate Patient Responses from CSV File with Strict Formatting (Faster)
#' @description This function reads pre-simulated patient data from a CSV file and returns visit-level responses
#'   for trial subjects. It requires strict CSV formatting (specific column names and treatment identifiers) but
#'   runs faster than GeneratePatientFromCSVGeneral. Use this function when you have control over CSV formatting
#'   and want optimal performance. For more flexible formatting support, see GeneratePatientFromCSVGeneral.R.
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
#'     }
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
#' }
#' @details Return each visit response as a separate named list element: Response1, Response2, ...,
#'   ResponseNumVisit.
#'
#' The CSV file must contain:
#' - A Treatment column (exact name, case-sensitive) with treatment assignments
#' - Visit columns named exactly "Visit 1", "Visit 2", etc. (with space, case-sensitive)
#'
#' Accepted Treatment Identifiers:
#' - Control: "0" (must be integer zero or string "0")
#' - Treatment: "1" (must be integer one or string "1")
#'
#' Missing Values: "", "NA", "NaN", "na", "null", "N/A" are recognized as missing
#'
#' The function caches the CSV data globally (`gdfPatients`) with its normalized file path for efficiency
#'   across multiple calls. The cache is reloaded when the requested file changes.
#'   Patients are randomly sampled without replacement from each treatment arm, ensuring unique patient assignments
#'   within each simulation replicate.
######################################################################################################################## .

GeneratePatientFromCSVSpecific <- function( NumSub, NumVisit, ArrivalTime, TreatmentID, Inputmethod, VisitTime, MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat, UserParam = NULL ) {
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

    # Check required Treatment column (strict match)
    if ( !( "Treatment" %in% colnames( dfPatients ) ) ) {
        nErrorCode <- -3
        lReturn$ErrorCode <- as.integer( nErrorCode )
        return( lReturn )
    }

    # Coerce Treatment column strictly to integer 0/1
    vTrt <- suppressWarnings( as.integer( dfPatients[[ "Treatment" ]] ) )
    vKeep <- !is.na( vTrt ) & vTrt %in% c( 0, 1 )
    dfPatients <- dfPatients[ vKeep, , drop = FALSE ]
    dfPatients[[ "Treatment" ]] <- vTrt[ vKeep ]

    # Validate and coerce Visit columns (Visit 1, ..., Visit NumVisit)
    vVisitCols <- paste0( "Visit ", seq_len( NumVisit ) )
    if ( !all( vVisitCols %in% colnames( dfPatients ) ) ) {
        nErrorCode <- -4
        lReturn$ErrorCode <- as.integer( nErrorCode )
        return( lReturn )
    }

    for ( strCol in vVisitCols ) {
        vChr <- as.character( dfPatients[[ strCol ]] )
        vChr[ vChr %in% c( "", "NA", "NaN", "na", "null", "N/A" ) ] <- NA_character_
        dfPatients[[ strCol ]] <- suppressWarnings( as.double( vChr ) )
    }

    # Determine how many patients needed for each arm
    nNeedCtl <- sum( as.integer( TreatmentID ) == 0 )
    nNeedTrt <- sum( as.integer( TreatmentID ) == 1 )

    vIdxCtrl <- which( dfPatients[[ "Treatment" ]] == 0 )
    vIdxTrt <- which( dfPatients[[ "Treatment" ]] == 1 )

    if ( length( vIdxCtrl ) < nNeedCtl || length( vIdxTrt ) < nNeedTrt ) {
        nErrorCode <- -5
        lReturn$ErrorCode <- as.integer( nErrorCode )
        return( lReturn )
    }

    # Randomly select unique patient rows per treatment arm
    vTakeCtrl <- if ( nNeedCtl > 0 ) {
        vIdxCtrl[ sample.int( length( vIdxCtrl ), nNeedCtl, replace = FALSE ) ]
    } else {
        integer( 0 )
    }
    vTakeTrt <- if ( nNeedTrt > 0 ) {
        vIdxTrt[ sample.int( length( vIdxTrt ), nNeedTrt, replace = FALSE ) ]
    } else {
        integer( 0 )
    }

    # Map selected patients to subjects by requested treatment order
    vPick <- integer( NumSub )
    nCtl <- 0
    nTrt <- 0

    for ( nSubIndx in seq_len( NumSub ) ) {
        if ( as.integer( TreatmentID[ nSubIndx ] ) == 0 ) {
            nCtl <- nCtl + 1
            vPick[ nSubIndx ] <- vTakeCtrl[ nCtl ]
        } else {
            nTrt <- nTrt + 1
            vPick[ nSubIndx ] <- vTakeTrt[ nTrt ]
        }
    }

    # Build Response1..ResponseK values for each subject
    for ( nVisitIndx in seq_len( NumVisit ) ) {
        lReturn[[ paste0( "Response", nVisitIndx ) ]] <- as.double( dfPatients[ vPick, vVisitCols[ nVisitIndx ] ] )
    }

    # Return assembled output with error code
    lReturn$ErrorCode <- as.integer( nErrorCode )
    return( lReturn )
}
