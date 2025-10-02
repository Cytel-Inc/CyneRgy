# ============================ =
# Minimal Bug Checker (styled, documented)
# ============================ =

#' Null-coalescing operator
#'
#' Returns the left-hand side if it is not \code{NULL}, otherwise returns the right-hand side.
#'
#' @name or_or
#' @rdname or_or
#' @param a Any object; returned when not \code{NULL}.
#' @param b Any object; returned when \code{a} is \code{NULL}.
#' @return The value of \code{a} if not \code{NULL}; otherwise \code{b}.
#' @usage a \%||\% b
#' @examples
#' 1 \%||\% 2
#' NULL \%||\% 2
`%||%` <- function( a, b ) {
    return( if ( !is.null( a ) ) a else b )
}

#' Fail with a formatted message (no call)
#'
#' A thin wrapper around \code{stop()} that formats the message via \code{sprintf()} and
#' suppresses the call.
#'
#' @param ... Character fragments passed to \code{sprintf()}.
#' @return This function always throws an error.
#' @seealso \code{base::stop}, \code{base::sprintf}
Failf <- function( ... ) {
    stop( sprintf( ... ), call. = FALSE )
}

#' Check allowed file extension (case-insensitive)
#'
#' Determines whether a path has one of the provided extensions. Comparisons are case-insensitive.
#'
#' @param strPath Character scalar file path.
#' @param vExts Character vector of extensions beginning with a dot. Defaults to \code{c(".R", ".rds")}.
#' @return Logical scalar indicating whether the extension matches one of \code{vExts}.
HasExt <- function( strPath, vExts = c( ".R", ".rds" ) ) {
    if ( !is.character( strPath ) || length( strPath ) != 1L ) return( FALSE )
    strExt <- tolower( paste0( ".", tools::file_ext( strPath ) ) )
    return( strExt %in% tolower( vExts ) )
}

#' Resolve a function specification
#'
#' Accepts a function object, unquoted symbol, function name, or a path to a \code{.R} / \code{.rds}
#' file containing a function. Returns the resolved function object or throws on failure.
#'
#' @param functionInput Function object, unquoted symbol, character name, or file path to \code{.R} or \code{.rds}.
#' @return A function object.
#' @seealso \code{ResolveParams}
ResolveFunction <- function( functionInput ) {
    # Already a function
    if ( is.function( functionInput ) ) return( functionInput )
    
    # Unquoted symbol (e.g., PerformMMRMAnalysis)
    symInput <- substitute( functionInput )
    if ( is.symbol( symInput ) ) {
        strName <- deparse( symInput )
        if ( exists( strName, mode = "function", inherits = TRUE ) ) {
            return( get( strName, mode = "function", inherits = TRUE ) )
        }
        Failf( "Function name '%s' not found in scope.", strName )
    }
    
    # Character: function name or path
    if ( is.character( functionInput ) && length( functionInput ) == 1L ) {
        if ( file.exists( functionInput ) ) {
            strPath <- functionInput
            
            # .rds containing a function
            if ( HasExt( strPath, ".rds" ) ) {
                oObj <- readRDS( strPath )
                if ( !is.function( oObj ) ) Failf( "'.rds' did not contain a function object." )
                return( oObj )
            }
            
            # .R file containing BuggedFunction or exactly one function
            if ( HasExt( strPath, ".R" ) ) {
                envTmp <- new.env( parent = emptyenv() )
                sys.source( strPath, envir = envTmp )
                
                if ( exists( "BuggedFunction", envir = envTmp, inherits = FALSE ) &&
                     is.function( get( "BuggedFunction", envir = envTmp ) ) ) {
                    return( get( "BuggedFunction", envir = envTmp ) )
                }
                
                vObjs <- ls( envTmp, all.names = TRUE )
                vFuns <- vObjs[ vapply( vObjs, function( strNm ) is.function( get( strNm, envir = envTmp ) ), logical( 1 ) ) ]
                if ( length( vFuns ) == 1L ) {
                    return( get( vFuns[ 1 ], envir = envTmp ) )
                }
                Failf( "Could not resolve a single function from '%s'. Define `BuggedFunction` or only one function.", strPath )
            }
            
            Failf( "Unsupported function file type for '%s' (use .R or .rds).", strPath )
        } else {
            strName <- functionInput
            if ( exists( strName, mode = "function", inherits = TRUE ) ) {
                return( get( strName, mode = "function", inherits = TRUE ) )
            }
            Failf( "Function name '%s' not found.", strName )
        }
    }
    
    Failf( "`functionInput` must be a function, a path to .R/.rds, or a function name (string)." )
}

#' Resolve a parameter specification
#'
#' Accepts a list or a path to a \code{.R} / \code{.rds} file. For \code{.R}, the function looks for
#' \code{lInputData}; if absent, it gathers all non-function objects into a list.
#'
#' @param paramsInput A list, or character path to \code{.R} / \code{.rds}.
#' @return A list suitable for passing as arguments.
#' @seealso \code{ResolveFunction}
ResolveParams <- function( paramsInput ) {
    if ( is.list( paramsInput ) ) return( paramsInput )
    
    if ( is.character( paramsInput ) && length( paramsInput ) == 1L && file.exists( paramsInput ) ) {
        strPath <- paramsInput
        
        # .rds containing a list
        if ( HasExt( strPath, ".rds" ) ) {
            oObj <- readRDS( strPath )
            if ( !is.list( oObj ) ) Failf( "'.rds' did not contain a list for lInputData." )
            return( oObj )
        }
        
        # .R file exporting lInputData or plain objects
        if ( HasExt( strPath, ".R" ) ) {
            envTmp <- new.env( parent = emptyenv() )
            sys.source( strPath, envir = envTmp )
            
            if ( exists( "lInputData", envir = envTmp, inherits = FALSE ) ) {
                lLi <- get( "lInputData", envir = envTmp )
                if ( !is.list( lLi ) ) Failf( "In params .R, `lInputData` exists but is not a list." )
                return( lLi )
            }
            
            vObjs <- ls( envTmp, all.names = TRUE )
            lVals <- lapply( vObjs, function( strNm ) get( strNm, envir = envTmp ) )
            names( lVals ) <- vObjs
            vKeep <- !vapply( lVals, is.function, logical( 1 ) )
            lVals <- lVals[ vKeep ]
            if ( !length( lVals ) ) Failf( "Params .R had no `lInputData` and no plain objects to bundle." )
            return( lVals )
        }
        
        Failf( "Unsupported params file type for '%s' (use .R or .rds).", strPath )
    }
    
    Failf( "`paramsInput` must be a list or a path to .R/.rds." )
}

#' Safely call a function with provided inputs
#'
#' Executes a function with arguments from a list, capturing errors. Returns a list with fields
#' \code{ok}, \code{value}, \code{error_message}, and \code{args_passed}.
#'
#' @param buggedFunction A function object to call.
#' @param lInputData A named list of arguments to pass.
#' @return A list containing execution status, return value (when successful), error message, and the arguments passed.
#' @examples
#' \dontrun{
#'   RunBugCheck( sum, list( 1, 2, na.rm = TRUE ) )
#' }
RunBugCheck <- function( buggedFunction, lInputData ) {
    if ( !is.function( buggedFunction ) ) Failf( "`buggedFunction` must be a function object." )
    if ( !is.list( lInputData ) )        Failf( "`lInputData` must be a named list." )
    
    vFormals     <- formals( buggedFunction ) %||% pairlist()
    vFormalNames <- names( vFormals ) %||% character( 0 )
    bHasDots     <- any( vFormalNames == "..." )
    vGiven       <- names( lInputData ) %||% character( 0 )
    vPass        <- if ( bHasDots ) vGiven else intersect( vGiven, setdiff( vFormalNames, "..." ) )
    lArgs        <- lInputData[ vPass ]
    
    strErr <- NULL
    val <- tryCatch( 
        do.call( buggedFunction, lArgs ),
        error = function( e ) { strErr <<- conditionMessage( e ); NULL }
    )
    
    lRes <- list( 
        ok            = is.null( strErr ),
        value         = if ( is.null( strErr ) ) val else NULL,
        error_message = strErr,
        args_passed   = lArgs
    )
    return( lRes )
}

#' Pretty-print a bug check result
#'
#' Prints a concise summary of a \code{RunBugCheck()} result.
#'
#' @param res A list returned by \code{RunBugCheck()}.
#' @return The input \code{res}, invisibly.
#' @seealso \code{RunBugCheck}
PrintBugCheckResult <- function( res ) {
    if ( !is.list( res ) || is.null( res$ok ) ) {
        cat( "Invalid result object.\n" )
        return( invisible( res ) )
    }
    
    if ( isTRUE( res$ok ) ) {
        cat( "OK: function executed without error.\nValue preview:\n" )
        try( utils::str( res$value, max.level = 1L, give.attr = FALSE ), silent = TRUE )
    } else {
        cat( "ERROR: ", res$error_message %||% "", "\n", sep = "" )
        if ( length( res$args_passed ) ) {
            cat( "ARGS PASSED: ", paste( names( res$args_passed ), collapse = ", " ), "\n", sep = "" )
        }
    }
    
    return( invisible( res ) )
}

# ============================ =
# Utility Functions
# ============================ =

#' Ensure a directory exists
#'
#' Creates a directory if it does not already exist (recursively). Returns \code{TRUE} invisibly.
#'
#' @param path Character scalar directory path.
#' @return Logical \code{TRUE}, invisibly.
EnsureDir <- function( path ) {
    if ( !dir.exists( path ) ) {
        dir.create( path, recursive = TRUE, showWarnings = FALSE )
    }
    invisible( TRUE )
}

#' Sanitize a string for safe filenames
#'
#' Replaces disallowed characters with underscores, trims leading underscores, and falls back to
#' \code{"code"} when the result would be empty.
#'
#' @param x Character scalar to sanitize.
#' @return A sanitized character scalar.
MakeSafeName <- function( x ) {
    s <- gsub( "[^A-Za-z0-9._-]+", "_", x )
    s <- sub( "^_+", "", s )
    if ( !nzchar( s ) ) s <- "code"
    return( s )
}

#' Read a text file as a single string
#'
#' Returns \code{""} when the file does not exist.
#'
#' @param path Character scalar file path.
#' @return A single character string with newline-separated file contents.
ReadTextFile <- function( path ) {
    if ( !file.exists( path ) ) return( "" )
    return( paste( readLines( path, warn = FALSE ), collapse = "\n" ) )
}

#' Normalize R code for hashing
#'
#' Canonicalizes line endings, trims leading/trailing whitespace, and collapses multiple blank lines.
#'
#' @param txt Character string of code.
#' @return Canonicalized code as a single string (possibly empty).
CanonicalizeCode <- function( txt ) {
    if ( !nzchar( txt ) ) return( "" )
    v <- unlist( strsplit( gsub( "\r\n?", "\n", txt ), "\n" ) )
    v <- sub( "^[ \t]+", "", v )
    v <- sub( "[ \t]+$", "", v )
    vKeep <- !( v == "" & c( FALSE, head( v == "", -1 ) ) )
    v <- v[ vKeep ]
    return( paste( v, collapse = "\n" ) )
}

#' Compute an MD5 hash of code text or a file
#'
#' If \code{txtOrPath} is an existing path, the file is read. Otherwise the string
#' is treated as the code text. Empty canonicalized input yields \code{NA_character_}.
#'
#' @param txtOrPath Character code string or path to a text file.
#' @return A length-1 character vector with the MD5 hash, or \code{NA_character_} for empty input.
CodeHash <- function( txtOrPath ) {
    txt <- if ( file.exists( txtOrPath ) ) ReadTextFile( txtOrPath ) else txtOrPath
    strCanon <- CanonicalizeCode( txt )
    if ( !nzchar( strCanon ) ) return( NA_character_ )
    strTmp <- tempfile( fileext = ".txt" )
    on.exit( unlink( strTmp ), add = TRUE )
    cat( strCanon, file = strTmp )
    return( unname( tools::md5sum( strTmp ) ) )
}

#' Find the largest .R file within a directory tree
#'
#' Recursively searches for \code{.R} files and returns the path to the largest by byte size.
#'
#' @param trialDir Directory to search.
#' @return Character path to the largest \code{.R} file, or \code{NA_character_} if none found.
FindPrimaryRScript <- function( trialDir ) {
    vR <- list.files( trialDir, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE )
    if ( !length( vR ) ) return( NA_character_ )
    vSizes <- suppressWarnings( file.info( vR )$size )
    vSizes[ is.na( vSizes ) ] <- -Inf
    return( vR[ which.max( vSizes ) ] )
}

#' Safe RDS reader
#'
#' Reads an RDS file and returns a list \code{list( ok = TRUE/FALSE, value, path )}. On error, a warning is issued and
#' \code{ok = FALSE} is returned.
#'
#' @param path Character path to an RDS file.
#' @return A list with fields \code{ok}, \code{value}, and \code{path}.
SafeReadRDS <- function( path ) {
    lOut <- tryCatch( {
        oVal <- readRDS( path )
        list( ok = TRUE, value = oVal, path = path )
    }, error = function( e ) {
        warning( "Could not read RDS: ", path, " (", conditionMessage( e ), ")" )
        list( ok = FALSE, value = NULL, path = path )
    } )
    return( lOut )
}

#' Combine heterogeneous RDS payloads into a uniform list-of-lists
#'
#' Keys the output by the source file's base name. Data frames are wrapped under \code{data}; other
#' objects under \code{value}. Lists are stored as-is.
#'
#' @param vals A list of deserialized objects.
#' @param srcs A character vector of source file paths aligned with \code{vals}.
#' @return A named list-of-lists keyed by file base name (sans extension).
CombineRDSObjects <- function( vals, srcs ) {
    lOut <- list()
    for ( i in seq_along( vals ) ) {
        oObj <- vals[[ i ]]
        strF <- tools::file_path_sans_ext( basename( srcs[[ i ]] ) )
        if ( is.list( oObj ) ) {
            lOut[[ strF ]] <- oObj
        } else if ( is.data.frame( oObj ) ) {
            lOut[[ strF ]] <- list( data = oObj )
        } else {
            lOut[[ strF ]] <- list( value = oObj )
        }
    }
    return( lOut )
}

#' Remove all contents inside a directory (but keep the directory)
#'
#' Deletes files and subdirectories within \code{dirPath}. No-op if the directory does not exist.
#'
#' @param dirPath Character scalar directory path.
#' @return Logical \code{TRUE}, invisibly.
PurgeDirContents <- function( dirPath ) {
    if ( !dir.exists( dirPath ) ) return( invisible( TRUE ) )
    vKids <- list.files( dirPath, all.files = TRUE, full.names = TRUE, no.. = TRUE, include.dirs = TRUE )
    if ( length( vKids ) ) unlink( vKids, recursive = TRUE, force = TRUE )
    invisible( TRUE )
}




