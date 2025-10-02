# =========================== =
# Minimal Bug Checker 
# =========================== =

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
    #---------------------------------------------------------------------- -
    # Return left if non-NULL; else right
    #---------------------------------------------------------------------- -
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
    #---------------------------------------------------------------------- -
    # Format message and throw without call trace
    #---------------------------------------------------------------------- -
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
    #---------------------------------------------------------------------- -
    # Step 1: Validate input and extract extension
    #---------------------------------------------------------------------- -
    if ( !is.character( strPath ) || length( strPath ) != 1L ) return( FALSE )
    strExt <- tolower( paste0( ".", tools::file_ext( strPath ) ) )
    
    #---------------------------------------------------------------------- -
    # Step 2: Case-insensitive membership check
    #---------------------------------------------------------------------- -
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
    #---------------------------------------------------------------------- -
    # Step 1: Return if already a function
    #---------------------------------------------------------------------- -
    if ( is.function( functionInput ) ) return( functionInput )
    
    #---------------------------------------------------------------------- -
    # Step 2: Unquoted symbol (Resolve from scope)
    #---------------------------------------------------------------------- -
    symInput <- substitute( functionInput )
    if ( is.symbol( symInput ) ) {
        strName <- deparse( symInput )
        if ( exists( strName, mode = "function", inherits = TRUE ) ) {
            return( get( strName, mode = "function", inherits = TRUE ) )
        }
        Failf( "Function name '%s' not found in scope.", strName )
    }
    
    #---------------------------------------------------------------------- -
    # Step 3: Character input — name or file path
    #---------------------------------------------------------------------- -
    if ( is.character( functionInput ) && length( functionInput ) == 1L ) {
        if ( file.exists( functionInput ) ) {
            strPath <- functionInput
            
            #--- 3a: .rds containing a function ---
            if ( HasExt( strPath, ".rds" ) ) {
                oObj <- readRDS( strPath )
                if ( !is.function( oObj ) ) Failf( "'.rds' did not contain a function object." )
                return( oObj )
            }
            
            #--- 3b: .R file with BuggedFunction or exactly one function ---
            if ( HasExt( strPath, ".R" ) ) {
                envTmp <- new.env( parent = emptyenv() )
                sys.source( strPath, envir = envTmp )
                
                # Prefer explicit BuggedFunction
                if ( exists( "BuggedFunction", envir = envTmp, inherits = FALSE ) &&
                     is.function( get( "BuggedFunction", envir = envTmp ) ) ) {
                    return( get( "BuggedFunction", envir = envTmp ) )
                }
                
                # Fallback: exactly one function defined
                vObjs <- ls( envTmp, all.names = TRUE )
                vFuns <- vObjs[ vapply( vObjs, function( strNm ) is.function( get( strNm, envir = envTmp ) ), logical( 1 ) ) ]
                if ( length( vFuns ) == 1L ) {
                    return( get( vFuns[ 1L ], envir = envTmp ) )
                }
                Failf( "Could not resolve a single function from '%s'. Define `BuggedFunction` or only one function.", strPath )
            }
            
            # Unsupported extension
            Failf( "Unsupported function file type for '%s' (use .R or .rds).", strPath )
        } else {
            # Name in scope
            strName <- functionInput
            if ( exists( strName, mode = "function", inherits = TRUE ) ) {
                return( get( strName, mode = "function", inherits = TRUE ) )
            }
            Failf( "Function name '%s' not found.", strName )
        }
    }
    
    #---------------------------------------------------------------------- -
    # Step 4: Otherwise, error
    #---------------------------------------------------------------------- -
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
    #---------------------------------------------------------------------- -
    # Step 1: Return list as-is
    #---------------------------------------------------------------------- -
    if ( is.list( paramsInput ) ) return( paramsInput )
    
    #---------------------------------------------------------------------- -
    # Step 2: Character input path (.rds / .R)
    #---------------------------------------------------------------------- -
    if ( is.character( paramsInput ) && length( paramsInput ) == 1L && file.exists( paramsInput ) ) {
        strPath <- paramsInput
        
        #--- 2a: .rds containing a list ---
        if ( HasExt( strPath, ".rds" ) ) {
            oObj <- readRDS( strPath )
            if ( !is.list( oObj ) ) Failf( "'.rds' did not contain a list for lInputData." )
            return( oObj )
        }
        
        #--- 2b: .R exporting lInputData, else gather non-functions ---
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
        
        # Unsupported extension
        Failf( "Unsupported params file type for '%s' (use .R or .rds).", strPath )
    }
    
    #---------------------------------------------------------------------- -
    # Step 3: Otherwise, error
    #---------------------------------------------------------------------- -
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
    #---------------------------------------------------------------------- -
    # Step 1: Validate inputs
    #---------------------------------------------------------------------- -
    if ( !is.function( buggedFunction ) ) Failf( "`buggedFunction` must be a function object." )
    if ( !is.list( lInputData ) )        Failf( "`lInputData` must be a named list." )
    
    #---------------------------------------------------------------------- -
    # Step 2: Prepare callables (respect ...)
    #---------------------------------------------------------------------- -
    vFormals     <- formals( buggedFunction ) %||% pairlist()
    vFormalNames <- names( vFormals ) %||% character( 0 )
    bHasDots     <- any( vFormalNames == "..." )
    vGiven       <- names( lInputData ) %||% character( 0 )
    vPass        <- if ( bHasDots ) vGiven else intersect( vGiven, setdiff( vFormalNames, "..." ) )
    lArgs        <- lInputData[ vPass ]
    
    #---------------------------------------------------------------------- -
    # Step 3: Safe execution and capture
    #---------------------------------------------------------------------- -
    strErr <- NULL
    val <- tryCatch( 
        do.call( buggedFunction, lArgs ),
        error = function( e ) { strErr <<- conditionMessage( e ); NULL }
    )
    
    #---------------------------------------------------------------------- -
    # Step 4: Package result
    #---------------------------------------------------------------------- -
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
    #---------------------------------------------------------------------- -
    # Step 1: Validate object shape
    #---------------------------------------------------------------------- -
    if ( !is.list( res ) || is.null( res$ok ) ) {
        cat( "Invalid result object.\n" )
        return( invisible( res ) )
    }
    
    #---------------------------------------------------------------------- -
    # Step 2: Pretty-print outcome
    #---------------------------------------------------------------------- -
    if ( isTRUE( res$ok ) ) {
        cat( "OK: function executed without error.\nValue preview:\n" )
        try( utils::str( res$value, max.level = 1L, give.attr = FALSE ), silent = TRUE )
    } else {
        cat( "ERROR: ", res$error_message %||% "", "\n", sep = "" )
        if ( length( res$args_passed ) ) {
            cat( "ARGS PASSED: ", paste( names( res$args_passed ), collapse = ", " ), "\n", sep = "" )
        }
    }
    
    #---------------------------------------------------------------------- -
    # Step 3: Return input invisibly
    #---------------------------------------------------------------------- -
    return( invisible( res ) )
}

# =========================== =
# Utility Functions
# =========================== =

#' Ensure a directory exists
#'
#' Creates a directory if it does not already exist (recursively). Returns \code{TRUE} invisibly.
#'
#' @param path Character scalar directory path.
#' @return Logical \code{TRUE}, invisibly.
EnsureDir <- function( path ) {
    #---------------------------------------------------------------------- -
    # Step 1: Create as needed
    #---------------------------------------------------------------------- -
    if ( !dir.exists( path ) ) {
        dir.create( path, recursive = TRUE, showWarnings = FALSE )
    }
    
    #---------------------------------------------------------------------- -
    # Step 2: Return TRUE invisibly
    #---------------------------------------------------------------------- -
    return( invisible( TRUE ) )
}

#' Sanitize a string for safe filenames
#'
#' Replaces disallowed characters with underscores, trims leading underscores, and falls back to
#' \code{"code"} when the result would be empty.
#'
#' @param x Character scalar to sanitize.
#' @return A sanitized character scalar.
MakeSafeName <- function( x ) {
    #---------------------------------------------------------------------- -
    # Step 1: Replace invalids and trim
    #---------------------------------------------------------------------- -
    strS <- gsub( "[^A-Za-z0-9._-]+", "_", x )
    strS <- sub( "^_+", "", strS )
    
    #---------------------------------------------------------------------- -
    # Step 2: Fallback if empty
    #---------------------------------------------------------------------- -
    if ( !nzchar( strS ) ) strS <- "code"
    return( strS )
}

#' Read a text file as a single string
#'
#' Returns \code{\"\"} when the file does not exist.
#'
#' @param path Character scalar file path.
#' @return A single character string with newline-separated file contents.
ReadTextFile <- function( path ) {
    #---------------------------------------------------------------------- -
    # Step 1: Early return on missing file
    #---------------------------------------------------------------------- -
    if ( !file.exists( path ) ) return( "" )
    
    #---------------------------------------------------------------------- -
    # Step 2: Read and join lines
    #---------------------------------------------------------------------- -
    return( paste( readLines( path, warn = FALSE ), collapse = "\n" ) )
}

#' Normalize R code for hashing
#'
#' Canonicalizes line endings, trims leading/trailing whitespace, and collapses multiple blank lines.
#'
#' @param txt Character string of code.
#' @return Canonicalized code as a single string (possibly empty).
CanonicalizeCode <- function( txt ) {
    #---------------------------------------------------------------------- -
    # Step 1: Handle empty
    #---------------------------------------------------------------------- -
    if ( !nzchar( txt ) ) return( "" )
    
    #---------------------------------------------------------------------- -
    # Step 2: Normalize line endings and trim
    #---------------------------------------------------------------------- -
    vLines <- unlist( strsplit( gsub( "\r\n?", "\n", txt ), "\n" ) )
    vLines <- sub( "^[ \t]+", "", vLines )
    vLines <- sub( "[ \t]+$", "", vLines )
    
    #---------------------------------------------------------------------- -
    # Step 3: Collapse repeated blanks
    #---------------------------------------------------------------------- -
    bKeep <- !( vLines == "" & c( FALSE, head( vLines == "", -1L ) ) )
    vLines <- vLines[ bKeep ]
    return( paste( vLines, collapse = "\n" ) )
}

#' Compute an MD5 hash of code text or a file
#'
#' If \code{txtOrPath} is an existing path, the file is read. Otherwise the string
#' is treated as the code text. Empty canonicalized input yields \code{NA_character_}.
#'
#' @param txtOrPath Character code string or path to a text file.
#' @return A length-1 character vector with the MD5 hash, or \code{NA_character_} for empty input.
CodeHash <- function( txtOrPath ) {
    #---------------------------------------------------------------------- -
    # Step 1: Get code text (from path or value)
    #---------------------------------------------------------------------- -
    txt <- if ( file.exists( txtOrPath ) ) ReadTextFile( txtOrPath ) else txtOrPath
    
    #---------------------------------------------------------------------- -
    # Step 2: Canonicalize and short-circuit
    #---------------------------------------------------------------------- -
    strCanon <- CanonicalizeCode( txt )
    if ( !nzchar( strCanon ) ) return( NA_character_ )
    
    #---------------------------------------------------------------------- -
    # Step 3: Hash via temp file
    #---------------------------------------------------------------------- -
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
    #---------------------------------------------------------------------- -
    # Step 1: Discover .R files
    #---------------------------------------------------------------------- -
    vR <- list.files( trialDir, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE )
    if ( !length( vR ) ) return( NA_character_ )
    
    #---------------------------------------------------------------------- -
    # Step 2: Choose the largest by size
    #---------------------------------------------------------------------- -
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
    #---------------------------------------------------------------------- -
    # Step 1: Try to read; capture error
    #---------------------------------------------------------------------- -
    lOut <- tryCatch( {
        oVal <- readRDS( path )
        list( ok = TRUE, value = oVal, path = path )
    }, error = function( e ) {
        warning( "Could not read RDS: ", path, " (", conditionMessage( e ), ")" )
        list( ok = FALSE, value = NULL, path = path )
    } )
    
    #---------------------------------------------------------------------- -
    # Step 2: Return status bundle
    #---------------------------------------------------------------------- -
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
    #---------------------------------------------------------------------- -
    # Step 1: Initialize output
    #---------------------------------------------------------------------- -
    lOut <- list()
    
    #---------------------------------------------------------------------- -
    # Step 2: Normalize each object under a key
    #---------------------------------------------------------------------- -
    for ( iObj in seq_along( vals ) ) {
        oObj <- vals[[ iObj ]]
        strF <- tools::file_path_sans_ext( basename( srcs[[ iObj ]] ) )
        
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
    #---------------------------------------------------------------------- -
    # Step 1: Exit if directory missing
    #---------------------------------------------------------------------- -
    if ( !dir.exists( dirPath ) ) return( invisible( TRUE ) )
    
    #---------------------------------------------------------------------- -
    # Step 2: Delete children recursively
    #---------------------------------------------------------------------- -
    vKids <- list.files( dirPath, all.files = TRUE, full.names = TRUE, no.. = TRUE, include.dirs = TRUE )
    if ( length( vKids ) ) unlink( vKids, recursive = TRUE, force = TRUE )
    return( invisible( TRUE ) )
}

# ================================================== =
# Auto-select helpers (readable + stepwise comments)
# ================================================== =

#' Parse only function definitions from an .R file (skip other top-level code)
#'
#' Reads an .R file, captures function definitions and simple symbol aliases into a provided (or new)
#' environment, and skips execution of any other top-level code.
#'
#' @param strRFile Character path to an .R file.
#' @param envir Environment to assign functions into. Defaults to a new environment with base parent.
#' @return A list with elements \code{env} (the environment) and \code{fun_names} (character vector of function names).
ParseFunctionsOnly <- function( strRFile, envir = new.env( parent = baseenv() ) ) {
    #---------------------------------------------------------------------- -
    # Step 1: Parse file into expressions (no evaluation)
    #---------------------------------------------------------------------- -
    vExprs <- parse( strRFile, keep.source = FALSE )
    
    #---------------------------------------------------------------------- -
    # Step 2: Initialize collectors for function names and alias pairs
    #---------------------------------------------------------------------- -
    vFunNames <- character( 0 )
    lAliases <- list()
    
    #---------------------------------------------------------------------- -
    # Step 3: Walk expressions; assign function defs and record simple aliases
    #---------------------------------------------------------------------- -
    for ( oExpr in vExprs ) {
        if ( is.call( oExpr ) && as.character( oExpr[[ 1L ]] ) %in% c( "<-", "=" ) ) {
            oLhs <- oExpr[[ 2L ]]
            oRhs <- oExpr[[ 3L ]]
            if ( is.symbol( oLhs ) ) {
                strNm <- as.character( oLhs )
                # Direct function definition: name <- function(...) { ... }
                if ( is.call( oRhs ) && identical( oRhs[[ 1L ]], as.name( "function" ) ) ) {
                    fnObj <- eval( oRhs, envir = envir, enclos = baseenv() )
                    assign( strNm, fnObj, envir = envir )
                    vFunNames <- c( vFunNames, strNm )
                } else if ( is.symbol( oRhs ) ) {
                    lAliases[[ strNm ]] <- as.character( oRhs )
                }
            }
        }
    }
    
    #---------------------------------------------------------------------- -
    # Step 4: Resolve simple aliases that point to existing functions
    #---------------------------------------------------------------------- -
    if ( length( lAliases ) ) {
        for ( strNm in names( lAliases ) ) {
            strTgt <- lAliases[[ strNm ]]
            if ( exists( strTgt, envir = envir, mode = "function", inherits = FALSE ) ) {
                assign( strNm, get( strTgt, envir = envir, mode = "function" ), envir = envir )
                vFunNames <- c( vFunNames, strNm )
            }
        }
    }
    
    #---------------------------------------------------------------------- -
    # Step 5: Return environment and unique function names
    #---------------------------------------------------------------------- -
    lOut <- list( env = envir, fun_names = unique( vFunNames ) )
    return( lOut )
}

#' Get non-dots formal argument names of a function
#'
#' @param fn A function object.
#' @return Character vector of formal argument names excluding \code{"..."}.
GetFormalNames <- function( fn ) {
    #---------------------------------------------------------------------- -
    # Step 1: Extract names of formals and drop "..."
    #---------------------------------------------------------------------- -
    v <- names( formals( fn ) )
    if ( is.null( v ) ) v <- character( 0 )
    v <- setdiff( v, "..." )
    return( v )
}

#' Collect candidate argument lists nested within an object
#'
#' Traverses nested lists to collect candidate lists (those that are not "all-lists" at a given level).
#'
#' @param x Any R object; typically a nested list loaded from an RDS.
#' @param path Internal recursive path accumulator (do not supply).
#' @param out Internal recursive output accumulator (do not supply).
#' @return A list of candidates, each as \code{list( path = <character>, value = <list> )}.
CollectCandidates <- function( x, path = list(), out = list() ) {
    #---------------------------------------------------------------------- -
    # Step 1: Recurse into lists; collect mixed (non-all-list) nodes
    #---------------------------------------------------------------------- -
    if ( is.list( x ) && length( x ) > 0L ) {
        bAllLists <- all( vapply( x, is.list, logical( 1L ) ) )
        if ( !bAllLists ) out[[ length( out ) + 1L ]] <- list( path = path, value = x )
        
        vN <- names( x )
        for ( i in seq_along( x ) ) {
            key <- if ( is.null( vN ) || !nzchar( vN[ i ] ) ) i else vN[ i ]
            out <- CollectCandidates( x[[ i ]], c( path, key ), out )
        }
    }
    return( out )
}

#' Build a reproducible call string and a runnable closure
#'
#' Uses a provided function name (string) alongside the function object and an argument list to build
#' a readable call string and a \code{run()} closure for execution.
#'
#' @param fn A function object.
#' @param lInputData A list of arguments to bind.
#' @param fn_name Character function name to display in the call string.
#' @param strDataVar Name to use for the argument list in the call string. Default: \code{"lInputData"}.
#' @return A list with elements \code{fn}, \code{args}, \code{call_string}, and \code{run}.
BindFunctionCall <- function( fn, lInputData, fn_name, strDataVar = "lInputData" ) {
    #---------------------------------------------------------------------- -
    # Step 1: Prepare safe references for each argument
    #---------------------------------------------------------------------- -
    vArgNames <- names( lInputData )
    if ( is.null( vArgNames ) ) vArgNames <- rep( "", length( lInputData ) )
    
    FnPiece <- function( i ) {
        strNm <- vArgNames[ i ]
        if ( nzchar( strNm ) ) {
            if ( strNm == make.names( strNm ) ) return( paste0( strDataVar, "$", strNm ) )
            return( paste0( strDataVar, '[[ "', strNm, '" ]]' ) )
        }
        return( paste0( strDataVar, "[[ ", i, " ]]" ) )
    }
    
    #---------------------------------------------------------------------- -
    # Step 2: Build human-readable call + runnable closure
    #---------------------------------------------------------------------- -
    strCall <- paste0( fn_name, "( ", paste( vapply( seq_along( lInputData ), FnPiece, "" ), collapse = ", " ), " )" )
    lOut <- list( fn = fn, args = lInputData, call_string = strCall, run = function() do.call( fn, lInputData ) )
    return( lOut )
}

#' Safely call a function and capture the result
#'
#' @param fn A function object.
#' @param lArgs A named list of arguments.
#' @return A list with fields \code{ok}, \code{value}, \code{error_message}, and \code{args_passed}.
SafeCall <- function( fn, lArgs ) {
    #---------------------------------------------------------------------- -
    # Step 1: Try-call and capture any error
    #---------------------------------------------------------------------- -
    strErr <- NULL
    oVal <- tryCatch( do.call( fn, lArgs ), error = function( e ) { strErr <<- conditionMessage( e ); NULL } )
    
    #---------------------------------------------------------------------- -
    # Step 2: Return status bundle
    #---------------------------------------------------------------------- -
    lRes <- list( ok = is.null( strErr ), value = if ( is.null( strErr ) ) oVal else NULL, error_message = strErr, args_passed = lArgs )
    return( lRes )
}

#' Auto-select best-matching (function, args) by name overlap (parse-only loader)
#'
#' Parses only function definitions from the .R file, scans nested lists in \code{lAll} for candidate
#' argument lists, scores each candidate by overlap with formal names for each function, and selects
#' the best match. Fills missing argument names where possible.
#'
#' @param lAll A nested list (e.g., from an RDS) potentially containing argument lists.
#' @param strRFile Character path to the .R file to parse.
#' @return A list with elements \code{fn} (function), \code{fn_name} (character), and \code{args} (list).
AutoSelectFunctionAndArgs <- function( lAll, strRFile ) {
    #---------------------------------------------------------------------- -
    # Step 1: Parse functions only (no top-level execution)
    #---------------------------------------------------------------------- -
    p <- ParseFunctionsOnly( strRFile )
    envTmp <- p$env
    vFns <- p$fun_names
    if ( !length( vFns ) ) stop( "No functions found in: ", strRFile )
    
    #---------------------------------------------------------------------- -
    # Step 2: Prefer BuggedFunction if present
    #---------------------------------------------------------------------- -
    if ( "BuggedFunction" %in% vFns ) vFns <- c( "BuggedFunction", setdiff( vFns, "BuggedFunction" ) )
    
    #---------------------------------------------------------------------- -
    # Step 3: Gather candidate argument lists from nested structures
    #---------------------------------------------------------------------- -
    lCands <- CollectCandidates( lAll )
    if ( !length( lCands ) ) stop( "No argument-list candidates found in RDS." )
    
    #---------------------------------------------------------------------- -
    # Step 4: Score candidates by name overlap with each function's formals
    #---------------------------------------------------------------------- -
    dfScores <- data.frame( cand = integer( 0 ), fn = character( 0 ), overlap = integer( 0 ), stringsAsFactors = FALSE )
    for ( i in seq_along( lCands ) ) {
        vCandNames <- names( lCands[[ i ]]$value )
        if ( is.null( vCandNames ) ) vCandNames <- character( 0 )
        for ( strFn in vFns ) {
            fnObj <- get( strFn, envir = envTmp, mode = "function" )
            nOverlap <- length( intersect( vCandNames, GetFormalNames( fnObj ) ) )
            dfScores[ nrow( dfScores ) + 1L, ] <- list( i, strFn, nOverlap )
        }
    }
    if ( !nrow( dfScores ) || max( dfScores$overlap ) == 0L ) {
        stop( "Could not match any candidate list to function formals in: ", strRFile )
    }
    
    #---------------------------------------------------------------------- -
    # Step 5: Select best-scoring pair and extract
    #---------------------------------------------------------------------- -
    dfBest <- dfScores[ which.max( dfScores$overlap ), ]
    fn <- get( dfBest$fn, envir = envTmp, mode = "function" )
    lArgs <- lCands[[ dfBest$cand ]]$value
    
    #---------------------------------------------------------------------- -
    # Step 6: Best-effort: fill missing names from formals
    #---------------------------------------------------------------------- -
    if ( !is.list( lArgs ) ) stop( "Chosen candidate is not a list." )
    vFormals <- GetFormalNames( fn )
    if ( is.null( names( lArgs ) ) || any( !nzchar( names( lArgs ) ) ) ) {
        n <- min( length( lArgs ), length( vFormals ) )
        if ( n > 0L ) names( lArgs )[ seq_len( n ) ] <- vFormals[ seq_len( n ) ]
    }
    
    #---------------------------------------------------------------------- -
    # Step 7: Return function, its name, and the chosen args
    #---------------------------------------------------------------------- -
    lOut <- list( fn = fn, fn_name = dfBest$fn, args = lArgs )
    return( lOut )
}
