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

# ====================================================================== =
# BugCheck code
# ====================================================================== =

# ====================================================================== =
# bugcheck/auto_select.R — Readable + stepwise comments (no export tags)
# ====================================================================== =

#' Null-coalescing operator
#'
#' Returns the left-hand side if it is not \code{NULL}, otherwise returns the right-hand side.
#'
#' @usage a \%||\% b
`%||%` <- function( a, b ) {
    #----------------------------------------------------------------------------- -
    # Return left if non-NULL; else right
    #----------------------------------------------------------------------------- -
    if ( !is.null( a ) ) a else b
}

#' Get non-dots formal argument names of a function
#'
#' @param fn A function object.
#' @return Character vector of formal names excluding "...".
GetFormalNames <- function( fn ) {
    #----------------------------------------------------------------------------- -
    # Step 1: Extract names; handle NULL
    #----------------------------------------------------------------------------- -
    v <- names( formals( fn ) )
    if ( is.null( v ) ) v <- character( 0 )
    
    #----------------------------------------------------------------------------- -
    # Step 2: Drop variadic token
    #----------------------------------------------------------------------------- -
    setdiff( v, "..." )
}

#' Traverse a nested object to collect candidate argument lists (NA-safe names)
#'
#' Adds entries for list nodes that contain at least one non-list element (i.e., "mixed" nodes).
#'
#' @param x Any R object (typically a nested list from an RDS).
#' @param path Internal path accumulator (do not supply).
#' @param out Internal output accumulator (do not supply).
#' @return A list of candidates: each item is list( path = <character>, value = <list> ).
CollectCandidates <- function( x, path = list(), out = list() ) {
    #----------------------------------------------------------------------------- -
    # Step 1: Recurse only into non-empty lists
    #----------------------------------------------------------------------------- -
    if ( is.list( x ) && length( x ) > 0L ) {
        #------------------------------------------------------------------------- -
        # Step 1a: Candidate if not all children are lists
        #------------------------------------------------------------------------- -
        all_lists <- all( vapply( x, is.list, logical( 1L ) ) )
        if ( !all_lists ) out[[ length( out ) + 1L ]] <- list( path = path, value = x )
        
        #------------------------------------------------------------------------- -
        # Step 1b: Recurse into children with NA-safe keys
        #------------------------------------------------------------------------- -
        vN <- names( x )
        for ( i in seq_along( x ) ) {
            nm  <- if ( is.null( vN ) ) "" else vN[ i ]
            key <- if ( !is.null( nm ) && !is.na( nm ) && nzchar( nm ) ) nm else i
            out <- CollectCandidates( x[[ i ]], c( path, key ), out )
        }
    }
    
    #----------------------------------------------------------------------------- -
    # Step 2: Return accumulator
    #----------------------------------------------------------------------------- -
    out
}

#' Parse ONLY function definitions from an .R file (avoid top-level execution)
#'
#' Captures direct function definitions and simple aliases, skipping arbitrary code.
#'
#' @param strRFile Path to .R file.
#' @param envir Environment to assign into (defaults to a new env with base parent).
#' @return list( env = <environment>, fun_names = <character> )
ParseFunctionsOnly <- function( strRFile, envir = new.env( parent = baseenv() ) ) {
    #----------------------------------------------------------------------------- -
    # Step 1: Parse file into expressions (no eval)
    #----------------------------------------------------------------------------- -
    exprs <- parse( strRFile, keep.source = FALSE )
    fun_names <- character( 0 )
    aliases   <- list()
    
    #----------------------------------------------------------------------------- -
    # Step 2: Walk expressions; assign function defs; record aliases
    #----------------------------------------------------------------------------- -
    for ( e in exprs ) {
        if ( is.call( e ) && as.character( e[[ 1L ]] ) %in% c( "<-", "=" ) ) {
            lhs <- e[[ 2L ]]; rhs <- e[[ 3L ]]
            if ( is.symbol( lhs ) ) {
                nm <- as.character( lhs )
                if ( is.call( rhs ) && identical( rhs[[ 1L ]], as.name( "function" ) ) ) {
                    assign( nm, eval( rhs, envir = envir, enclos = baseenv() ), envir = envir )
                    fun_names <- c( fun_names, nm )
                } else if ( is.symbol( rhs ) ) {
                    aliases[[ nm ]] <- as.character( rhs )  # alias: nm <- OtherName
                }
            }
        }
    }
    
    #----------------------------------------------------------------------------- -
    # Step 3: Resolve aliases that point to defined functions
    #----------------------------------------------------------------------------- -
    if ( length( aliases ) ) {
        for ( nm in names( aliases ) ) {
            tgt <- aliases[[ nm ]]
            if ( exists( tgt, envir = envir, mode = "function", inherits = FALSE ) ) {
                assign( nm, get( tgt, envir = envir, mode = "function" ), envir = envir )
                fun_names <- c( fun_names, nm )
            }
        }
    }
    
    #----------------------------------------------------------------------------- -
    # Step 4: Return environment + unique names
    #----------------------------------------------------------------------------- -
    list( env = envir, fun_names = unique( fun_names ) )
}

#' Build a call string from FORMALS (defensive; no eval of defaults)
#'
#' Produces a readable call signature (optionally including simple defaults) and a runnable closure.
#'
#' @param fn Function object.
#' @param lInputData Named list of arguments to pass when running.
#' @param strFnName Optional display name for the function.
#' @param strDataVar Label used for argument references in the call string (unused here, kept for API-compat).
#' @return list( fn, args, call_string, run )
BindFunctionCall <- function( fn, lInputData, strFnName = NULL, strDataVar = "lInputData" ) {
    #----------------------------------------------------------------------------- -
    # Step 1: Resolve a printable function name (no deparse/eval risk)
    #----------------------------------------------------------------------------- -
    if ( is.null( strFnName ) || !nzchar( strFnName ) ) {
        strFnName <- tryCatch({
            if ( is.character( fn ) && length( fn ) == 1L ) return( fn )
            nm <- deparse( substitute( fn ) )
            if ( length( nm ) && nzchar( nm[ 1L ] ) ) nm[ 1L ] else "<function>"
        }, error = function( e ) "<function>" )
    }
    
    #----------------------------------------------------------------------------- -
    # Step 2: Helper to format safe default labels
    #----------------------------------------------------------------------------- -
    .safe_label <- function( expr ) {
        if ( identical( expr, quote( expr = ) ) ) return( NULL )   # missing default
        if ( is.symbol( expr ) ) {
            sx <- as.character( expr )
            if ( sx %in% c( "NULL", "TRUE", "FALSE" ) ) return( sx )
            return( "…" )
        }
        if ( is.null( expr ) )                      return( "NULL" )
        if ( identical( expr, TRUE ) )              return( "TRUE" )
        if ( identical( expr, FALSE ) )             return( "FALSE" )
        if ( is.numeric( expr ) && length( expr ) == 1L )   return( as.character( expr ) )
        if ( is.character( expr ) && length( expr ) == 1L ) return( paste0( '"', expr, '"' ) )
        "…"
    }
    
    #----------------------------------------------------------------------------- -
    # Step 3: Build call string from formals; fall back to names-only
    #----------------------------------------------------------------------------- -
    call_str <- tryCatch({
        fmls <- formals( fn )
        if ( is.null( fmls ) || !length( fmls ) ) return( paste0( strFnName, "()" ) )
        nms <- names( fmls ); if ( is.null( nms ) ) nms <- character( 0 )
        pieces <- character( 0 )
        for ( nm in nms ) {
            if ( identical( nm, "..." ) ) next
            lab <- .safe_label( fmls[[ nm ]] )
            pieces <- c( pieces, if ( is.null( lab ) ) nm else paste0( nm, " = ", lab ) )
        }
        if ( !length( pieces ) ) paste0( strFnName, "()" ) else paste0( strFnName, "( ", paste( pieces, collapse = ", " ), " )" )
    }, error = function( e ) {
        fnames <- tryCatch({
            v <- names( formals( fn ) ); if ( is.null( v ) ) character( 0 ) else setdiff( v, "..." )
        }, error = function( e2 ) character( 0 ) )
        if ( length( fnames ) ) paste0( strFnName, "( ", paste( fnames, collapse = ", " ), " )" ) else paste0( strFnName, "()" )
    })
    
    #----------------------------------------------------------------------------- -
    # Step 4: Return runnable bundle
    #----------------------------------------------------------------------------- -
    list(
        fn          = fn,
        args        = lInputData,
        call_string = call_str,
        run         = function() do.call( fn, lInputData )
    )
}

#' Safely call a function (capture error and args)
#'
#' @param fn Function to call.
#' @param lArgs Named list of arguments.
#' @return list( ok, value, error_message, args_passed )
SafeCall <- function( fn, lArgs ) {
    #----------------------------------------------------------------------------- -
    # Step 1: Try to call; capture failures without throwing
    #----------------------------------------------------------------------------- -
    strErr <- NULL
    val <- tryCatch( do.call( fn, lArgs ), error = function( e ) { strErr <<- conditionMessage( e ); NULL } )
    
    #----------------------------------------------------------------------------- -
    # Step 2: Return structured status
    #----------------------------------------------------------------------------- -
    list( ok = is.null( strErr ), value = if ( is.null( strErr ) ) val else NULL, error_message = strErr, args_passed = lArgs )
}

#' Create a binder wrapper to simulate matching without evaluating body
#'
#' @param fn Function to mirror.
#' @return A function with the same formals whose body calls match.call().
.MakeBinder <- function( fn ) {
    #----------------------------------------------------------------------------- -
    # Step 1: Mirror formals into a wrapper
    #----------------------------------------------------------------------------- -
    fml  <- formals( fn )
    wrap <- function() {}
    formals( wrap ) <- fml
    
    #----------------------------------------------------------------------------- -
    # Step 2: Replace body with match.call and set environment
    #----------------------------------------------------------------------------- -
    body( wrap ) <- quote( match.call() )
    environment( wrap ) <- environment( fn ) %||% baseenv()
    wrap
}

#' Try binding a candidate argument list to a function's formals
#'
#' @param fn Function object.
#' @param args Candidate argument list.
#' @return Numeric score (>=0) or -Inf when binding fails.
.TryBindScore <- function( fn, args ) {
    #----------------------------------------------------------------------------- -
    # Step 1: Validate and normalize arg names
    #----------------------------------------------------------------------------- -
    if ( !is.list( args ) ) return( -Inf )
    argn <- names( args ); if ( is.null( argn ) ) argn <- rep( "", length( args ) ); argn[ is.na( argn ) ] <- ""
    
    #----------------------------------------------------------------------------- -
    # Step 2: Align unnamed args to leading formals; keep named intersection
    #----------------------------------------------------------------------------- -
    fml <- GetFormalNames( fn )
    A <- args
    if ( any( nzchar( argn ) ) ) {
        keep <- intersect( names( A ), fml )
        A <- A[ keep ]
        A <- A[ match( intersect( fml, names( A ) ), names( A ) ) ]
    } else {
        n <- min( length( args ), length( fml ) )
        if ( !n ) return( -Inf )
        names( A )[ seq_len( n ) ] <- fml[ seq_len( n ) ]
        A <- A[ seq_len( n ) ]
    }
    
    #----------------------------------------------------------------------------- -
    # Step 3: Use binder to test match without executing real body
    #----------------------------------------------------------------------------- -
    binder <- .MakeBinder( fn )
    ok <- TRUE
    tryCatch( do.call( binder, A ), error = function( e ) { ok <<- FALSE; NULL } )
    if ( !ok ) return( -Inf )
    
    #----------------------------------------------------------------------------- -
    # Step 4: Score by number of matched names
    #----------------------------------------------------------------------------- -
    length( intersect( names( A ), fml ) )
}

#' Lightweight predicate for "non-atomic-like" structures
.IsNonAtomicLike <- function( z ) {
    #----------------------------------------------------------------------------- -
    # Treat lists, data.frames, or any recursive object as structural
    #----------------------------------------------------------------------------- -
    is.list( z ) || is.data.frame( z ) || is.recursive( z )
}

#' Score how well a named list matches expected formal names
#'
#' @param lst Candidate list.
#' @param expected Character vector of expected names.
#' @return Large positive score for better matches; -Inf for non-matches.
.NamedListMatchScore <- function( lst, expected ) {
    #----------------------------------------------------------------------------- -
    # Step 1: Validate and extract names (NA-safe)
    #----------------------------------------------------------------------------- -
    if ( !is.list( lst ) || !length( lst ) ) return( -Inf )
    nms <- names( lst ); if ( is.null( nms ) ) nms <- rep( "", length( lst ) ); nms[ is.na( nms ) ] <- ""
    
    #----------------------------------------------------------------------------- -
    # Step 2: Count matching names and structural bonus
    #----------------------------------------------------------------------------- -
    match_names <- intersect( nms[ nzchar( nms ) ], expected )
    if ( !length( match_names ) ) return( -Inf )
    struct_bonus <- sum( vapply( match_names, function( k ) .IsNonAtomicLike( lst[[ k ]] ), logical( 1L ) ) )
    
    #----------------------------------------------------------------------------- -
    # Step 3: Combine into a weighted score
    #----------------------------------------------------------------------------- -
    100L * length( match_names ) + struct_bonus
}

#' Find lists under x that match expected names, scoring each
#'
#' @param x Nested object to search.
#' @param expected Expected formal names.
#' @param path Internal path accumulator.
#' @param out Internal output accumulator.
#' @return List of hits: each has path, value, score.
.FindNamedLists <- function( x, expected, path = list(), out = list() ) {
    #----------------------------------------------------------------------------- -
    # Step 1: If x is a list, score it and then recurse into children
    #----------------------------------------------------------------------------- -
    if ( is.list( x ) ) {
        sc <- .NamedListMatchScore( x, expected )
        if ( is.finite( sc ) && sc > -Inf ) out[[ length( out ) + 1L ]] <- list( path = path, value = x, score = sc )
        
        nms <- names( x )
        for ( i in seq_along( x ) ) {
            key <- if ( !is.null( nms ) && !is.na( nms[ i ] ) && nzchar( nms[ i ] ) ) nms[ i ] else i
            out <- .FindNamedLists( x[[ i ]], expected, c( path, key ), out )
        }
    }
    
    #----------------------------------------------------------------------------- -
    # Step 2: Return accumulator
    #----------------------------------------------------------------------------- -
    out
}

#' Promote an argument list to a better-aligned named/structured list from lAll
#'
#' @param lAll Root object (e.g., loaded RDS).
#' @param fn Function for which formals are expected.
#' @param args Current argument list.
#' @return Either original args or a promoted/realigned list.
.PromoteByExpectedNames <- function( lAll, fn, args ) {
    #----------------------------------------------------------------------------- -
    # Step 1: Get expected names; short-circuit if none
    #----------------------------------------------------------------------------- -
    expected <- GetFormalNames( fn )
    if ( !length( expected ) ) return( args )
    
    #----------------------------------------------------------------------------- -
    # Step 2: Search for best-matching named lists under lAll
    #----------------------------------------------------------------------------- -
    hits <- .FindNamedLists( lAll, expected )
    if ( !length( hits ) ) return( args )
    
    #----------------------------------------------------------------------------- -
    # Step 3: Take highest score; align names to expected, keep extras
    #----------------------------------------------------------------------------- -
    scores <- vapply( hits, function( h ) h$score, numeric( 1 ) )
    best   <- hits[[ which.max( scores ) ]]
    cand   <- best$value
    keep   <- intersect( names( cand ), expected )
    aligned <- cand[ keep ]
    extra   <- cand[ setdiff( names( cand ), keep ) ]
    c( aligned, extra )
}

#' Structure-aware auto-select best (function, args) pair
#'
#' Scores (candidate list, function) pairs by name overlap, bindability, and structure; promotes
#' args to better-aligned named lists when available.
#'
#' @param lAll Root object from RDS.
#' @param strRFile Path to .R file to parse functions from (no top-level eval).
#' @return list( fn, fn_name, args )
.AutoSelectBestPair <- function( lAll, strRFile ) {
    #----------------------------------------------------------------------------- -
    # Step 1: Parse functions only; prefer BuggedFunction name
    #----------------------------------------------------------------------------- -
    p      <- ParseFunctionsOnly( strRFile )
    envTmp <- p$env
    vFns   <- p$fun_names
    if ( !length( vFns ) ) stop( "No functions found in: ", strRFile )
    if ( "BuggedFunction" %in% vFns ) vFns <- c( "BuggedFunction", setdiff( vFns, "BuggedFunction" ) )
    
    #----------------------------------------------------------------------------- -
    # Step 2: Collect candidate lists from lAll
    #----------------------------------------------------------------------------- -
    cands <- CollectCandidates( lAll )
    if ( !length( cands ) ) stop( "No argument-list candidates in RDS." )
    
    #----------------------------------------------------------------------------- -
    # Step 3: Define structural score (counts complex children)
    #----------------------------------------------------------------------------- -
    struct_score <- function( a ) {
        if ( !is.list( a ) || !length( a ) ) return( 0L )
        sum( vapply( a, function( z ) .IsNonAtomicLike( z ), logical( 1L ) ) )
    }
    
    #----------------------------------------------------------------------------- -
    # Step 4: Score each (candidate, function) pair: overlap + bindability + structure
    #----------------------------------------------------------------------------- -
    S <- data.frame( cand = integer( 0 ), fn = character( 0 ), score = numeric( 0 ) )
    for ( i in seq_along( cands ) ) {
        ai <- cands[[ i ]]$value
        cn <- names( ai ); if ( is.null( cn ) ) cn <- rep( "", length( ai ) ); cn[ is.na( cn ) ] <- ""
        for ( fnm in vFns ) {
            f  <- get( fnm, envir = envTmp, mode = "function" )
            ov <- length( intersect( cn[ nzchar( cn ) ], GetFormalNames( f ) ) )
            bs <- .TryBindScore( f, ai )
            sc <- 100L * ov + 10L * if ( is.finite( bs ) ) bs else 0L + struct_score( ai )
            S[ nrow( S ) + 1L, ] <- list( i, fnm, sc )
        }
    }
    
    #----------------------------------------------------------------------------- -
    # Step 5: Select best pair and optionally promote arguments
    #----------------------------------------------------------------------------- -
    best    <- S[ which.max( S$score ), ]
    fn      <- get( best$fn, envir = envTmp, mode = "function" )
    fn_name <- best$fn
    args    <- cands[[ best$cand ]]$value
    args    <- .PromoteByExpectedNames( lAll, fn, args )
    
    #----------------------------------------------------------------------------- -
    # Step 6: Return bundle
    #----------------------------------------------------------------------------- -
    list( fn = fn, fn_name = fn_name, args = args )
}

#' Public: Run with TWO inputs only (RDS + R)
#'
#' @param strRds Path to .rds containing candidate parameter lists.
#' @param strRFile Path to .R containing candidate functions.
#' @param bPrintCall Logical; print reproducible call string when TRUE.
#' @return Structured result list with fields ok, value, error, call_string, fn, fn_name, args.
RunWithTwoInputs <- function( strRds, strRFile, bPrintCall = TRUE ) {
    #----------------------------------------------------------------------------- -
    # Step 1: Load candidate parameter lists from .rds
    #----------------------------------------------------------------------------- -
    lAll  <- readRDS( strRds )
    
    #----------------------------------------------------------------------------- -
    # Step 2: Auto-select the best (function, args) pair from .R file
    #----------------------------------------------------------------------------- -
    pick  <- .AutoSelectBestPair( lAll, strRFile )
    
    #----------------------------------------------------------------------------- -
    # Step 3: Bind into a reproducible call structure (includes function name)
    #----------------------------------------------------------------------------- -
    bound <- BindFunctionCall( pick$fn, pick$args, strFnName = pick$fn_name )
    
    #----------------------------------------------------------------------------- -
    # Step 4: Optionally print reproducible call string
    #----------------------------------------------------------------------------- -
    if ( isTRUE( bPrintCall ) ) {
        cat( "Reproducible call:\n", bound$call_string, "\n\n", sep = "" )
    }
    
    #----------------------------------------------------------------------------- -
    # Step 5: Run function safely, prefer RunBugCheck if available
    #----------------------------------------------------------------------------- -
    res <- if ( exists( "RunBugCheck", mode = "function" ) ) {
        RunBugCheck( bound$fn, pick$args )
    } else {
        SafeCall( bound$fn, pick$args )
    }
    
    #----------------------------------------------------------------------------- -
    # Step 6: Print outcome summary
    #----------------------------------------------------------------------------- -
    if ( isTRUE( res$ok ) ) {
        cat( "OK: function executed without error.\n" )
    } else {
        cat( "ERROR: ", res$error_message %||% "", "\n", sep = "" )
    }
    
    #----------------------------------------------------------------------------- -
    # Step 7: Return structured result bundle
    #----------------------------------------------------------------------------- -
    list(
        ok          = res$ok,
        value       = res$value,
        error       = res$error_message,
        call_string = bound$call_string,
        fn          = pick$fn,
        fn_name     = pick$fn_name,
        args        = pick$args
    )
}
