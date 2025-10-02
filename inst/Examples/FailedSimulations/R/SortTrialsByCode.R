#' Sort trial folders by primary R script hash
#'
#' Scans trial directories under a root, identifies a primary R script per trial (via
#' \code{FindPrimaryRScript}), computes a code hash (via \code{CodeHash}), groups trials with
#' identical hashes, and for each group writes a representative script and a combined RDS produced
#' by \code{CombineRDSObjects}. Trials with no identifiable R script are optionally collected into
#' a \code{NoRScript} bucket. The function can optionally purge and rebuild the output root.
#'
#' @param strInputRoot Character path to the root folder containing trial subdirectories.
#' @param strOutputRoot Character path to the output root. Defaults to \code{file.path( strInputRoot, "grouped_trials" )}.
#' @param bRecursiveTrials Logical; if \code{TRUE}, search for trial directories recursively under \code{strInputRoot}.
#' @param vChooseName Character scalar; one of \code{"script"} or \code{"hash"}. Controls how group
#'   folders are named: use the dominant script name in the group (\code{"script"}) or a short hash prefix (\code{"hash"}).
#' @param bDryRun Logical; if \code{TRUE}, do not create, modify, or delete files — only compute and return the manifest.
#' @param bPurgeOutputRoot Logical; if \code{TRUE} and not a dry run, delete existing contents of \code{strOutputRoot} before writing.
#'
#' @return An invisible \code{data.frame} manifest with one row per created group (plus an optional \code{NoRScript}
#'   row), including: \code{group_folder}, \code{code_hash_md5}, \code{script_name}, \code{combined_rds}, \code{kept_script_path},
#'   and \code{n_source_rds}.
#'
#' @details This function relies on several helper functions expected to exist in the calling package or environment:
#' \itemize{
#'   \item \code{EnsureDir( path )}: create a directory if it does not exist.
#'   \item \code{PurgeDirContents( path )}: remove all contents of a directory (but keep the directory).
#'   \item \code{FindPrimaryRScript( dir )}: return the path to the primary R script for a trial, or \code{NA_character_}.
#'   \item \code{CodeHash( path )}: compute a stable MD5 (or similar) hash of a script file's contents.
#'   \item \code{MakeSafeName( x )}: sanitize a string for safe use as a directory/file name.
#'   \item \code{SafeReadRDS( path )}: read an RDS file safely, returning a list with fields \code{ok} (logical) and \code{value}.
#'   \item \code{CombineRDSObjects( lVals, vSrcs )}: combine multiple deserialized R objects into one summary object.
#' }
#'
#' @section Side Effects:
#' When \code{bDryRun == FALSE}, this function creates a folder per code group under \code{strOutputRoot}, each containing only
#' the representative script (\code{<rep_script>.R}) and the combined RDS (\code{<rep_script>.rds}). It also writes
#' a CSV manifest at \code{file.path( strOutputRoot, "group_manifest.csv" )}.
#'
#' @examples
#' \dontrun{
#'   manifest <- SortTrialsByRCode( strInputRoot = "./trials" )
#'   print( manifest )
#' }
#'
#' @seealso \code{FindPrimaryRScript}, \code{CodeHash}, \code{CombineRDSObjects}
#'
#' @export
SortTrialsByRCode <- function( 
        strInputRoot,
        strOutputRoot = file.path( strInputRoot, "grouped_trials" ),
        bRecursiveTrials = FALSE,
        vChooseName = c( "script", "hash" ),
        bDryRun = FALSE,
        bPurgeOutputRoot = TRUE
) {
    vChooseName <- match.arg( vChooseName )
    
    if ( !dir.exists( strInputRoot ) ) stop( "strInputRoot does not exist: ", strInputRoot )
    
    # ------------------------------
    # Prepare output root (no writes on dry run)
    # ------------------------------
    if ( !bDryRun ) {
        if ( bPurgeOutputRoot && dir.exists( strOutputRoot ) ) {
            unlink( strOutputRoot, recursive = TRUE, force = TRUE )
        }
        EnsureDir( strOutputRoot )
    }
    
    # ------------------------------
    # Step 1: Collect trial directories
    # ------------------------------
    if ( bRecursiveTrials ) {
        vAllDirs <- list.dirs( strInputRoot, recursive = TRUE, full.names = TRUE )
        vTrialDirs <- Filter( function( strD ) {
            if ( identical( normalizePath( strD, winslash = "/", mustWork = FALSE ),
                            normalizePath( strOutputRoot, winslash = "/", mustWork = FALSE ) ) ) return( FALSE )
            vFilesHere <- list.files( strD, recursive = FALSE )
            return( any( grepl( "\\.[Rr]$", vFilesHere ) ) || any( grepl( "\\.rds$", vFilesHere, ignore.case = TRUE ) ) )
        }, vAllDirs )
    } else {
        vTrialDirs <- list.dirs( strInputRoot, full.names = TRUE, recursive = FALSE )
        vTrialDirs <- setdiff( vTrialDirs, strInputRoot )
    }
    
    if ( !length( vTrialDirs ) ) {
        message( "No trial folders found under: ", strInputRoot )
        return( invisible( data.frame() ) )
    }
    
    # ------------------------------
    # Step 2: Compute per-trial primary script + hash
    # ------------------------------
    lRecs <- lapply( vTrialDirs, function( strD ) {
        strRFile <- FindPrimaryRScript( strD )
        if ( is.na( strRFile ) ) {
            return( list( trial_dir = strD, primary_script = NA_character_, script_name = NA_character_, code_hash = NA_character_ ) )
        } else {
            strHash <- CodeHash( strRFile )
            return( list( trial_dir = strD, primary_script = strRFile,
                          script_name = tools::file_path_sans_ext( basename( strRFile ) ),
                          code_hash = strHash ) )
        }
    } )
    dfTrials <- do.call( rbind.data.frame, lapply( lRecs, as.data.frame, stringsAsFactors = FALSE ) )
    names( dfTrials ) <- c( "trial_dir", "primary_script", "script_name", "code_hash" )
    
    # ------------------------------
    # Step 3: Group by hash
    # ------------------------------
    dfTrials$group_key <- dfTrials$code_hash
    dfCode  <- dfTrials[ !is.na( dfTrials$group_key ) & nzchar( dfTrials$group_key ), , drop = FALSE ]
    dfEmpty <- dfTrials[ is.na( dfTrials$group_key ) | !nzchar( dfTrials$group_key ), , drop = FALSE ]
    
    lGroups <- split( dfCode, dfCode$group_key )
    lOutMap <- list()
    for ( strK in names( lGroups ) ) {
        dfG <- lGroups[[ strK ]]
        vScriptNames <- dfG$script_name[ !is.na( dfG$script_name ) & nzchar( dfG$script_name ) ]
        if ( !length( vScriptNames ) ) vScriptNames <- paste0( "code_", substr( strK, 1, 6 ) )
        strBestScript <- names( sort( table( vScriptNames ), decreasing = TRUE ) )[ 1 ]
        strBaseName <- if ( vChooseName == "script" ) strBestScript else paste0( "code_", substr( strK, 1, 6 ) )
        strFolderName <- MakeSafeName( strBaseName )
        if ( !is.null( lOutMap[[ strFolderName ]] ) ) {
            strFolderName <- MakeSafeName( paste0( strBaseName, "__", substr( strK, 1, 6 ) ) )
        }
        lOutMap[[ strFolderName ]] <- list( 
            hash = strK,
            trials = dfG$trial_dir,
            rep_script = strBestScript
        )
    }
    
    # ------------------------------
    # Step 4: Process groups (single write after purge; no writes on dry run)
    # ------------------------------
    dfManifest <- data.frame( 
        group_folder     = character( 0 ),
        code_hash_md5    = character( 0 ),
        script_name      = character( 0 ),
        combined_rds     = character( 0 ),
        kept_script_path = character( 0 ),
        n_source_rds     = integer( 0 ),
        stringsAsFactors = FALSE
    )
    
    for ( strFolderName in names( lOutMap ) ) {
        lG <- lOutMap[[ strFolderName ]]
        strGroupDir <- file.path( strOutputRoot, strFolderName )
        
        # Representative script selection (do not copy yet)
        vPrimaryScripts <- dfTrials$primary_script[ dfTrials$trial_dir %in% lG$trials ]
        vPrimaryScripts <- vPrimaryScripts[ !is.na( vPrimaryScripts ) ]
        if ( length( vPrimaryScripts ) ) {
            vSizes <- suppressWarnings( file.info( vPrimaryScripts )$size )
            vSizes[ is.na( vSizes ) ] <- -Inf
            strRepScriptSrc <- vPrimaryScripts[ which.max( vSizes ) ]
            strTargetRep <- file.path( strGroupDir, paste0( lG$rep_script, ".R" ) )
        } else {
            strRepScriptSrc <- NULL
            strTargetRep <- NA_character_
        }
        
        # Combine RDS (compute only; write later)
        vRdsFiles <- unlist( lapply( 
            lG$trials,
            function( strTd ) list.files( strTd, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE )
        ), use.names = FALSE )
        
        if ( length( vRdsFiles ) ) {
            lReads <- lapply( vRdsFiles, SafeReadRDS )
            vOkIdx <- which( vapply( lReads, `[[`, logical( 1 ), "ok" ) )
            if ( length( vOkIdx ) ) {
                lVals <- lapply( lReads[ vOkIdx ], `[[`, "value" )
                vSrcs <- vRdsFiles[ vOkIdx ]
                oCombined <- CombineRDSObjects( lVals, vSrcs )
                strOutRds <- file.path( strGroupDir, paste0( lG$rep_script, ".rds" ) )
                nSrc <- length( vSrcs )
            } else {
                strOutRds <- NA_character_
                nSrc <- 0L
                warning( "No readable RDS files in group: ", strGroupDir )
            }
        } else {
            strOutRds <- NA_character_
            nSrc <- 0L
        }
        
        dfManifest[ nrow( dfManifest ) + 1L, ] <- list( strFolderName, lOutMap[[ strFolderName ]]$hash,
                                                        lG$rep_script, strOutRds, strTargetRep, nSrc )
        
        # Minimal output: only keep the two files (guarded by dry run)
        if ( !bDryRun ) {
            if ( dir.exists( strGroupDir ) ) PurgeDirContents( strGroupDir ) else EnsureDir( strGroupDir )
            if ( !is.na( strTargetRep ) && !is.null( strRepScriptSrc ) && file.exists( strRepScriptSrc ) ) {
                file.copy( strRepScriptSrc, file.path( strGroupDir, paste0( lG$rep_script, ".R" ) ), overwrite = TRUE )
            }
            if ( !is.na( strOutRds ) && exists( "oCombined" ) ) {
                saveRDS( oCombined, file.path( strGroupDir, paste0( lG$rep_script, ".rds" ) ) )
            }
        }
    }
    
    # ------------------------------
    # Step 5: NoRScript bucket (same single-write pattern; guarded by dry run)
    # ------------------------------
    if ( nrow( dfEmpty ) ) {
        strNoCodeDir <- file.path( strOutputRoot, "NoRScript" )
        
        vRdsNC <- unlist( lapply( 
            dfEmpty$trial_dir,
            function( strTd ) list.files( strTd, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE )
        ), use.names = FALSE )
        
        if ( length( vRdsNC ) ) {
            lReadsNC <- lapply( vRdsNC, SafeReadRDS )
            vOkNC <- which( vapply( lReadsNC, `[[`, logical( 1 ), "ok" ) )
            if ( length( vOkNC ) ) {
                lValsNC <- lapply( lReadsNC[ vOkNC ], `[[`, "value" )
                vSrcsNC <- vRdsNC[ vOkNC ]
                oCombinedNC <- CombineRDSObjects( lValsNC, vSrcsNC )
                strOutNC <- file.path( strNoCodeDir, "NoRScript.rds" )
                
                dfManifest[ nrow( dfManifest ) + 1L, ] <- list( "NoRScript", NA_character_, "NoRScript",
                                                                strOutNC, NA_character_, length( vSrcsNC ) )
                
                if ( !bDryRun ) {
                    if ( dir.exists( strNoCodeDir ) ) PurgeDirContents( strNoCodeDir ) else EnsureDir( strNoCodeDir )
                    saveRDS( oCombinedNC, strOutNC )
                }
            } else if ( !bDryRun ) {
                if ( dir.exists( strNoCodeDir ) ) unlink( strNoCodeDir, recursive = TRUE, force = TRUE )
            }
        } else if ( !bDryRun ) {
            if ( dir.exists( strNoCodeDir ) ) unlink( strNoCodeDir, recursive = TRUE, force = TRUE )
        }
    }
    
    # ------------------------------
    # Step 6: Write manifest (guarded by dry run)
    # ------------------------------
    if ( !bDryRun ) {
        utils::write.csv( dfManifest, file.path( strOutputRoot, "group_manifest.csv" ), row.names = FALSE )
        message( "Grouped ", length( lOutMap ), " code bucket(s). NoRScript bucket ",
                 if ( nrow( dfEmpty ) ) "processed." else "not needed.",
                 " Manifest: ",
                 normalizePath( file.path( strOutputRoot, "group_manifest.csv" ), mustWork = FALSE ) )
    } else {
        message( "Dry run: computed manifest only. No files were written." )
    }
    
    return( invisible( dfManifest ) )
}


