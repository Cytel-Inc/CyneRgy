#============================= =
# Minimal Bug Checker (readable) 
#============================= =

#-------------------------------------------- -
# If a is not null return a otherwise return b
#-------------------------------------------- -
`%||%` <- function(a, b) {
  return(if (!is.null(a)) a else b)
}

#----------------------------- -
# Formatted stop with no call
#----------------------------- -
Failf <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

#----------------------------- -
# Check allowed file extension (case-insensitive) 
#----------------------------- -
HasExt <- function(strPath, vExts = c(".R", ".rds")) {
  if (!is.character(strPath) || length(strPath) != 1L) return(FALSE)
  strExt <- tolower(paste0(".", tools::file_ext(strPath)))
  return(strExt %in% tolower(vExts))
}

#----------------------------- -
# Resolve function input:
# - function object
# - unquoted symbol
# - function name
# - path to .R or .rds
#----------------------------- -
ResolveFunction <- function(functionInput) {
  # Already a function
  if (is.function(functionInput)) return(functionInput)
  
  # Unquoted symbol (e.g., PerformMMRMAnalysis)
  symInput <- substitute(functionInput)
  if (is.symbol(symInput)) {
    strName <- deparse(symInput)
    if (exists(strName, mode = "function", inherits = TRUE)) {
      return(get(strName, mode = "function", inherits = TRUE))
    }
    Failf("Function name '%s' not found in scope.", strName)
  }
  
  # Character: function name or path
  if (is.character(functionInput) && length(functionInput) == 1L) {
    if (file.exists(functionInput)) {
      strPath <- functionInput
      
      # .rds containing a function
      if (HasExt(strPath, ".rds")) {
        obj <- readRDS(strPath)
        if (!is.function(obj)) Failf("'.rds' did not contain a function object.")
        return(obj)
      }
      
      # .R file containing BuggedFunction or exactly one function
      if (HasExt(strPath, ".R")) {
        env <- new.env(parent = emptyenv())
        sys.source(strPath, envir = env)
        
        if (exists("BuggedFunction", envir = env, inherits = FALSE) &&
            is.function(get("BuggedFunction", envir = env))) {
          return(get("BuggedFunction", envir = env))
        }
        
        vObjs <- ls(env, all.names = TRUE)
        vFuns <- vObjs[vapply(vObjs, function(strNm) is.function(get(strNm, envir = env)), logical(1))]
        if (length(vFuns) == 1L) {
          return(get(vFuns[1], envir = env))
        }
        Failf("Could not resolve a single function from '%s'. Define `BuggedFunction` or only one function.", strPath)
      }
      
      Failf("Unsupported function file type for '%s' (use .R or .rds).", strPath)
    } else {
      strName <- functionInput
      if (exists(strName, mode = "function", inherits = TRUE)) {
        return(get(strName, mode = "function", inherits = TRUE))
      }
      Failf("Function name '%s' not found.", strName)
    }
  }
  
  Failf("`function_input` must be a function, a path to .R/.rds, or a function name (string).")
}

#----------------------------- -
# Resolve params: 
# - list 
# - path to .R or .rds 
#----------------------------- -
ResolveParams <- function(paramsInput) {
  if (is.list(paramsInput)) return(paramsInput)
  
  if (is.character(paramsInput) && length(paramsInput) == 1L && file.exists(paramsInput)) {
    strPath <- paramsInput
    
    # .rds containing a list
    if (HasExt(strPath, ".rds")) {
      obj <- readRDS(strPath)
      if (!is.list(obj)) Failf("'.rds' did not contain a list for lInputData.")
      return(obj)
    }
    
    # .R file exporting lInputData or plain objects
    if (HasExt(strPath, ".R")) {
      env <- new.env(parent = emptyenv())
      sys.source(strPath, envir = env)
      
      if (exists("lInputData", envir = env, inherits = FALSE)) {
        lLi <- get("lInputData", envir = env)
        if (!is.list(lLi)) Failf("In params .R, `lInputData` exists but is not a list.")
        return(lLi)
      }
      
      vObjs <- ls(env, all.names = TRUE)
      lVals <- lapply(vObjs, function(strNm) get(strNm, envir = env))
      names(lVals) <- vObjs
      vKeep <- !vapply(lVals, is.function, logical(1))
      lVals <- lVals[vKeep]
      if (!length(lVals)) Failf("Params .R had no `lInputData` and no plain objects to bundle.")
      return(lVals)
    }
    
    Failf("Unsupported params file type for '%s' (use .R or .rds).", strPath)
  }
  
  Failf("`params_input` must be a list or a path to .R/.rds.")
}

#----------------------------- -
# RunBugCheck: safely call a function
# Returns list(ok, value, error_message, args_passed)
#----------------------------- -
RunBugCheck <- function(buggedFunction, lInputData) {
  if (!is.function(buggedFunction)) Failf("`bugged_function` must be a function object.")
  if (!is.list(lInputData))        Failf("`lInputData` must be a named list.")
  
  vFormals     <- formals(buggedFunction) %||% pairlist()
  vFormalNames <- names(vFormals) %||% character(0)
  bHasDots     <- any(vFormalNames == "...")
  vGiven       <- names(lInputData) %||% character(0)
  vPass        <- if (bHasDots) vGiven else intersect(vGiven, setdiff(vFormalNames, "..."))
  lArgs        <- lInputData[vPass]
  
  strErr <- NULL
  val <- tryCatch(
    do.call(buggedFunction, lArgs),
    error = function(e) { strErr <<- conditionMessage(e); NULL }
  )
  
  lRes <- list(
    ok            = is.null(strErr),
    value         = if (is.null(strErr)) val else NULL,
    error_message = strErr,
    args_passed   = lArgs
  )
  return(lRes)
}

#----------------------------- - 
# PrintBugCheckResult: pretty print output   
#----------------------------- -   
PrintBugCheckResult <- function(res) {
  if (!is.list(res) || is.null(res$ok)) {
    cat("Invalid result object.\n")
    return(invisible(res))
  }
  
  if (isTRUE(res$ok)) {
    cat("OK: function executed without error.\nValue preview:\n")
    try(utils::str(res$value, max.level = 1L, give.attr = FALSE), silent = TRUE)
  } else {
    cat("ERROR: ", res$error_message %||% "", "\n", sep = "")
    if (length(res$args_passed)) {
      cat("ARGS PASSED: ", paste(names(res$args_passed), collapse = ", "), "\n", sep = "")
    }
  }
  
  return(invisible(res))
}

#============================= =
# End of Minimal Bug Checker 
#============================= =





#==============================
# Utility Functions
#==============================

#------------------------------
# Null-coalescing operator
#------------------------------
`%||%` <- function(a, b) {
    return(if (!is.null(a)) a else b)
}

#------------------------------
# Ensure a directory exists
#------------------------------
EnsureDir <- function(path) {
    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
    invisible(TRUE)
}

#------------------------------
# Sanitize a string for safe filenames
#------------------------------
MakeSafeName <- function(x) {
    s <- gsub("[^A-Za-z0-9._-]+", "_", x)
    s <- sub("^_+", "", s)
    if (!nzchar(s)) s <- "code"
    return(s)
}

#------------------------------
# Read a text file as a single string
#------------------------------
ReadTextFile <- function(path) {
    if (!file.exists(path)) return("")
    return(paste(readLines(path, warn = FALSE), collapse = "\n"))
}

#------------------------------
# Normalize R code for hashing
#------------------------------
CanonicalizeCode <- function(txt) {
    if (!nzchar(txt)) return("")
    v <- unlist(strsplit(gsub("\r\n?", "\n", txt), "\n"))
    v <- sub("^[ \t]+", "", v)
    v <- sub("[ \t]+$", "", v)
    keep <- !(v == "" & c(FALSE, head(v == "", -1)))
    v <- v[keep]
    return(paste(v, collapse = "\n"))
}

#------------------------------
# Compute MD5 hash of code text or file
#------------------------------
CodeHash <- function(txtOrPath) {
    txt <- if (file.exists(txtOrPath)) ReadTextFile(txtOrPath) else txtOrPath
    canon <- CanonicalizeCode(txt)
    if (!nzchar(canon)) return(NA_character_)
    tmp <- tempfile(fileext = ".txt"); on.exit(unlink(tmp), add = TRUE)
    cat(canon, file = tmp)
    return(unname(tools::md5sum(tmp)))
}

#------------------------------
# Find the largest .R file in a directory
#------------------------------
FindPrimaryRScript <- function(trialDir) {
    vR <- list.files(trialDir, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
    if (!length(vR)) return(NA_character_)
    sizes <- suppressWarnings(file.info(vR)$size)
    sizes[is.na(sizes)] <- -Inf
    return(vR[which.max(sizes)])
}

#------------------------------
# Safe RDS reader (returns list with status)
#------------------------------
SafeReadRDS <- function(path) {
    out <- tryCatch({
        val <- readRDS(path)
        list(ok = TRUE, value = val, path = path)
    }, error = function(e) {
        warning("Could not read RDS: ", path, " (", conditionMessage(e), ")")
        list(ok = FALSE, value = NULL, path = path)
    })
    return(out)
}

#------------------------------
# Always return a list-of-lists keyed by RDS filename
#------------------------------
CombineRDSObjects <- function(vals, srcs) {
    out <- list()
    for (i in seq_along(vals)) {
        obj <- vals[[i]]
        fname <- tools::file_path_sans_ext(basename(srcs[[i]]))
        if (is.list(obj)) {
            out[[fname]] <- obj
        } else if (is.data.frame(obj)) {
            out[[fname]] <- list(data = obj)
        } else {
            out[[fname]] <- list(value = obj)
        }
    }
    return(out)
}

#------------------------------
# Remove everything inside a directory
# but keep the directory itself
#------------------------------
PurgeDirContents <- function(dirPath) {
    if (!dir.exists(dirPath)) return(invisible(TRUE))
    kids <- list.files(dirPath, all.files = TRUE, full.names = TRUE, no.. = TRUE, include.dirs = TRUE)
    if (length(kids)) unlink(kids, recursive = TRUE, force = TRUE)
    invisible(TRUE)
}



