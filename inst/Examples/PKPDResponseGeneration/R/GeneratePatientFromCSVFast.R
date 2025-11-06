GeneratePatientFromCSV <- function(
    NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime,
    MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat,
    UserParam = NULL
) {
  
  #-------------------------------------- -
  # Initialize return variables and error code
  #-------------------------------------- -
  nError <- 0L
  lReturn <- list()
  
  #-------------------------------------- -
  # Build CSV path and confirm it exists
  #-------------------------------------- -
  strCSVPath <- paste0("Inputs/", UserParam$InputFileName)
  if (!file.exists(strCSVPath)) {
    nError <- -1L
    lReturn$ErrorCode <- as.integer(nError)
    return(lReturn)
  }
  
  #-------------------------------------- -
  # Load or reuse cached CSV data
  #-------------------------------------- -
  if (!exists("gdfPatients", envir = .GlobalEnv)) {
    dfPatients <- tryCatch({
      read.csv(strCSVPath, check.names = FALSE, stringsAsFactors = FALSE)
    }, error = function(e) { nError <<- -2L; NULL })
    gdfPatients <<- dfPatients
  } else {
    dfPatients <- get("gdfPatients", envir = .GlobalEnv)
  }
  
  if (is.null(dfPatients)) {
    lReturn$ErrorCode <- as.integer(nError)
    return(lReturn)
  }
  
  #-------------------------------------- -
  # Check required Treatment column (strict match)
  #-------------------------------------- -
  if (!("Treatment" %in% colnames(dfPatients))) {
    nError <- -5L
    lReturn$ErrorCode <- as.integer(nError)
    return(lReturn)
  }
  
  #-------------------------------------- -
  # Coerce Treatment column strictly to integer 0/1
  #-------------------------------------- -
  vTrt <- suppressWarnings(as.integer(dfPatients[["Treatment"]]))
  vKeep <- !is.na(vTrt) & vTrt %in% c(0L, 1L)
  dfPatients <- dfPatients[vKeep, , drop = FALSE]
  dfPatients[["Treatment"]] <- vTrt[vKeep]
  
  #-------------------------------------- -
  # Validate and coerce Visit columns (Visit1..VisitK)
  #-------------------------------------- -
  vVisitCols <- paste0("Visit ", seq_len(NumVisit))
  if (!all(vVisitCols %in% colnames(dfPatients))) {
    nError <- -4L
    lReturn$ErrorCode <- as.integer(nError)
    return(lReturn)
  }
  
  for (strCol in vVisitCols) {
    xChr <- as.character(dfPatients[[strCol]])
    xChr[xChr %in% c("", "NA", "NaN", "na", "null", "N/A")] <- NA_character_
    dfPatients[[strCol]] <- suppressWarnings(as.double(xChr))
  }
  
  #-------------------------------------- -
  # Determine how many patients needed for each arm
  #-------------------------------------- -
  nNeedCtl <- sum(as.integer(TreatmentID) == 0L)
  nNeedTrt <- sum(as.integer(TreatmentID) == 1L)
  
  vIdxCtrl <- which(dfPatients[["Treatment"]] == 0L)
  vIdxTrt  <- which(dfPatients[["Treatment"]] == 1L)
  
  if (length(vIdxCtrl) < nNeedCtl || length(vIdxTrt) < nNeedTrt) {
    nError <- -6L
    lReturn$ErrorCode <- as.integer(nError)
    return(lReturn)
  }
  
  #-------------------------------------- -
  # Randomly select unique patient rows per treatment arm
  #-------------------------------------- -
  vTakeCtrl <- if (nNeedCtl > 0L) sample(vIdxCtrl, nNeedCtl, replace = FALSE) else integer(0)
  vTakeTrt  <- if (nNeedTrt > 0L) sample(vIdxTrt, nNeedTrt, replace = FALSE) else integer(0)
  
  #-------------------------------------- -
  # Map selected patients to subjects by requested treatment order
  #-------------------------------------- -
  vPick <- integer(NumSub)
  nCtl <- 0L
  nTrt <- 0L
  for (iSub in seq_len(NumSub)) {
    if (as.integer(TreatmentID[iSub]) == 0L) {
      nCtl <- nCtl + 1L
      vPick[iSub] <- vTakeCtrl[nCtl]
    } else {
      nTrt <- nTrt + 1L
      vPick[iSub] <- vTakeTrt[nTrt]
    }
  }
  
  #-------------------------------------- -
  # Build Response1..ResponseK values for each subject
  #-------------------------------------- -
  for (iVisit in seq_len(NumVisit)) {
    lReturn[[paste0("Response", iVisit)]] <- as.double(dfPatients[vPick, vVisitCols[iVisit]])
  }
  
  #-------------------------------------- -
  # Return assembled output with error code
  #-------------------------------------- -
  lReturn$ErrorCode <- as.integer(nError)
  return(lReturn)
}

