# ==============================================================================
# East Horizon Validation R Script — Multi Look Only (Fully Documented)
# Story: PLATFORM-86673
# Scope: Explore R | Dose Finding | Continuous | Analysis | Decision from R code
#
# This script contains TWO main functions:
#   1. PerformDFContinuous_FixSeqPairwise_MultiLook (MultAdjMethod = 8)
#   2. PerformDFContinuous_FixSeqTrend_MultiLook    (MultAdjMethod = 9)
#
# And ONE router function:
#   3. PerformDFContinuousAnalysis
#
# All helper functions are fully documented inline.
#
# References:
#   - East Horizon R Templates | Analysis | Multi-Arm Confirmatory & Dose Finding
#     https://cytel1.atlassian.net/wiki/spaces/platform/pages/3726573569
# ==============================================================================


# ==============================================================================
# HELPER: Pool Adjacent Violators Algorithm (PAVA) — Robust Implementation
#
# Purpose:
#   Computes isotonic regression to enforce monotonicity on group means.
#   Used by the Trend Test to compute isotonic means and isotonic deltas.
#
# Parameters:
#   y         : numeric vector of values (e.g., group means)
#   w         : numeric vector of weights (e.g., group sample sizes)
#   increasing: logical — TRUE for non-decreasing (Right-Tail),
#                          FALSE for non-increasing (Left-Tail)
#
# Returns:
#   numeric vector of isotonic (monotone-constrained) values, same length as y
#
# Algorithm:
#   Iteratively pools adjacent blocks that violate the monotonicity constraint.
#   Each pooled block gets the weighted average of its constituent values.
#   Repeats until no violations remain.
# ==============================================================================
pava_robust <- function(y, w = NULL, increasing = TRUE)
{
  n <- length(y)
  if (is.null(w)) w <- rep(1, n)
  if (!increasing) y <- -y

  block_val <- y
  block_wt  <- w
  block_idx <- as.list(1:n)

  repeat {
    merged <- FALSE
    i <- 1
    while (i < length(block_val)) {
      if (block_val[i] > block_val[i + 1]) {
        new_wt  <- block_wt[i] + block_wt[i + 1]
        new_val <- (block_wt[i] * block_val[i] +
                    block_wt[i + 1] * block_val[i + 1]) / new_wt
        block_val[i] <- new_val
        block_wt[i]  <- new_wt
        block_idx[[i]] <- c(block_idx[[i]], block_idx[[i + 1]])
        block_val <- block_val[-(i + 1)]
        block_wt  <- block_wt[-(i + 1)]
        block_idx <- block_idx[-(i + 1)]
        merged <- TRUE
      } else {
        i <- i + 1
      }
    }
    if (!merged) break
  }

  result <- numeric(n)
  for (j in seq_along(block_val)) {
    result[block_idx[[j]]] <- block_val[j]
  }

  if (!increasing) result <- -result
  return(result)
}


# ==============================================================================
# HELPER: Fixed Sequence Flags
#
# Purpose:
#   Implements the Fixed Sequence gatekeeping procedure.
#   Tests p-values sequentially in a pre-specified order (highest dose first).
#   Rejects while raw_pval < alpha; stops at the first failure.
#   Fixed Sequence inherently controls FWER — full Total Alpha is used.
#
# Parameters:
#   raw_pval : numeric vector of raw p-values (dim = narms)
#   alpha    : numeric scalar — Total Alpha (DesignParam$Alpha)
#   narms    : integer — number of active treatment arms
#   seqpref  : integer vector — test sequence preference
#              (maps original arm index to test order)
#
# Returns:
#   list with:
#     flag_ord : integer vector (dim = narms) — 1 = rejected, 0 = not rejected
#                in ORIGINAL arm order
#     ord_pval : numeric vector (dim = narms) — p-values reordered by test
#                sequence
#
# Logic:
#   1. Reorder p-values according to seqpref (highest dose → lowest dose)
#   2. Sequentially test: if ord_pval[i] < alpha → flag[i] = 1, else break
#   3. Map flags back to original arm order
# ==============================================================================
FixSeq_flags_EH <- function(raw_pval, alpha, narms, seqpref)
{
  flag     <- rep(0L, narms)
  ord_pval <- numeric(narms)

  for (k in 1:narms) {
    ord_pval[seqpref[k]] <- raw_pval[k]
  }

  for (i in 1:narms) {
    if (ord_pval[i] < alpha) {
      flag[i] <- 1L
    } else {
      break
    }
  }

  flag_ord <- integer(narms)
  for (k in 1:narms) {
    flag_ord[k] <- flag[seqpref[k]]
  }

  return(list(flag_ord = flag_ord, ord_pval = ord_pval))
}


# ==============================================================================
# HELPER: Compute Pairwise Raw P-values
#
# Purpose:
#   Computes pairwise t-test (each treatment arm vs. control) to produce
#   raw p-values, test statistics, and delta estimates.
#
# Parameters:
#   SimData          : data.frame with columns TreatmentID and Response
#   NumTrt           : integer — total number of treatment arms (from DesignParam)
#   SelectedArmIndex : integer vector — indices of currently active treatment arms
#   TailType         : integer — 0 = Left-Tail, 1 = Right-Tail
#   VarType          : integer — 4 = Equal Variance (pooled), 5 = Unequal (Welch)
#
# Returns:
#   list with:
#     RawPval  : numeric vector (dim = length(SelectedArmIndex))
#                One-sided p-value from t-distribution
#     TestStat : numeric vector — t-statistic for each arm vs. control
#     Delta    : numeric vector — mean(Treatment) - mean(Control)
#
# Variance Handling:
#   VarType = 4 (Equal):
#     Pooled variance: Sp2 = ((nC-1)*varC + (nT-1)*varT) / (nC+nT-2)
#     SE = sqrt(Sp2 * (1/nC + 1/nT))
#     DOF = nC + nT - 2
#
#   VarType = 5 (Unequal / Welch):
#     SE = sqrt(varC/nC + varT/nT)
#     DOF = Welch-Satterthwaite approximation (rounded)
# ==============================================================================
ComputePairwiseRawPval <- function(SimData, NumTrt, SelectedArmIndex,
                                    TailType, VarType)
{
  CtrlResp <- SimData$Response[SimData$TreatmentID == 0]
  nC    <- length(CtrlResp)
  meanC <- mean(CtrlResp)
  varC  <- var(CtrlResp)

  nActive  <- length(SelectedArmIndex)
  RawPval  <- rep(NA_real_, nActive)
  TestStat <- rep(NA_real_, nActive)
  DeltaVec <- rep(NA_real_, nActive)

  for (j in seq_along(SelectedArmIndex)) {
    k <- SelectedArmIndex[j]
    TrtResp <- SimData$Response[SimData$TreatmentID == k]
    nT    <- length(TrtResp)
    meanT <- mean(TrtResp)
    varT  <- var(TrtResp)

    DeltaEst    <- meanT - meanC
    DeltaVec[j] <- DeltaEst

    if (VarType == 4L) {
      Sp2 <- ((nC - 1) * varC + (nT - 1) * varT) / (nC + nT - 2)
      SE  <- sqrt(Sp2 * (1/nC + 1/nT))
      dof <- nC + nT - 2
    } else {
      vC  <- varC / nC
      vT  <- varT / nT
      SE  <- sqrt(vC + vT)
      dof <- round((vC + vT)^2 / (vC^2/(nC - 1) + vT^2/(nT - 1)))
    }

    if (SE < 1e-12) {
      TestStat[j] <- 0.0
      RawPval[j]  <- 1.0
    } else {
      tstat <- DeltaEst / SE
      TestStat[j] <- tstat
      if (TailType == 1L) {
        # Right-Tail: P(T > tstat)
        RawPval[j] <- pt(tstat, df = dof, lower.tail = FALSE)
      } else {
        # Left-Tail: P(T < tstat)
        RawPval[j] <- pt(tstat, df = dof, lower.tail = TRUE)
      }
    }
  }

  return(list(RawPval = RawPval, TestStat = TestStat, Delta = DeltaVec))
}


# ==============================================================================
# HELPER: Compute Isotonic Delta
#
# Purpose:
#   Computes isotonic means via PAVA and derives isotonic deltas
#   (isotonic treatment mean - isotonic control mean) for each arm.
#   Used for PoC assessment and for futility checks when
#   FutBdryScale = 4 (Isotonic Delta Scale).
#
# Parameters:
#   SimData          : data.frame with TreatmentID and Response
#   SelectedArmIndex : integer vector — active treatment arm indices
#   TailType         : integer — 0 = Left, 1 = Right
#
# Returns:
#   list with:
#     IsotonicMeans : numeric vector (dim = 1 + length(SelectedArmIndex))
#                     Isotonic means for [control, treatment1, ..., treatmentK]
#     IsotonicDelta : numeric vector (dim = length(SelectedArmIndex))
#                     IsotonicMean[treatment] - IsotonicMean[control]
#     RawMeans      : numeric vector — raw (non-isotonic) group means
#     Weights       : numeric vector — sample sizes per group
# ==============================================================================
ComputeIsotonicDelta <- function(SimData, SelectedArmIndex, TailType)
{
  ArmIndex <- c(0, SelectedArmIndex)
  nArms    <- length(ArmIndex)
  Means    <- numeric(nArms)
  Weights  <- numeric(nArms)

  for (i in seq_along(ArmIndex)) {
    resp <- SimData$Response[SimData$TreatmentID == ArmIndex[i]]
    Means[i]   <- mean(resp)
    Weights[i] <- length(resp)
  }

  if (TailType == 1L) {
    # Right-Tail: enforce non-decreasing dose-response
    IsoMeans <- pava_robust(Means, Weights, increasing = TRUE)
  } else {
    # Left-Tail: enforce non-increasing dose-response
    IsoMeans <- pava_robust(Means, Weights, increasing = FALSE)
  }

  IsoDelta <- IsoMeans[2:nArms] - IsoMeans[1]
  return(list(IsotonicMeans = IsoMeans, IsotonicDelta = IsoDelta,
              RawMeans = Means, Weights = Weights))
}


# ==============================================================================
# HELPER: Proof of Concept (PoC) Assessment
#
# Purpose:
#   Evaluates whether the PoC threshold has been crossed for each treatment
#   arm (POCStatusArm) and overall (POCStatus).
#
# Inputs (from LookInfo):
#   PoCScale     : Integer (dim = 1), 0 = High Dose vs. Control
#   PoCThreshold : Numeric (dim = 1), scalar threshold value
#
# Parameters:
#   OrderedDelta   : numeric vector — isotonic deltas for active arms,
#                    ordered from lowest dose to highest dose
#   ThresholdValue : numeric scalar — PoCThreshold from LookInfo
#   TailType       : integer — 0 = Left, 1 = Right
#   ArmwiseFlag    : logical —
#                    TRUE  = Independent per-arm PoC (Pairwise test)
#                    FALSE = Sequential/monotonic PoC (Trend test)
#
# Returns:
#   list with:
#     Armwise_POC_Status : integer vector (dim = length(OrderedDelta))
#       1 = PoC Threshold crossed in current look for this arm
#       0 = PoC Threshold NOT crossed in current look for this arm
#
#     Overall_POC_Status : integer scalar (dim = 1)
#       1 = PoC Threshold crossed overall (highest dose arm)
#       0 = PoC Threshold NOT crossed overall
#       When PoCScale = 0 (High Dose vs. Control), overall PoC is
#       determined by the highest dose arm (last element).
#
# Monotonic Mode (ArmwiseFlag = FALSE, used for Trend Test):
#   Checks from highest dose arm downward. If any arm fails PoC,
#   all lower arms automatically fail too (cascade).
# ==============================================================================
POC_Assessment_EH <- function(OrderedDelta, ThresholdValue, TailType,
                               ArmwiseFlag = TRUE)
{
  NumArms     <- length(OrderedDelta)
  Armwise_POC <- rep(0L, NumArms)
  Overall_POC <- 0L

  if (ArmwiseFlag) {
    # ----- Independent per-arm PoC (Pairwise Test) -----
    if (TailType == 1L) {
      # Right-Tail: PoC attained if delta > threshold
      Armwise_POC[round(OrderedDelta, 7) > ThresholdValue] <- 1L
    } else {
      # Left-Tail: PoC attained if delta < threshold
      Armwise_POC[round(OrderedDelta, 7) < ThresholdValue] <- 1L
    }
    # Overall PoC: highest dose arm (PoCScale = 0)
    Overall_POC <- Armwise_POC[NumArms]

  } else {
    # ----- Sequential/monotonic PoC (Trend Test) -----
    for (i in NumArms:1) {
      if (is.na(OrderedDelta[i])) {
        Armwise_POC[i] <- 0L
      } else {
        if (TailType == 1L) {
          Armwise_POC[i] <- ifelse(
            round(OrderedDelta[i], 7) > ThresholdValue, 1L, 0L
          )
        } else {
          Armwise_POC[i] <- ifelse(
            round(OrderedDelta[i], 7) < ThresholdValue, 1L, 0L
          )
        }
      }
      if (Armwise_POC[i] == 0L) {
        if (i > 1) Armwise_POC[1:(i - 1)] <- 0L
        break
      }
    }
    Overall_POC <- Armwise_POC[NumArms]
  }

  return(list(
    Armwise_POC_Status = Armwise_POC,
    Overall_POC_Status = as.integer(Overall_POC)
  ))
}


# ==============================================================================
# HELPER: Compute Trend Test Statistic
#
# Purpose:
#   Computes the trend test statistic using contrast coefficients (CC)
#   applied to isotonic means. The statistic tests for a monotone
#   dose-response trend.
#
# Formula:
#   Numerator = sum(CC * IsotonicMeans[arms])
#   Variance  = sum(CC^2 * MSW / n_per_arm)
#   TestStat  = Numerator / sqrt(Variance)
#
# Parameters:
#   SimData       : data.frame with TreatmentID and Response
#   CC            : numeric vector — contrast coefficients
#   IsotonicMeans : numeric vector — PAVA-constrained means
#   RawMeans      : numeric vector — raw group means (for SS computation)
#   TotCompleters : numeric vector — sample sizes per group
#   NumTrtArms    : integer — number of active treatment arms
#   ArmIndex      : integer vector — arm indices including control (0)
#   VarType       : integer — 4 = Equal, 5 = Unequal
#
# Returns:
#   list with:
#     TestStat : numeric scalar — trend test statistic
#     SE       : numeric scalar — standard error
#     Variance : numeric scalar — variance of the contrast
# ==============================================================================
ComputeTrendTestStat <- function(SimData, CC, IsotonicMeans, RawMeans,
                                  TotCompleters, NumTrtArms, ArmIndex, VarType)
{
  TStats_Num <- sum(CC * IsotonicMeans[ArmIndex + 1])

  MSW <- numeric(length(ArmIndex))
  for (idx in seq_along(ArmIndex)) {
    i <- ArmIndex[idx]
    Response <- SimData$Response[SimData$TreatmentID == i]
    SS_i <- sum((Response - RawMeans[i + 1])^2)
    if (VarType == 5L) {
      # Unequal variance: arm-specific MSW
      MSW[idx] <- SS_i / (TotCompleters[i + 1] - 1)
    } else {
      # Equal variance: pooled MSW across all arms in the test
      MSW[idx] <- SS_i / (sum(TotCompleters[ArmIndex + 1]) -
                           (NumTrtArms + 1))
    }
  }

  if (VarType == 4L) {
    MSW_use <- sum(MSW)
  } else {
    MSW_use <- MSW
  }

  Variance <- sum(CC^2 * MSW_use / TotCompleters[ArmIndex + 1])
  SE       <- sqrt(Variance)
  TestStat <- TStats_Num / SE

  return(list(TestStat = TestStat, SE = SE, Variance = Variance))
}


# ==============================================================================
# HELPER: Get Contrast Coefficients for Trend Test
#
# Purpose:
#   Returns equispaced linear contrast coefficients for the trend test.
#   The number of coefficients equals the total number of arms in the
#   current test (control + active treatment arms).
#
# Mapping:
#   5 arms → c(-2, -1, 0, 1, 2)
#   4 arms → c(-3, -1, 1, 3)
#   3 arms → c(-1, 0, 1)
#   2 arms → c(-1, 1)
#   Other  → equispaced from -(n-1) to (n-1) by 2
#
# Note:
#   Coefficients change as arms are removed during the Fixed Sequence
#   procedure. Each iteration recomputes CC for the current arm set.
# ==============================================================================
GetContrastCoefficients <- function(numTotalArms)
{
  CC <- switch(as.character(numTotalArms),
    "5" = c(-2, -1, 0, 1, 2),
    "4" = c(-3, -1, 1, 3),
    "3" = c(-1, 0, 1),
    "2" = c(-1, 1),
    seq(-(numTotalArms - 1), (numTotalArms - 1), by = 2)
  )
  return(CC)
}


# ==============================================================================
# FUNCTION 1: Multi Look | Fixed Sequence Pairwise | Decision from R Code
# ==============================================================================
# MultAdjMethod = 8
# Statistical Designs: Group Sequential, GS with Treatment Selection
#
# Template: PerformMAMSDecision (Multi-Arm Multi-Stage)
#
# ---- ALPHA ----
# Uses DesignParam$Alpha (Total Alpha) directly.
# Fixed Sequence gatekeeping inherently controls FWER at every look.
# CumAlpha is NaN for Dose Finding — NOT used.
#
# ---- DECISION MECHANISM ----
# Pairwise t-test (each treatment vs. control) → raw p-value
# FixSeq_flags: sequentially test raw_pval < TotalAlpha from highest dose down
# On efficacy: remove arm, re-estimate parameters, continue to next lower arm
# On failure: assign futility to current arm + all remaining lower arms, break
#
# ---- INPUT ARGUMENTS ----
# SimData     : data.frame — Simulated patient-level data
#   $TreatmentID : integer — 0 = Control, 1..NumTrt = Treatment arms
#   $Response    : numeric — Continuous endpoint response value
#   $ArrivalTime : numeric — Patient arrival time
#
# DesignParam : list — Design-level parameters (constant across looks)
#   $NumTreatments : integer (dim=1) — Number of treatment arms (excl. control)
#   $Alpha         : numeric (dim=1) — Total one-sided significance level
#   $TailType      : integer (dim=1) — 0 = Left-Tail, 1 = Right-Tail
#   $VarType       : integer (dim=1) — 4 = Equal Variance, 5 = Unequal Variance
#   $IsArmPresent  : integer (dim=NumTreatments) — 1 = arm active, 0 = dropped
#   $MultAdjMethod : integer (dim=1) — 8 = Fixed Seq Pairwise
#
# LookInfo    : list — Look-specific parameters
#   $NumLooks      : integer (dim=1) — Total number of looks
#   $CurrLookIndex : integer (dim=1) — Current look number (1-based)
#   $EffBdry       : numeric (dim=NumLooks) — Efficacy boundaries (Z-scale)
#   $FutBdry       : numeric (dim=NumLooks) — Futility boundaries (may be NA)
#   $FutBdryScale  : integer (dim=1) — 2=Delta, 4=Isotonic Delta, 6=HR
#   $PoCScale      : integer (dim=1) — 0 = High Dose vs. Control
#   $PoCThreshold  : numeric (dim=1) — PoC threshold value (scalar)
#
# OutList     : list or NULL — Pass-through from previous look
#   Engine sets this to NULL for the first look.
#   Supports: Numeric/Integer/Character Scalar & Vector, List.
#
# UserParam   : list or NULL — User-specified custom variables
#
# ---- RETURNED OUTPUTS ----
# All outputs are documented inline below at the return() statement.
# ==============================================================================
PerformDFContinuous_FixSeqPairwise_MultiLook <- function(SimData, DesignParam,
                                                          LookInfo,
                                                          OutList = NULL,
                                                          UserParam = NULL)
{
  ErrorCode <- 0L

  NumTrt       <- DesignParam$NumTreatments
  TotalAlpha   <- DesignParam$Alpha
  TailType     <- DesignParam$TailType
  VarType      <- DesignParam$VarType
  IsArmPresent <- DesignParam$IsArmPresent

  NumLooks      <- LookInfo$NumLooks
  CurrLookIndex <- LookInfo$CurrLookIndex
  FutBdryScale  <- LookInfo$FutBdryScale
  FutBdry       <- LookInfo$FutBdry

  PoCScale     <- LookInfo$PoCScale
  PoCThreshold <- LookInfo$PoCThreshold

  # ---------- Initialize Output Vectors ----------
  #
  # Decision     : dim = NumTreatments
  #                NA = dropped, 0 = no boundary, 1 = lower eff,
  #                2 = upper eff, 3 = futility, 4 = equivalence
  DecisionVec <- rep(0L, NumTrt)

  # RawPVal      : dim = NumTreatments
  #                Raw (unadjusted) one-sided p-value from pairwise t-test
  #                NA for dropped arms
  RawPValVec <- rep(NA_real_, NumTrt)

  # TestStat     : dim = NumTreatments
  #                Pairwise t-statistic (on Wald/Z scale) for each arm vs control
  #                NA for dropped arms
  TestStatVec <- rep(NA_real_, NumTrt)

  # Delta        : dim = NumTreatments
  #                Estimate of Delta = mean(Treatment) - mean(Control)
  #                NA for dropped arms
  #                Conditionally mandatory when FutBdryScale = 2 or 4
  DeltaVec <- rep(NA_real_, NumTrt)

  # IsoDeltaVec  : dim = NumTreatments (internal, used for PoC computation)
  #                Isotonic delta from PAVA — not directly returned
  IsoDeltaVec <- rep(NA_real_, NumTrt)

  # POCStatusArm : dim = NumTreatments
  #                Per-arm PoC threshold crossing status
  #                1 = crossed, 0 = not crossed
  #                Optional — Dose Finding Continuous only
  POCStatusArmVec <- rep(0.0, NumTrt)

  # POCStatus    : dim = 1 (scalar)
  #                Overall PoC threshold crossing status
  #                1 = crossed, 0 = not crossed
  #                Determined by highest dose arm when PoCScale = 0
  #                Optional — Dose Finding Continuous only
  OverallPOC <- 0.0

  # Analysis Time Computation:
  #   1. CompletionTime for each patient = ArrivalTime + RespLag
  #      (RespLag = Follow-Up Duration from DesignParam)
  #   2. Sort all completion times across ALL arms (including control
  #      and dropped arms — their patients are still in SimData)
  #   3. AnalysisTime = the CumCompleters[CurrLookIndex]-th sorted
  #      completion time
  
 
  RespLag <- ifelse(!is.null(DesignParam$RespLag), DesignParam$RespLag, 0)
  CompletionTimes <- sort(SimData$ArrivalTime + RespLag)
  TargetCompleters <- LookInfo$CumCompleters[CurrLookIndex]

  if (!is.null(TargetCompleters) &&
      !is.na(TargetCompleters) &&
      TargetCompleters <= length(CompletionTimes)) {
    EstAnalysisTime <- CompletionTimes[TargetCompleters]
  } else {
    EstAnalysisTime <- max(CompletionTimes, na.rm = TRUE)
  }


  tryCatch({

    SelectedArmIndex <- which(IsArmPresent == 1)

    if (length(SelectedArmIndex) == 0) {
      ErrorCode <- 1L
      return(list(
        Decision     = as.integer(DecisionVec),
        RawPVal      = as.double(RawPValVec),
        TestStat     = as.double(TestStatVec),
        Delta        = as.double(DeltaVec),
        POCStatusArm = as.double(POCStatusArmVec),
        POCStatus    = as.double(OverallPOC),
        OutList      = OutList,
        ErrorCode    = as.integer(ErrorCode)
      ))
    }

    # Mark dropped arms as NA in Decision
    DroppedArms <- which(IsArmPresent == 0)
    if (length(DroppedArms) > 0) {
      DecisionVec[DroppedArms] <- NA_integer_
    }

    # ------------------------------------------------------------------
    # Fixed Sequence Pairwise: Top-Down from Highest Dose
    #
    # Algorithm:
    #   1. Start with highest dose arm
    #   2. Compute pairwise raw p-values for all active arms vs. control
    #   3. Apply FixSeq_flags with TotalAlpha
    #   4. If highest arm rejected (efficacy):
    #      - Record efficacy decision
    #      - Remove arm from active set
    #      - Re-estimate parameters with remaining arms
    #      - Move to next lower arm
    #   5. If highest arm NOT rejected (futility):
    #      - Assign futility to this arm + all remaining lower arms
    #      - Break
    # ------------------------------------------------------------------
    NumActive   <- length(SelectedArmIndex)
    CurrentArms <- SelectedArmIndex
    TrtArmIndex <- CurrentArms[NumActive]

    while (NumActive > 0 && DecisionVec[TrtArmIndex] == 0L) {

      # Test sequence: highest dose first (reversed order)
      TestSequence <- NumActive:1

      # Compute pairwise raw p-values for currently active arms
      PairResult <- ComputePairwiseRawPval(
        SimData, NumTrt, CurrentArms, TailType, VarType
      )

      # Apply Fixed Sequence procedure with Total Alpha
      fixedSeq <- FixSeq_flags_EH(
        raw_pval = PairResult$RawPval,
        alpha    = TotalAlpha,
        narms    = NumActive,
        seqpref  = TestSequence
      )

      # Store results for the highest arm under test
      highIdx <- NumActive
      DeltaVec[TrtArmIndex]    <- PairResult$Delta[highIdx]
      RawPValVec[TrtArmIndex]  <- PairResult$RawPval[highIdx]
      TestStatVec[TrtArmIndex] <- PairResult$TestStat[highIdx]

      if (fixedSeq$flag_ord[highIdx] == 1L) {
        # ---- EFFICACY ----
        # Decision = 2 (Upper Efficacy) for Right-Tail
        # Decision = 1 (Lower Efficacy) for Left-Tail
        DecisionVec[TrtArmIndex] <- ifelse(TailType == 1L, 2L, 1L)

        # Remove this arm and continue with remaining
        CurrentArms <- CurrentArms[CurrentArms != TrtArmIndex]
        NumActive   <- length(CurrentArms)
        if (NumActive > 0) {
          TrtArmIndex <- CurrentArms[NumActive]
        }

      } else {
        # ---- NO EFFICACY: Check Futility Boundary ----
        HasFutility <- !is.null(FutBdry) && !all(is.na(FutBdry))
        FutBdryVal  <- NA

        if (HasFutility) FutBdryVal <- FutBdry[CurrLookIndex]

        if (HasFutility && !is.na(FutBdryVal)) {
          for (jj in seq_along(CurrentArms)) {
            arm <- CurrentArms[jj]
            if (is.na(DeltaVec[arm])) DeltaVec[arm] <- PairResult$Delta[jj]

            futility_hit <- FALSE
            if (FutBdryScale %in% c(2L, 4L)) {
              # FutBdryScale = 2 (Delta Scale) or 4 (Isotonic Delta Scale)
              # Compare Delta estimate against futility boundary
              if (TailType == 1L) {
                futility_hit <- (DeltaVec[arm] < FutBdryVal)
              } else {
                futility_hit <- (DeltaVec[arm] > FutBdryVal)
              }
            } else {
              # FutBdryScale on Z Scale
              # Compare test statistic against futility boundary
              ts <- PairResult$TestStat[jj]
              if (TailType == 1L) {
                futility_hit <- (ts < FutBdryVal)
              } else {
                futility_hit <- (ts > FutBdryVal)
              }
            }

            if (futility_hit && DecisionVec[arm] == 0L) {
              DecisionVec[arm] <- 3L  # Futility
            }
          }
        }

        # At final look: any arm still at 0 → futility (no more looks)
        if (CurrLookIndex == NumLooks) {
          lowerArmIdx <- CurrentArms[CurrentArms <= TrtArmIndex]
          remaining   <- lowerArmIdx[DecisionVec[lowerArmIdx] == 0L]
          if (length(remaining) > 0) {
            DecisionVec[remaining] <- 3L
          }
        }

        # Store results for all remaining arms
        for (jj in seq_along(CurrentArms)) {
          arm <- CurrentArms[jj]
          if (is.na(DeltaVec[arm]))    DeltaVec[arm]    <- PairResult$Delta[jj]
          if (is.na(RawPValVec[arm]))  RawPValVec[arm]  <- PairResult$RawPval[jj]
          if (is.na(TestStatVec[arm])) TestStatVec[arm]  <- PairResult$TestStat[jj]
        }
        break
      }
    }

    # ------------------------------------------------------------------
    # Isotonic Delta — computed for PoC assessment
    # ------------------------------------------------------------------
    ActiveForIso <- which(!is.na(DeltaVec))
    if (length(ActiveForIso) > 0) {
      IsoResult <- ComputeIsotonicDelta(SimData, ActiveForIso, TailType)
      IsoDeltaVec[ActiveForIso] <- IsoResult$IsotonicDelta
    }

    # ------------------------------------------------------------------
    # POC Assessment
    # Produces both POCStatusArm (per-arm) and POCStatus (overall)
    # PoCThreshold is scalar (dim=1) per Confluence spec
    # PoCScale = 0 → Overall determined by highest dose arm
    # ArmwiseFlag = TRUE → Independent per-arm for Pairwise test
    # ------------------------------------------------------------------
    if (!is.null(PoCThreshold) && length(ActiveForIso) > 0) {
      poc <- POC_Assessment_EH(
        OrderedDelta   = IsoDeltaVec[ActiveForIso],
        ThresholdValue = PoCThreshold,
        TailType       = TailType,
        ArmwiseFlag    = TRUE
      )
      POCStatusArmVec[ActiveForIso] <- poc$Armwise_POC_Status
      OverallPOC                    <- poc$Overall_POC_Status
    }

  }, error = function(e) {
    ErrorCode <<- -1L
  })

  # ------------------------------------------------------------------
  # OutList : User Specified (list)
  #   Pass-through list from current look to next look.
  #   Engine passes this as input OutList to the next look's call.
  #   Engine sets OutList = NULL for the first look.
  #   Supports: Numeric/Integer/Character Scalar & Vector, List.
  # ------------------------------------------------------------------
  NewOutList <- list(
    PrevDecisions = DecisionVec,
    PrevDeltas    = DeltaVec,
    PrevIsoDeltas = IsoDeltaVec,
    LookIndex     = CurrLookIndex
  )

  # ==================================================================
  # RETURN STATEMENT — All Outputs Documented
  #
  # Decision     : as.integer, dim = NumTreatments
  #   Boundary crossing decision per treatment arm.
  #   NA=Dropped, 0=NoBoundary, 1=LowerEff, 2=UpperEff, 3=Futility
  #   MANDATORY (Step 1 of Analysis Output Hierarchy).
  #   Since Decision is returned, all other outputs are OPTIONAL.
  #
  # RawPVal      : as.double, dim = NumTreatments
  #   Raw (unadjusted) one-sided p-value from pairwise t-test.
  #   NA for dropped arms.
  #   OPTIONAL (Step 5 of Hierarchy — not needed when Decision present).
  #
  # TestStat     : as.double, dim = NumTreatments
  #   Pairwise t-statistic on Wald (Z) scale for each arm vs. control.
  #   NA for dropped arms.
  #   OPTIONAL (Step 6 of Hierarchy — not needed when Decision present).
  #
  # Delta        : as.double, dim = NumTreatments
  #   Estimate of Delta = mean(Treatment) - mean(Control).
  #   NA for dropped arms.
  #   CONDITIONALLY MANDATORY when FutBdryScale = 2 (Delta) or 4 (Isotonic Delta).
  #   Also mandatory for GS with Treatment Selection without User R for TS.
  #   Used by Explore for: Market Share, Observed Delta chart, Critical Delta.
  #
  # POCStatusArm : as.double, dim = NumTreatments
  #   Per-arm PoC threshold crossing status.
  #   1 = PoC Threshold crossed in current look for this arm.
  #   0 = PoC Threshold NOT crossed.
  #   OPTIONAL — Dose Finding Continuous only.
  #   Computed using isotonic deltas vs. PoCThreshold.
  #
  # POCStatus    : as.double, dim = 1 (scalar)
  #   Overall PoC threshold crossing status.
  #   1 = PoC Threshold crossed overall.
  #   0 = PoC Threshold NOT crossed overall.
  #   When PoCScale = 0 (High Dose vs. Control), determined by highest dose arm.
  #   OPTIONAL — Dose Finding Continuous only.
  #
  # AnalysisTime : as.double, dim = 1 (scalar)
  #   Estimate of Analysis Time.
  #   Same as Look Time for interims; same as Study Duration for final.
  #   OPTIONAL.
  #
  # OutList      : list, dim = User Specified
  #   Pass-through from current look to next look.
  #   Engine passes this as input to the next look.
  #   NULL for first look. Supports Numeric/Integer/Character/List.
  #   OPTIONAL.
  #
  # ErrorCode    : as.integer, dim = 1 (scalar)
  #   0 = No Error.
  #   Positive Integer = Non-Fatal Error (current sim aborted, next proceeds).
  #   Negative Integer = Fatal Error (no further simulation attempted).
  #   OPTIONAL.
  # ==================================================================
  return(list(
    Decision     = as.integer(DecisionVec),
    RawPVal      = as.double(RawPValVec),
    TestStat     = as.double(TestStatVec),
    Delta        = as.double(DeltaVec),
    POCStatusArm = as.integer(POCStatusArmVec),
    POCStatus    = as.integer(OverallPOC),
    AnalysisTime = as.double(EstAnalysisTime),
    OutList      = NewOutList,
    ErrorCode    = as.integer(ErrorCode)
  ))
}


# ==============================================================================
# FUNCTION 2: Multi Look | Fixed Sequence Trend Test | Decision from R Code
# ==============================================================================
# MultAdjMethod = 9
# Statistical Designs: Group Sequential, GS with Treatment Selection
#
# Template: PerformMAMSDecision (Multi-Arm Multi-Stage)
#
# ---- ALPHA ----
# Uses DesignParam$Alpha (Total Alpha) directly.
# Fixed Sequence gatekeeping inherently controls FWER at every look.
# CumAlpha is NaN for Dose Finding — NOT used.
#
# ---- DECISION MECHANISM ----
# Trend test using contrast coefficients on isotonic means (PAVA).
# Critical value from t-distribution at TotalAlpha.
# Decision: TestStat > CritVal (Right-Tail) or TestStat < CritVal (Left-Tail)
# On efficacy: remove arm, recompute contrast coefficients, continue
# On failure: assign futility to current arm + all remaining lower arms, break
#
# ---- KEY DIFFERENCE FROM PAIRWISE ----
# - Uses isotonic means (PAVA) instead of raw means
# - Uses contrast coefficients that change with number of active arms
# - Decision via critical value comparison, NOT via raw p-value
# - RawPVal is NOT returned (not applicable for trend test)
# - PoC uses monotonic assessment (ArmwiseFlag = FALSE)
#
# ---- INPUT ARGUMENTS ----
# SimData     : data.frame — Simulated patient-level data
#   $TreatmentID : integer — 0 = Control, 1..NumTrt = Treatment arms
#   $Response    : numeric — Continuous endpoint response value
#   $ArrivalTime : numeric — Patient arrival time
#
# DesignParam : list — Design-level parameters (constant across looks)
#   $NumTreatments : integer (dim=1) — Number of treatment arms (excl. control)
#   $Alpha         : numeric (dim=1) — Total one-sided significance level
#   $TailType      : integer (dim=1) — 0 = Left-Tail, 1 = Right-Tail
#   $VarType       : integer (dim=1) — 4 = Equal Variance, 5 = Unequal Variance
#   $IsArmPresent  : integer (dim=NumTreatments) — 1 = arm active, 0 = dropped
#   $MultAdjMethod : integer (dim=1) — 9 = Fixed Seq Trend Test
#
# LookInfo    : list — Look-specific parameters
#   $NumLooks      : integer (dim=1) — Total number of looks
#   $CurrLookIndex : integer (dim=1) — Current look number (1-based)
#   $EffBdry       : numeric (dim=NumLooks) — Efficacy boundaries (Z-scale)
#   $FutBdry       : numeric (dim=NumLooks) — Futility boundaries (may be NA)
#   $FutBdryScale  : integer (dim=1) — 2=Delta, 4=Isotonic Delta, 6=HR
#   $PoCScale      : integer (dim=1) — 0 = High Dose vs. Control
#   $PoCThreshold  : numeric (dim=1) — PoC threshold value (scalar)
#
# OutList     : list or NULL — Pass-through from previous look
#   Engine sets this to NULL for the first look.
#
# UserParam   : list or NULL — User-specified custom variables
#
# ---- RETURNED OUTPUTS ----
# All outputs are documented inline below at the return() statement.
# ==============================================================================
PerformDFContinuous_FixSeqTrend_MultiLook <- function(SimData, DesignParam,
                                                       LookInfo,
                                                       OutList = NULL,
                                                       UserParam = NULL)
{
  ErrorCode <- 0L

  NumTrt       <- DesignParam$NumTreatments
  TotalAlpha   <- DesignParam$Alpha
  TailType     <- DesignParam$TailType
  VarType      <- DesignParam$VarType
  IsArmPresent <- DesignParam$IsArmPresent

  NumLooks      <- LookInfo$NumLooks
  CurrLookIndex <- LookInfo$CurrLookIndex
  FutBdryScale  <- LookInfo$FutBdryScale
  FutBdry       <- LookInfo$FutBdry

  PoCScale     <- LookInfo$PoCScale
  PoCThreshold <- LookInfo$PoCThreshold

  # ---------- Initialize Output Vectors ----------
  #
  # Decision     : dim = NumTreatments
  #                NA = dropped, 0 = no boundary, 1 = lower eff,
  #                2 = upper eff, 3 = futility, 4 = equivalence
  DecisionVec <- rep(0L, NumTrt)

  # TestStat     : dim = NumTreatments
  #                Trend contrast test statistic for each arm
  #                = sum(CC * IsotonicMeans) / SE
  #                NA for dropped arms
  #                NOTE: This is a TREND statistic, not a pairwise t-stat
  TestStatVec <- rep(NA_real_, NumTrt)

  # Delta        : dim = NumTreatments
  #                Raw (non-isotonic) delta = mean(Treatment) - mean(Control)
  #                NA for dropped arms
  #                Conditionally mandatory when FutBdryScale = 2 or 4
  DeltaVec <- rep(NA_real_, NumTrt)

  # IsoDeltaVec  : dim = NumTreatments (internal, used for PoC & futility)
  #                Isotonic delta from PAVA
  IsoDeltaVec <- rep(NA_real_, NumTrt)

  # POCStatusArm : dim = NumTreatments
  #                Per-arm PoC threshold crossing status
  #                1 = crossed, 0 = not crossed
  #                Uses MONOTONIC assessment for Trend Test
  POCStatusArmVec <- rep(0.0, NumTrt)

  # POCStatus    : dim = 1 (scalar)
  #                Overall PoC threshold crossing status
  #                1 = crossed, 0 = not crossed
  OverallPOC <- 0.0

  #   Analysis Time Computation:
  #   1. CompletionTime for each patient = ArrivalTime + RespLag
  #      (RespLag = Follow-Up Duration from DesignParam)
  #   2. Sort all completion times across ALL arms (including control
  #      and dropped arms — their patients are still in SimData)
  #   3. AnalysisTime = the CumCompleters[CurrLookIndex]-th sorted
  #      completion time
  
 
  RespLag <- ifelse(!is.null(DesignParam$RespLag), DesignParam$RespLag, 0)
  CompletionTimes <- sort(SimData$ArrivalTime + RespLag)
  TargetCompleters <- LookInfo$CumCompleters[CurrLookIndex]

  if (!is.null(TargetCompleters) &&
      !is.na(TargetCompleters) &&
      TargetCompleters <= length(CompletionTimes)) {
    EstAnalysisTime <- CompletionTimes[TargetCompleters]
  } else {
    EstAnalysisTime <- max(CompletionTimes, na.rm = TRUE)
  }

  tryCatch({

    SelectedArmIndex <- which(IsArmPresent == 1)

    if (length(SelectedArmIndex) == 0) {
      ErrorCode <- 1L
      return(list(
        Decision     = as.integer(DecisionVec),
        TestStat     = as.double(TestStatVec),
        Delta        = as.double(DeltaVec),
        POCStatusArm = as.integer(POCStatusArmVec),
        POCStatus    = as.integer(OverallPOC),
        OutList      = OutList,
        ErrorCode    = as.integer(ErrorCode)
      ))
    }

    DroppedArms <- which(IsArmPresent == 0)
    if (length(DroppedArms) > 0) {
      DecisionVec[DroppedArms] <- NA_integer_
    }

    # ------------------------------------------------------------------
    # Fixed Sequence Trend: Top-Down from Highest Dose
    #
    # Algorithm:
    #   1. Start with all active arms + control
    #   2. Compute isotonic means via PAVA
    #   3. Compute contrast coefficients for current arm count
    #   4. Compute trend test statistic
    #   5. Compute critical value from t-distribution at TotalAlpha
    #   6. If TestStat crosses CritVal (efficacy):
    #      - Record efficacy decision for highest arm
    #      - Remove arm from active set
    #      - Recompute with fewer arms (new CC, new PAVA)
    #      - Move to next lower arm
    #   7. If TestStat does NOT cross CritVal:
    #      - Check futility boundary
    #      - If futility: assign to this arm + all lower arms, break
    #      - If no futility and not final look: leave as 0 (continue)
    #      - If final look: assign futility to all remaining
    # ------------------------------------------------------------------
    NumActive   <- length(SelectedArmIndex)
    TrtArmIndex <- max(SelectedArmIndex)

    while (NumActive > 0 && DecisionVec[TrtArmIndex] == 0L) {

      # Include control (TreatmentID = 0) in the arm set
      ArmIndex         <- c(0, SelectedArmIndex)
      nTotalArmsInTest <- NumActive + 1

      # ---- Compute group statistics ----
      Means   <- numeric(nTotalArmsInTest)
      Wts     <- numeric(nTotalArmsInTest)
      SD_arms <- numeric(nTotalArmsInTest)
      for (idx in seq_along(ArmIndex)) {
        k    <- ArmIndex[idx]
        resp <- SimData$Response[SimData$TreatmentID == k]
        Means[idx]   <- mean(resp)
        Wts[idx]     <- length(resp)
        SD_arms[idx] <- sd(resp)
      }

      # ---- Isotonic Means via PAVA ----
      if (TailType == 1L) {
        IsoMeans <- pava_robust(Means, Wts, increasing = TRUE)
      } else {
        IsoMeans <- pava_robust(Means, Wts, increasing = FALSE)
      }

      # Store raw delta and isotonic delta for highest arm
      DeltaVec[TrtArmIndex]    <- Means[nTotalArmsInTest] - Means[1]
      IsoDeltaVec[TrtArmIndex] <- IsoMeans[nTotalArmsInTest] - IsoMeans[1]

      # ---- Degrees of Freedom ----
      if (VarType == 4L) {
        # Equal variance: pooled DOF
        dof <- sum(Wts) - nTotalArmsInTest
      } else {
        # Unequal variance: Welch-Satterthwaite for highest arm vs. control
        vC  <- (SD_arms[1]^2) / Wts[1]
        vT  <- (SD_arms[nTotalArmsInTest]^2) / Wts[nTotalArmsInTest]
        dof <- round(((vC + vT)^2) /
                     (vC^2/(Wts[1] - 1) + vT^2/(Wts[nTotalArmsInTest] - 1)))
      }

      # ---- Critical Value from t-distribution at TOTAL ALPHA ----
      # TotalAlpha used directly — Fixed Sequence controls FWER
      if (TailType == 1L) {
        CritVal <- qt(TotalAlpha, df = dof, lower.tail = FALSE)
      } else {
        CritVal <- qt(TotalAlpha, df = dof, lower.tail = TRUE)
      }

      # ---- Contrast Coefficients (change with number of active arms) ----
      CC <- GetContrastCoefficients(nTotalArmsInTest)

      # ---- Trend Test Statistic ----
      TS <- ComputeTrendTestStat(
        SimData, CC, IsoMeans, Means, Wts,
        NumActive, ArmIndex, VarType
      )
      TestStatVec[TrtArmIndex] <- TS$TestStat

      # ---- Decision: Test Stat vs. Critical Value ----
      if (TailType == 1L) {
        efficacy <- (TS$TestStat > CritVal)
      } else {
        efficacy <- (TS$TestStat < CritVal)
      }

      if (efficacy) {
        # ---- EFFICACY ----
        DecisionVec[TrtArmIndex] <- ifelse(TailType == 1L, 2L, 1L)
        SelectedArmIndex <- SelectedArmIndex[SelectedArmIndex != TrtArmIndex]
        NumActive <- length(SelectedArmIndex)
        if (NumActive > 0) {
          TrtArmIndex <- max(SelectedArmIndex)
        }

      } else {
        # ---- NO EFFICACY: Check Futility Boundary ----
        HasFutility <- !is.null(FutBdry) && !all(is.na(FutBdry))

        if (HasFutility) {
          FutBdryVal <- FutBdry[CurrLookIndex]
          if (!is.na(FutBdryVal)) {
            futility_hit <- FALSE
            if (FutBdryScale %in% c(2L, 4L)) {
              # Delta Scale / Isotonic Delta Scale
              # Compare isotonic delta against futility boundary
              if (TailType == 1L) {
                futility_hit <- (IsoDeltaVec[TrtArmIndex] < FutBdryVal)
              } else {
                futility_hit <- (IsoDeltaVec[TrtArmIndex] > FutBdryVal)
              }
            } else {
              # Z Scale — compare test statistic against futility boundary
              if (TailType == 1L) {
                futility_hit <- (TS$TestStat < FutBdryVal)
              } else {
                futility_hit <- (TS$TestStat > FutBdryVal)
              }
            }

            if (futility_hit) {
              # Futility cascades: this arm + all lower arms
              lowerArms <- SelectedArmIndex[SelectedArmIndex <= TrtArmIndex]
              DecisionVec[lowerArms] <- 3L
              break
            }
          }
        }

        # Final look: no boundary crossed → futility for all remaining
        if (CurrLookIndex == NumLooks) {
          lowerArms <- SelectedArmIndex[SelectedArmIndex <= TrtArmIndex]
          DecisionVec[lowerArms] <- 3L
        }
        break
      }
    }

    # ------------------------------------------------------------------
    # POC Assessment
    # Produces both POCStatusArm (per-arm) and POCStatus (overall)
    # ArmwiseFlag = FALSE → Monotonic assessment for Trend Test
    #   If highest arm fails PoC → all lower arms also fail (cascade)
    # ------------------------------------------------------------------
    ActiveForPoC <- which(!is.na(IsoDeltaVec))
    if (!is.null(PoCThreshold) && length(ActiveForPoC) > 0) {
      poc <- POC_Assessment_EH(
        OrderedDelta   = IsoDeltaVec[ActiveForPoC],
        ThresholdValue = PoCThreshold,
        TailType       = TailType,
        ArmwiseFlag    = FALSE
      )
      POCStatusArmVec[ActiveForPoC] <- poc$Armwise_POC_Status
      OverallPOC                    <- poc$Overall_POC_Status
    }

  }, error = function(e) {
    ErrorCode <<- -1L
  })


  # OutList: pass-through to next look
  NewOutList <- list(
    PrevDecisions = DecisionVec,
    PrevDeltas    = DeltaVec,
    PrevIsoDeltas = IsoDeltaVec,
    LookIndex     = CurrLookIndex
  )

  # ==================================================================
  # RETURN STATEMENT — All Outputs Documented
  #
  # Decision     : as.integer, dim = NumTreatments
  #   Boundary crossing decision per treatment arm.
  #   NA=Dropped, 0=NoBoundary, 1=LowerEff, 2=UpperEff, 3=Futility
  #   MANDATORY (Step 1 of Analysis Output Hierarchy).
  #   Since Decision is returned, all other outputs are OPTIONAL.
  #
  # TestStat     : as.double, dim = NumTreatments
  #   Trend contrast test statistic = sum(CC * IsotonicMeans) / SE.
  #   This is a TREND statistic (not pairwise t-stat).
  #   NA for dropped arms.
  #   OPTIONAL (Step 6 of Hierarchy — not needed when Decision present).
  #
  # Delta        : as.double, dim = NumTreatments
  #   Raw (non-isotonic) delta = mean(Treatment) - mean(Control).
  #   NA for dropped arms.
  #   CONDITIONALLY MANDATORY when FutBdryScale = 2 (Delta) or 4 (Isotonic Delta).
  #   Also mandatory for GS with Treatment Selection without User R for TS.
  #   Used by Explore for: Market Share, Observed Delta chart, Critical Delta.
  #
  # POCStatusArm : as.double, dim = NumTreatments
  #   Per-arm PoC threshold crossing status.
  #   1 = PoC Threshold crossed in current look for this arm.
  #   0 = PoC Threshold NOT crossed.
  #   Uses MONOTONIC assessment: if highest arm fails → all lower fail.
  #   OPTIONAL — Dose Finding Continuous only.
  #
  # POCStatus    : as.double, dim = 1 (scalar)
  #   Overall PoC threshold crossing status.
  #   1 = PoC Threshold crossed overall.
  #   0 = PoC Threshold NOT crossed overall.
  #   When PoCScale = 0 (High Dose vs. Control), determined by highest dose arm.
  #   OPTIONAL — Dose Finding Continuous only.
  #
  # AnalysisTime : as.double, dim = 1 (scalar)
  #   Estimate of Analysis Time.
  #   Same as Look Time for interims; same as Study Duration for final.
  #   OPTIONAL.
  #
  # OutList      : list, dim = User Specified
  #   Pass-through from current look to next look.
  #   Engine passes this as input to the next look.
  #   NULL for first look. Supports Numeric/Integer/Character/List.
  #   OPTIONAL.
  #
  # ErrorCode    : as.integer, dim = 1 (scalar)
  #   0 = No Error.
  #   Positive Integer = Non-Fatal Error (current sim aborted, next proceeds).
  #   Negative Integer = Fatal Error (no further simulation attempted).
  #   OPTIONAL.
  #
  # NOTE: RawPVal is NOT returned by the Trend Test function.
  #   The Trend Test makes decisions via TestStat vs. CritVal,
  #   not via raw p-values. This is a key difference from Pairwise.
  # ==================================================================
  return(list(
    Decision     = as.integer(DecisionVec),
    TestStat     = as.double(TestStatVec),
    Delta        = as.double(DeltaVec),
    POCStatusArm = as.integer(POCStatusArmVec),
    POCStatus    = as.integer(OverallPOC),
    AnalysisTime = as.double(EstAnalysisTime),
    OutList      = NewOutList,
    ErrorCode    = as.integer(ErrorCode)
  ))
}


# ==============================================================================
# FUNCTION 3: Unified Router (Multi Look Only)
#
# Purpose:
#   Reads MultAdjMethod from DesignParam and dispatches to the correct
#   analysis function. Only Multi Look is in scope for Dose Finding.
#
# Routing:
#   MCP = 8 → PerformDFContinuous_FixSeqPairwise_MultiLook
#   MCP = 9 → PerformDFContinuous_FixSeqTrend_MultiLook
#
# ==============================================================================
AnalyzeDoseFindingCont <- function(SimData, DesignParam,
                                         LookInfo,
                                         OutList = NULL,
                                         UserParam = NULL)
{

  MCP <- DesignParam$MultAdjMethod

  if (MCP == 8L) {
    return(PerformDFContinuous_FixSeqPairwise_MultiLook(
      SimData, DesignParam, LookInfo, OutList, UserParam
    ))
  } else {
    return(PerformDFContinuous_FixSeqTrend_MultiLook(
      SimData, DesignParam, LookInfo, OutList, UserParam
    ))
  } 
}
# ===============================================================================

