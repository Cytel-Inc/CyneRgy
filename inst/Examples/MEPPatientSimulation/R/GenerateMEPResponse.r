##' Generate Multi-Endpoint Patient Responses
##'
##' Simulates patient responses for 5 endpoints (3 survival, 1 binary, 1 continuous) using a multi-state model for survival endpoints.
##'
##' @param NumPat Number of patients
##' @param NumArms Number of treatment arms
##' @param TreatmentID Vector of treatment assignment for each patient
##' @param ArrivalTime Vector of patient arrival times
##' @param EndpointType Character vector of endpoint types ("survival", "binary", "continuous")
##' @param EndpointName Character vector of endpoint names
##' @param RespParams List of parameters for each endpoint:
##'   - For survival: list with SurvMethod and method-specific parameters:
##'       - SurvMethod=1: Hazard rate method. Use 'NumPiece', 'StartAtTime', 'HR', 'Control' (hazard rates, can be vectors for piecewise)
##'       - SurvMethod=2: Cumulative %survival method. Use 'ByTime', 'HR', 'Control' (%survival at ByTime)
##'       - SurvMethod=3: Median survival time method. Use 'HR', 'Control' (median survival time)
##'   - For binary: list with 'treat_prob' and 'control_prob'
##'   - For continuous: list with 'mean' and 'sd'
##' @param Correlation Correlation matrix for endpoints
##' @param UserParam List with optional scalar elements:
##'   - states: vector of state names for multi-state model (default: c("Healthy", "Disease", "Death"))
##'   - trans_H_D: transition rate from Healthy to Disease
##'   - trans_H_De: transition rate from Healthy to Death
##'   - trans_D_De: transition rate from Disease to Death
##'   - bin_flip: if 1, flips binary endpoint probability
##'   - cont_scale: scales continuous endpoint values
##'
##' @return List with Response (simulated data for each endpoint) and ErrorCode
GenMEPResp <- function(NumPat, NumArms, TreatmentID, ArrivalTime, EndpointType, EndpointName, RespParams, Correlation, UserParam = NULL) {
  # Error handling
  ErrorCode <- 0
  if (length(EndpointType) != 5 || length(EndpointName) != 5) {
    stop("Exactly 5 endpoints required: 3 survival, 1 binary, 1 continuous.")
  }
  # Support EndpointType as integer: 0=continuous, 1=binary, 2=survival
  valid_types <- c(0, 1, 2)
  if (!all(EndpointType %in% valid_types)) {
    stop("EndpointType must be 0 (continuous), 1 (binary), or 2 (survival).")
  }

  Response <- list()
  # Multi-state model for survival endpoints
  surv_idx <- which(EndpointType == 2)
  if (length(surv_idx) > 0) {
  states <- c("Healthy", "Disease", "Death")
    # Get transition rates from UserParam scalars
    trans_H_D <- if (!is.null(UserParam) && !is.null(UserParam$trans_H_D)) UserParam$trans_H_D else 0.1
    trans_H_De <- if (!is.null(UserParam) && !is.null(UserParam$trans_H_De)) UserParam$trans_H_De else 0.05
    trans_D_De <- if (!is.null(UserParam) && !is.null(UserParam$trans_D_De)) UserParam$trans_D_De else 0.2
    n_states <- length(states)
    # For each patient, simulate transitions for each survival endpoint
    for (i in seq_along(surv_idx)) {
      endpoint <- EndpointName[surv_idx[i]]
      param <- RespParams[[surv_idx[i]]]
        # Determine SurvMethod and simulate accordingly
        SurvMethod <- if (!is.null(param$SurvMethod)) param$SurvMethod else 1
        times <- matrix(NA, nrow=NumPat, ncol=n_states)
        colnames(times) <- states
  if (SurvMethod == 3) {
          # Median Survival Time method
          median <- param$Control
          hr <- param$HR
          for (pat in 1:NumPat) {
            treat <- TreatmentID[pat]
            med <- if (treat == 1) median else median * hr
            hazard <- log(2) / med
            current_state <- 1
            current_time <- 0
            times[pat, current_state] <- current_time
            while (current_state < n_states) {
              if (current_state == 1) {
                rate <- trans_H_D * hazard
              } else if (current_state == 2) {
                rate <- trans_D_De * hazard
              } else {
                rate <- 0
              }
              if (rate > 0) {
                t_next <- rexp(1, rate)
                current_time <- current_time + t_next
                current_state <- current_state + 1
                times[pat, current_state] <- current_time
              } else {
                break
              }
            }
          }
        } else if (SurvMethod == 1) {
          # Hazard rate method (piecewise possible)
          NumPiece <- param$NumPiece
          StartAtTime <- param$StartAtTime
          HR <- param$HR
          Control <- param$Control
          # Use first value if NumPiece > 1
          if (!is.null(NumPiece) && NumPiece > 1) {
            StartAtTime <- StartAtTime[1]
            HR <- HR[1]
            Control <- Control[1]
          }
          for (pat in 1:NumPat) {
            treat <- TreatmentID[pat]
            hazard <- if (treat == 1) Control else Control * HR
            current_state <- 1
            current_time <- 0
            times[pat, current_state] <- current_time
            while (current_state < n_states) {
              if (current_state == 1) {
                rate <- trans_H_D * hazard
              } else if (current_state == 2) {
                rate <- trans_D_De * hazard
              } else {
                rate <- 0
              }
              if (rate > 0) {
                t_next <- rexp(1, rate)
                current_time <- current_time + t_next
                current_state <- current_state + 1
                times[pat, current_state] <- current_time
              } else {
                break
              }
            }
          }
        } else if (SurvMethod == 2) {
          # Cumulative %survival method
          ByTime <- param$ByTime
          HR <- param$HR
          Control <- param$Control
          for (pat in 1:NumPat) {
            treat <- TreatmentID[pat]
            surv_pct <- if (treat == 1) Control else Control * HR
            # Simulate time to event based on cumulative survival percentage
            # For demonstration, use exponential approximation
            hazard <- -log(surv_pct / 100) / ByTime
            current_state <- 1
            current_time <- 0
            times[pat, current_state] <- current_time
            while (current_state < n_states) {
              if (current_state == 1) {
                rate <- trans_H_D * hazard
              } else if (current_state == 2) {
                rate <- trans_D_De * hazard
              } else {
                rate <- 0
              }
              if (rate > 0) {
                t_next <- rexp(1, rate)
                current_time <- current_time + t_next
                current_state <- current_state + 1
                times[pat, current_state] <- current_time
              } else {
                break
              }
            }
          }
        } else {
          stop("Unknown SurvMethod in RespParams.")
        }
  # For survival endpoints, return time to final state (Death)
  Response[[endpoint]] <- times[, n_states]
    }
  }
  # Binary endpoint
  bin_idx <- which(EndpointType == 1)
  if (length(bin_idx) == 1) {
    param <- RespParams[[bin_idx]]
    prob <- numeric(NumPat)
    for (pat in 1:NumPat) {
      treat <- TreatmentID[pat]
      if (treat == 1 && !is.null(param$Treatment)) {
        prob[pat] <- param$Treatment
      } else if (!is.null(param$Control)) {
        prob[pat] <- param$Control
      } else {
        prob[pat] <- NA
      }
    }
    Response[[EndpointName[bin_idx]]] <- rbinom(NumPat, 1, prob)
  }
  # Continuous endpoint
  cont_idx <- which(EndpointType == 0)
  if (length(cont_idx) == 1) {
    param <- RespParams[[cont_idx]]
    vals <- numeric(NumPat)
    for (pat in 1:NumPat) {
      treat <- TreatmentID[pat]
      if (treat == 1 && !is.null(param$Treatment)) {
        mean <- param$Treatment[1]
        sd <- param$Treatment[2]
      } else if (!is.null(param$Control)) {
        mean <- param$Control[1]
        sd <- param$Control[2]
      } else {
        mean <- NA
        sd <- NA
      }
      vals[pat] <- rnorm(1, mean, sd)
    }
    if (!is.null(UserParam) && !is.null(UserParam$cont_scale)) {
      vals <- vals * UserParam$cont_scale
    }
    Response[[EndpointName[cont_idx]]] <- vals
  }
  # Compute ArrivalRank: rank order of patient arrival times
  ArrivalRank <- rank(ArrivalTime, ties.method = "first")

  matrix_by_row <- matrix(unlist(Response), ncol = length(EndpointType), byrow = FALSE)
  Corr <- cor(matrix_by_row, method = "pearson")

  RetList <- list(Response = Response, ErrorCode = ErrorCode, ArrivalRank = ArrivalRank, Corr=NULL)
  return(RetList)
}