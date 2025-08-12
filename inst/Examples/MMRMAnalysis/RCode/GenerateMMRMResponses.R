
# Define the simulation function
GenerateMMRMResponses <- function(NumSub, NumVisit, TreatmentID, Inputmethod, VisitTime,
                                  MeanControl, MeanTrt, StdDevControl, StdDevTrt, CorrMat,
                                  UserParam = NULL) {
    # —————————————————————————————————————————————————————————————
     # Libraries use in function
    # library(MASS)
    # —————————————————————————————————————————————————————————————
    
    # ————————————————————————————————————————————————————————————— 
    # Initialize an inputs
    # —————————————————————————————————————————————————————————————
    Error <- 0
    retval <- list()
    
    # —————————————————————————————————————————————————————————————
    # 1) Validate dimensions of inputs: means, SDs, and correlation matrix
    #    All must match NumVisit
    # —————————————————————————————————————————————————————————————
    if (length(MeanControl) != NumVisit ||
        length(MeanTrt)     != NumVisit ||
        length(StdDevControl) != NumVisit ||
        length(StdDevTrt)     != NumVisit ||
        nrow(CorrMat)       != NumVisit ||
        ncol(CorrMat)       != NumVisit) {
        
        Error <- -1                        # signal a dimension mismatch
        retval$ErrorCode <- as.integer(Error)
        return(retval)                    # bail out early
    }
    
    # —————————————————————————————————————————————————————————————
    # 2) Build covariance matrices for each arm:
    #    Σ = (sd_vector %*% t(sd_vector)) ∘ CorrMat
    #    so that Var_i = sd_i^2 and CorrMat provides off-diagonals
    # —————————————————————————————————————————————————————————————
    CovMatControl <- (StdDevControl %*% t(StdDevControl)) * CorrMat
    CovMatTrt     <- (StdDevTrt     %*% t(StdDevTrt))     * CorrMat
    
    # —————————————————————————————————————————————————————————————
    # 3) Draw multivariate‐normal samples for each arm:
    #      - one row per subject in that arm,
    #      - vector of per‐visit means,
    #      - and the above covariance matrix
    # —————————————————————————————————————————————————————————————
    ControlResponses <- MASS::mvrnorm(
        n     = sum(TreatmentID == 0),
        mu    = MeanControl,
        Sigma = CovMatControl
    )
    TrtResponses <- MASS::mvrnorm(
        n     = sum(TreatmentID == 1),
        mu    = MeanTrt,
        Sigma = CovMatTrt
    )
    
    # —————————————————————————————————————————————————————————————
    # 4) Combine into a full Responses matrix of size NumSub × NumVisit
    #    Rows with TreatmentID==0 get the control draws; 1 get the treatment draws
    # —————————————————————————————————————————————————————————————
    Responses <- matrix(0, nrow = NumSub, ncol = NumVisit)
    Responses[TreatmentID == 0, ] <- ControlResponses
    Responses[TreatmentID == 1, ] <- TrtResponses
    
    # —————————————————————————————————————————————————————————————
    # 5) Pack the simulated vectors into the return list:
    #      retval$Response1, retval$Response2, …, retval$ResponseN
    # —————————————————————————————————————————————————————————————
    for (i in seq_len(NumVisit)) {
        # as.double() ensures we return a numeric vector, not e.g. a matrix column
        retval[[paste0("Response", i)]] <- as.double(Responses[, i])
    }
    
    # —————————————————————————————————————————————————————————————
    # 6) Finalize and return
    # —————————————————————————————————————————————————————————————
    retval$ErrorCode <- as.integer(Error)
    return(retval)
}