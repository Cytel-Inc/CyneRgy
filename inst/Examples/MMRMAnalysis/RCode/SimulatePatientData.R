
# ------------------------------------------------------
# Function: Simulate MMRM Responses and Return Long Format
# ------------------------------------------------------
simulate_mmrm_responses <- function(NumSub, NumVisit, TreatmentID, Inputmethod,
                                    VisitTime, MeanControl, MeanTrt,
                                    StdDevControl, StdDevTrt, CorrMat) {
    # ————————————————————————————————————————————————————————————— 
    # Libraries use in function
    # library(dplyr)
    # library(tidyr)
    # library(MASS)
    # ————————————————————————————————————————————————————————————— 
    
    
    # ————————————————————————————————————————————————————————————— 
    # Initialize an inputs
    # —————————————————————————————————————————————————————————————
    Error <- 0
    retval <- list()
    
    # —————————————————————————————————————————————————————————————
    # Validate dimensions
    # —————————————————————————————————————————————————————————————
    
    if (length(MeanControl) != NumVisit || length(MeanTrt) != NumVisit || 
        length(StdDevControl) != NumVisit || length(StdDevTrt) != NumVisit || 
        nrow(CorrMat) != NumVisit || ncol(CorrMat) != NumVisit) {
        stop("Input dimension mismatch.")
    }
    
    # —————————————————————————————————————————————————————————————
    # Covariance matrices
    # —————————————————————————————————————————————————————————————
    CovMatControl <- (StdDevControl %*% t(StdDevControl)) * CorrMat
    CovMatTrt <- (StdDevTrt %*% t(StdDevTrt)) * CorrMat
  
    # —————————————————————————————————————————————————————————————  
    # Simulate responses
    # —————————————————————————————————————————————————————————————
    ControlResponses <- MASS::mvrnorm(n = sum(TreatmentID == 0), mu = MeanControl, Sigma = CovMatControl)
    TrtResponses <- MASS::mvrnorm(n = sum(TreatmentID == 1), mu = MeanTrt, Sigma = CovMatTrt)
    
    Responses <- matrix(0, nrow = NumSub, ncol = NumVisit)
    Responses[TreatmentID == 0, ] <- ControlResponses
    Responses[TreatmentID == 1, ] <- TrtResponses
    colnames(Responses) <- paste0("Visit", 1:NumVisit)
    
    # —————————————————————————————————————————————————————————————
    # Convert to long-format data
    # —————————————————————————————————————————————————————————————
    long_data <- as.data.frame(Responses) %>%
        mutate(PatientID = 1:NumSub, Treatment = TreatmentID) %>%
        pivot_longer(cols = starts_with("Visit"),
                     names_to = "Visit",
                     values_to = "Response")
    
    long_data$Visit <- as.numeric(gsub("Visit", "", long_data$Visit)) - 1
    long_data$Treatment <- factor(long_data$Treatment, levels = c(0, 1), labels = c("Control", "Treatment"))
    
    return(long_data)
}
