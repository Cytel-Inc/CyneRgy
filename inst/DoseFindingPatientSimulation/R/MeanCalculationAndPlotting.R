
# ================================================================
# CALCULATE MEANS; PLOT SIMULATED RESPONSES AND DOSE-RESPONSE CURVES
# ================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# ================================================================
# 1. SIMULATION PARAMETERS
# ================================================================

set.seed(123)  # Use NULL for non-reproducible simulations

NumSub  <- 10000
NumArms <- 5

TreatmentID <- rep(0:(NumArms - 1), each = NumSub / NumArms)

StdDev <- c(18, 35, 35, 35, 35)
vDoses <- c(0, 150, 300, 450, 600)

# ================================================================
# 2. DEFINE CANDIDATE DOSE-RESPONSE MODELS
# ================================================================

CommonParam <- list(Dose1 = 150,
                    Dose2 = 300,
                    Dose3 = 450,
                    Dose4 = 600,
                    E0    = 0,
                    EMax  = 20)

Models <- list(Linear  = c(CommonParam, list(Model = "linear")),
               
               Emax    = c(CommonParam, list(Model = "emax",
                                             ED50  = 100)),
               
               SigEmax = c(CommonParam, list(Model = "SigEmax",
                                             ED50  = 280,
                                             Hill  = 4)),
               
               Beta    = c(CommonParam, list(Model = "beta",
                                             D1    = 2,
                                             D2    = 1.2,
                                             Scale = 700)),
               
               Exponential = c(CommonParam, list(Model = "exponential",
                                                 Delta = 270)))

# ================================================================
# 3. CALCULATE THEORETICAL MEANS FOR ALL CANDIDATE MODELS
# ================================================================
source("SimulatePatientOutcomeDoseFinding.Continuous.R")

dfCurve <- bind_rows(
    lapply(names(Models), function(modelName) {
        
        modelParam <- Models[[modelName]]
        
        data.frame(Model = modelName,
                   Dose = vDoses,
                   ExpectedMean = GenerateMeans(
                       vDoses,
                       modelParam
                   )$vModelMeans)
    })
)

cat("\nTheoretical mean responses for all candidate models:\n")
print(dfCurve)

# ================================================================
# 4. SELECT MODEL FOR SIMULATION
# ================================================================

# Change this to: "Linear", "Emax", "SigEmax", "Exponential" or "Beta"

SelectedModel <- "SigEmax"

stopifnot(SelectedModel %in% names(Models))

UserParam <- Models[[SelectedModel]]

# Select theoretical curve corresponding to simulation model
dfSelectedCurve <- dfCurve %>%
    filter(Model == SelectedModel)

# ================================================================
# 5. SIMULATE PATIENT RESPONSES FOR 1 MODEL
# ================================================================

lResult <- SimulatePatientOutcomeDoseFinding.Continuous(NumSub      = NumSub,
                                                        NumArms     = NumArms,
                                                        ArrivalTime = rep(0, NumSub),
                                                        TreatmentID = TreatmentID,
                                                        Mean        = NULL,
                                                        StdDev      = StdDev,
                                                        UserParam   = UserParam)

stopifnot(lResult$ErrorCode == 0)

# ================================================================
# 6. PREPARE AND SUMMARISE SIMULATED RESPONSES
# ================================================================

dfResponses <- data.frame(Dose     = vDoses[TreatmentID + 1],
                          Response = lResult$Response)

dfSummary <- dfResponses %>%
    group_by(Dose) %>%
    summarise(N            = n(),
              ObservedMean = mean(Response),
              ObservedSD   = sd(Response),
              .groups      = "drop")

# Add theoretical means to observed summary
dfSummary <- dfSummary %>%
    left_join(dfSelectedCurve %>% 
                  select(Dose, ExpectedMean), by = "Dose")

cat("\nObserved versus theoretical mean responses:\n")
print(dfSummary)

# ================================================================
# 7. PLOT 1: INDIVIDUAL RESPONSES AND DOSE-RESPONSE CURVE
# ================================================================

p1 <- ggplot() +
    
    # Individual patient responses
    geom_jitter(data = dfResponses,
                aes(x = Dose, y = Response),
                width = 12,
                alpha = 0.15,
                color = "grey50",
                size = 1) +
    
    # Observed mean +/- SD
    geom_errorbar(data = dfSummary,
                  aes(x = Dose,
                      ymin = ObservedMean - ObservedSD,
                      ymax = ObservedMean + ObservedSD),
                  width = 15,
                  color = "steelblue",
                  linewidth = 0.8) +
    
    # Observed means
    geom_point(data = dfSummary,
               aes(x = Dose, y = ObservedMean),
               color = "steelblue",
               size = 3) +
    
    # Theoretical dose-response curve
    geom_line(data = dfSelectedCurve,
              aes(x = Dose, y = ExpectedMean),
              color = "firebrick",
              linewidth = 1.2) +
    
    geom_point(data = dfSelectedCurve,
               aes(x = Dose, y = ExpectedMean),
               color = "firebrick",
               size = 3) +
    
    scale_x_continuous(breaks = vDoses) +
    
    # Zoom into the E0-EMax range without removing observations
    coord_cartesian(ylim = c(UserParam$E0, UserParam$EMax)) +
    
    labs(title = "Simulated Continuous Dose-Response",
         subtitle = paste0("Model: ", SelectedModel,
                           " | N = ", NumSub),
         x = "Dose",
         y = "Patient Response",
         caption = paste("Red: theoretical mean |",
                         "Blue: observed mean +/- SD |",
                         "Grey: individual responses")) +
    
    theme_minimal(base_size = 13) +
    
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "grey40"))

print(p1)

# ================================================================
# 8. PLOT 2: THEORETICAL VS OBSERVED MEAN RESPONSES
# ================================================================

p2 <- ggplot() +
    
    # Theoretical mean
    geom_line(data = dfSummary,
              aes(x = Dose,
                  y = ExpectedMean,
                  color = "Theoretical mean"),
              linewidth = 1.2) +
    
    geom_point(data = dfSummary,
               aes(x = Dose,
                   y = ExpectedMean,
                   color = "Theoretical mean"),
               size = 3) +
    
    # Observed mean
    geom_line(data = dfSummary,
              aes(x = Dose,
                  y = ObservedMean,
                  color = "Observed mean"),
              linewidth = 1,
              linetype = "dashed") +
    
    geom_point(data = dfSummary,
               aes(x = Dose,
                   y = ObservedMean,
                   color = "Observed mean" ),
               size = 3) +
    
    scale_color_manual(values = c("Theoretical mean" = "firebrick",
                                  "Observed mean" = "steelblue")) +
    
    scale_x_continuous(breaks = vDoses) +
    
    labs(title = "Theoretical vs Observed Dose-Response",
         subtitle = paste0(
             "Model: ", SelectedModel,
             " | N = ", NumSub),
         x = "Dose",
         y = "Mean Response",
         color = NULL) +
    
    theme_minimal(base_size = 13) +
    
    theme(plot.title = element_text(face = "bold"),
          legend.position = "bottom")

print(p2)

# ================================================================
# 9. OPTIONAL: EXPORT RESULTS
# ================================================================

# write.csv(dfCurveWide, "TheoreticalMeans_AllModels.csv", row.names = FALSE)
# write.csv(dfSummary, "ObservedVsTheoretical.csv", row.names = FALSE)
