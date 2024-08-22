source("simulatePatientOutcomeCHU9V2.R")

# Call the function with some parameter values
NumSub      <- 100
TreatmentID <- sample(0:1, NumSub, replace = TRUE)
Mean        <- c(0, 10)
StdDev      <- c(10.6, 10.6)


UserParam   <- list(dMeanBaselineCtrl = 25,
                    dMeanBaselineExp  = 25,
                    dStdDevBaselineCtrl = 10.6,
                    dStdDevBaselineExp =10.6 )

lRet<-SimulatePatientOutcome(NumSub, TreatmentID, Mean, StdDev, UserParam)

dfPatientData <- data.frame( TreatmentID = TreatmentID, 
                             PatientOutcome = lRet$Response)

# Test the simulated data ####
# Simulated Data Requirements 
# Note: Response is Baseline - Followup so a value above 0 means the patient improved. 
# Patient data requirements: 
# -36 < Response <= 36
# Given the setup above, control mean = 0, experimental mean = 10 

any( lRet$Response > 36)  # This should be false
any( lRet$Response < -36) # This should be false

mean( dfPatientData$PatientOutcome[ dfPatientData$TreatmentID == 0 ]) # Expected to be 0
mean( dfPatientData$PatientOutcome[ dfPatientData$TreatmentID == 1 ]) # Expected to close to 10 

table( dfPatientData)



# Load the ggplot2 package
library(ggplot2)

# Create a histogram of PatientOutcome by TreatmentID
ggplot(dfPatientData, aes(x=PatientOutcome, fill=factor(TreatmentID))) +
    geom_histogram(binwidth=1, alpha=0.5, position="identity") +
    labs(x="Patient Outcome", y="Count", fill="Treatment ID") +
    theme_minimal() +
    facet_grid(~factor(TreatmentID), scales = "free_y") +
    ggtitle("Histogram of Patient Outcomes by Treatment")

