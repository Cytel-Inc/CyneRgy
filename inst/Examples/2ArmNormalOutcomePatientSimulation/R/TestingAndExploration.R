# Step 1 - source the desired files ####
source( "SimulatePatientOutcomePercentAtZero.R")

# Step 2 - Load any needed input, eg if you saved output from East you can manually or programmatically load inputs here ####
NumSub          <- readRDS( "../ExampleEastoutput/NumSub.Rds")
TreatmentID     <- readRDS( "../ExampleEastoutput/TreatmentID.Rds" )
Mean            <- readRDS( "../ExampleEastoutput/Mean.Rds" )
StdDev          <- readRDS( "../ExampleEastoutput/StdDev.Rds" )
UserParam       <- readRDS( "../ExampleEastoutput/UserParam.Rds" )

#UserParam       <- list( dProbOfZeroOutcomeExp = 0, dProbOfZeroOutcomeCtrl = 0)

# Step 3 - Example call to the desired function ####
lRet <- SimulatePatientOutcomePercentAtZero(NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam  )

# Step 4 - Check a few values from simulated data set and create a few visuals to make sure the function you developed appears to function as intended
# Compute the mean and standard deviation of the patient outcomes for each treatment
dMeanTrt0       <- mean( lRet$Response[ TreatmentID == 0 ] )    
dProb0Trt0      <- mean(  ifelse( lRet$Response[ TreatmentID == 0 ] == 0, 1, 0 ) )   # Compute the probability that the outcome = 0
dMeanTrt1       <- mean( lRet$Response[ TreatmentID == 1 ] )
dProb0Trt1      <- mean(  ifelse( lRet$Response[ TreatmentID == 1 ] == 0, 1, 0 ) )   # Compute the probability that the outcome = 0

# Inspect the output
dMeanTrt0 
dProb0Trt0
dMeanTrt1 
dProb0Trt1


# Step 5 - Create a few visual to check if the simulated data looks as expected ###

hist( lRet$Response[ TreatmentID == 0], main = "Control")
hist( lRet$Response[ TreatmentID == 1], main = "Experimental")

# It is always important to test any code that is developed before running extensive simulations

