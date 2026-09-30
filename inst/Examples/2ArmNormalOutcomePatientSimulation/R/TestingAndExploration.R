######################################################################################################################## .
#' @name TestingAndExploration
#'
#' @title Explore simulated continuous outcomes
#'
#' @description Load the saved example inputs, call SimulatePatientOutcomePercentAtZero, and inspect the
#'   mean response and proportion of zero outcomes in each arm. Plot the generated control and experimental
#'   responses to check the simulation before running larger experiments.
#'
#' @author J. Kyle Wathen
#'
#' @return This demonstration script stores the generated response list in lResult, prints the arm-specific
#'   summaries, and displays two histograms.
#'
#' @details Run this script from the example R directory. The bundled ExampleEastoutput directory supplies
#'   NumSub, TreatmentID, Mean, StdDev, and UserParam as RDS files. If ArrivalTime.Rds is available, load it;
#'   otherwise use zero arrival times, since this response generator does not use calendar enrollment times.
######################################################################################################################## .

# Step 1 - Load the response generator and saved inputs.
source( "SimulatePatientOutcomePercentAtZero.R" )
NumSub <- readRDS( "../ExampleEastoutput/NumSub.Rds" )
TreatmentID <- readRDS( "../ExampleEastoutput/TreatmentID.Rds" )
Mean <- readRDS( "../ExampleEastoutput/Mean.Rds" )
StdDev <- readRDS( "../ExampleEastoutput/StdDev.Rds" )
UserParam <- readRDS( "../ExampleEastoutput/UserParam.Rds" )

strArrivalPath <- "../ExampleEastoutput/ArrivalTime.Rds"
if ( file.exists( strArrivalPath ) ) {
    ArrivalTime <- readRDS( strArrivalPath )
} else {
    ArrivalTime <- rep( 0, NumSub )
}

# Step 2 - Call the generator and calculate summaries for each treatment arm.
lResult <- SimulatePatientOutcomePercentAtZero( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam )
dMeanTrt0 <- mean( lResult$Response[ TreatmentID == 0 ] )
dProb0Trt0 <- mean( lResult$Response[ TreatmentID == 0 ] == 0 )
dMeanTrt1 <- mean( lResult$Response[ TreatmentID == 1 ] )
dProb0Trt1 <- mean( lResult$Response[ TreatmentID == 1 ] == 0 )

print( dMeanTrt0 )
print( dProb0Trt0 )
print( dMeanTrt1 )
print( dProb0Trt1 )

# Step 3 - Inspect the response distributions before running extensive simulations.
graphics::hist( lResult$Response[ TreatmentID == 0 ], main = "Control" )
graphics::hist( lResult$Response[ TreatmentID == 1 ], main = "Experimental" )
