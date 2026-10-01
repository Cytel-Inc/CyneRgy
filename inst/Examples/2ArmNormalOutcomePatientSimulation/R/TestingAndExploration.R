######################################################################################################################## .
#' @name TestingAndExploration
#' @title Explore simulated continuous outcomes
#' @description Load the saved example inputs, call SimulatePatientOutcomePercentAtZero, and inspect the
#'   mean response and proportion of zero outcomes in each arm. Plot the generated control and experimental
#'   responses to check the simulation before running larger experiments.
#' @author J. Kyle Wathen
#' @return This demonstration script stores the generated response list in lResult, prints the arm-specific
#'   summaries, and displays two histograms.
#' @details Run this script from the example R directory. The bundled ExampleEastoutput directory supplies
#'   NumSub, TreatmentID, Mean, StdDev, and UserParam as RDS files. If ArrivalTime.Rds is available, load it;
#'   otherwise use zero arrival times.
######################################################################################################################## .

# Step 1 - Load the response generator and saved inputs.
source( "SimulatePatientOutcomePercentAtZero.R" )
nSubjects <- readRDS( "../ExampleEastoutput/NumSub.Rds" )
vTreatmentID <- readRDS( "../ExampleEastoutput/TreatmentID.Rds" )
vMean <- readRDS( "../ExampleEastoutput/Mean.Rds" )
vStdDev <- readRDS( "../ExampleEastoutput/StdDev.Rds" )
lUserParam <- readRDS( "../ExampleEastoutput/UserParam.Rds" )

strArrivalPath <- "../ExampleEastoutput/ArrivalTime.Rds"
if ( file.exists( strArrivalPath ) ) {
    vArrivalTime <- readRDS( strArrivalPath )
} else {
    vArrivalTime <- rep( 0, nSubjects )
}

# Step 2 - Call the generator and calculate summaries for each treatment arm.
lResult <- SimulatePatientOutcomePercentAtZero( nSubjects, vArrivalTime, vTreatmentID, vMean, vStdDev, lUserParam )
dMeanTrt0 <- mean( lResult$Response[ vTreatmentID == 0 ] )
dProb0Trt0 <- mean( lResult$Response[ vTreatmentID == 0 ] == 0 )
dMeanTrt1 <- mean( lResult$Response[ vTreatmentID == 1 ] )
dProb0Trt1 <- mean( lResult$Response[ vTreatmentID == 1 ] == 0 )

print( dMeanTrt0 )
print( dProb0Trt0 )
print( dMeanTrt1 )
print( dProb0Trt1 )

# Step 3 - Inspect the response distributions before running extensive simulations.
graphics::hist( lResult$Response[ vTreatmentID == 0 ], main = "Control" )
graphics::hist( lResult$Response[ vTreatmentID == 1 ], main = "Experimental" )
