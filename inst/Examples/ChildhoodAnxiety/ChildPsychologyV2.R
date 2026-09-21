######################################################################################################################## .
#' @name ChildPsychologyV2Example
#' @title Run the Childhood Anxiety baseline simulation example
#' @description Source the CHU-9 baseline response generator, simulate one data set, print range and arm-summary
#' checks, and display a faceted histogram. Sourcing this script creates example objects in the global environment.
#' @author Audrey Wathen, J. Kyle Wathen
#' @return No return value. The script creates example data objects, prints diagnostics, and displays a ggplot.
######################################################################################################################## .
source( "R/SimulatePatientOutcomeCHU9V2.R" )

# Call the function with some parameter values
NumSub      <- 100
ArrivalTime <- sort( runif( NumSub, min = 0, max = 10 ) )
TreatmentID <- sample( 0:1, NumSub, replace = TRUE )
Mean        <- c( 0, 10 )
StdDev      <- c( 10.6, 10.6 )

UserParam   <- list( dMeanBaselineCtrl = 25,
                     dMeanBaselineExp  = 25,
                     dStdDevBaselineCtrl = 10.6,
                     dStdDevBaselineExp  = 10.6 )

lRet <- SimulatePatientOutcome( NumSub, ArrivalTime, TreatmentID, Mean, StdDev, UserParam )

dfPatientData <- data.frame( TreatmentID = TreatmentID,
                             PatientOutcome = lRet$Response )

# Test the simulated data ####
# Simulated Data Requirements
# Note: Response is Baseline - Followup so a value above 0 means the patient improved.
# Patient data requirements:
# -36 < Response <= 36
# Given the setup above, control mean = 0, experimental mean = 10

any( lRet$Response > 36 )  # This should be false
any( lRet$Response < -36 ) # This should be false

mean( dfPatientData$PatientOutcome[ dfPatientData$TreatmentID == 0 ] ) # Expected to be 0
mean( dfPatientData$PatientOutcome[ dfPatientData$TreatmentID == 1 ] ) # Expected to close to 10

table( dfPatientData )

# Create a histogram of PatientOutcome by TreatmentID
ggplot2::ggplot( dfPatientData, ggplot2::aes( x = PatientOutcome, fill = factor( TreatmentID ) ) ) +
    ggplot2::geom_histogram( binwidth = 1, alpha = 0.5, position = "identity" ) +
    ggplot2::labs( x = "Patient Outcome", y = "Count", fill = "Treatment ID" ) +
    ggplot2::theme_minimal( ) +
    ggplot2::facet_grid( ~factor( TreatmentID ), scales = "free_y" ) +
    ggplot2::ggtitle( "Histogram of Patient Outcomes by Treatment" )
