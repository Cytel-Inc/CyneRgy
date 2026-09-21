######################################################################################################################## .
#' @name ChildPsychologyExample
#' @title Run the Childhood Anxiety follow-up simulation example
#' @description Source the CHU-9 follow-up response generator, simulate one data set, print range and arm-summary
#' checks, and display a faceted histogram. Sourcing this script creates example objects in the global environment.
#' @author Audrey Wathen, J. Kyle Wathen
#' @return No return value. The script creates example data objects, prints diagnostics, and displays a ggplot.
######################################################################################################################## .
source( "R/SimulatePatientOutcomeCHU9.R" )

# Call the function with some parameter values
NumSub      <- 100
ArrivalTime <- sort( runif( NumSub, min = 0, max = 10 ) )
TreatmentID <- sample( 0:1, NumSub, replace = TRUE )
Mean        <- c( 25, 25 )
StdDev      <- c( 5, 5 )

UserParam   <- list( dMeanFollowUpCtrl   = 25,
                     dMeanFollowUpExp    = 15,
                     dStdDevFollowUpCtrl = 5,
                     dStdDevFollowUpExp  = 5 )

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
gPlot <- ggplot2::ggplot( dfPatientData, ggplot2::aes( x = PatientOutcome, fill = factor( TreatmentID ) ) ) +
    ggplot2::geom_histogram( binwidth = 1, alpha = 0.5, position = "identity" ) +
    ggplot2::labs( x = "Patient Outcome", y = "Count", fill = "Treatment ID" ) +
    ggplot2::theme_bw( ) +
    ggplot2::facet_grid( ~factor( TreatmentID ), scales = "free_y" ) +
    ggplot2::ggtitle( "Histogram of Patient Outcomes by Treatment" )

print( gPlot )
