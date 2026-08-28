######################################################################################################################## .
#' @name GenerateTreatmentControlCSV
#' @title Generate Sample CSV File with Treatment and Control Patient Data
#' @description
#' This helper function creates a CSV file with simulated patient data across multiple visits. It is designed to help users
#' test the CSV-reading functions (GeneratePatientFromCSVGeneral.R and GeneratePatientFromCSVSpecific>R) by
#' generating properly formatted sample data. The function simulates treatment and control groups with normally distributed
#' responses and an optional treatment effect that increases across visits.
#'
#' @author Anton Sun, Jacob Wathen, Gabriel Potvin
#' @param nSubjects The number of subjects to simulate. Default is 100000.
#' @param strFileName The name of the output CSV file. Default is "SimPatientDataNull.csv".
#' @param dTreatmentEffect The magnitude of the treatment effect. The effect is applied from Visit 2 onwards and
#'        increases linearly with each subsequent visit. Set to 0 for null hypothesis simulation (no treatment effect).
#'        Default is 2.
#'
#' @details
#' The function performs the following steps:
#'
#' 1. Sets a random seed (123) for reproducibility
#' 2. Randomly assigns subjects to treatment (1) or control (0) groups
#' 3. Simulates responses for 5 visits using normal distributions with mean = 10 and standard deviation = 3
#' 4. For treatment subjects, applies a treatment effect starting from Visit 2: effect = -dTreatmentEffect * (visit - 1)
#' 5. Creates a data frame with columns: Treatment, Visit 1, Visit 2, Visit 3, Visit 4, Visit 5
#' 6. Writes the data to a CSV file
#'
#' @return A data frame containing the simulated patient data (also saved to CSV file).
#'
#' @examples
#' \dontrun{
#' # Generate a null hypothesis dataset (no treatment effect)
#' dfNull <- GenerateTreatmentControlCSV( nSubjects = 500000,
#'                                        strFileName = "SimPatientDataNull.csv",
#'                                        dTreatmentEffect = 0 )
#'
#' # Generate an alternative hypothesis dataset (with treatment effect)
#' dfAlt <- GenerateTreatmentControlCSV( nSubjects = 500000,
#'                                       strFileName = "SimPatientDataAlt.csv",
#'                                       dTreatmentEffect = 2 )
#' }
#'
######################################################################################################################## .

GenerateTreatmentControlCSV <- function( nSubjects = 100000, strFileName = "SimPatientDataNull.csv", dTreatmentEffect = 2 )
{

    set.seed( 123 )  # Reproducibility

    # Create treatment assignment: 0 = control, 1 = experimental
    vTreatment <- sample( 0:1, nSubjects, replace = TRUE )

    # Simulate effects for 5 visits (normally distributed with some treatment effect)
    mVisits <- matrix( nrow = nSubjects, ncol = 5 )

    for( iVisit in 1:5 )
    {

        # Baseline mean difference grows with visit
        vMean <- 10 - dTreatmentEffect * vTreatment * ( iVisit > 1 ) * ( iVisit - 1 )

        mVisits[ , iVisit ] <- rnorm( nSubjects, mean = vMean, sd = 3 )

    }

    # Build data frame with appropriate column names
    vVisitNames <- paste0( "Visit ", 1:5 )

    dfData <- data.frame( Treatment = vTreatment, mVisits )

    colnames( dfData ) <- c( "Treatment", vVisitNames )

    # Write to CSV
    write.csv( dfData, paste0( "", strFileName ), row.names = FALSE )

    return( dfData )

}

# Example usage:
# Uncomment the lines below to generate sample CSV files

# dfOutput <- GenerateTreatmentControlCSV( nSubjects = 500000, strFileName = "SimPatientDataAlt.csv", dTreatmentEffect = 2 )
# mean( dfOutput[ dfOutput$Treatment == 1, 6 ] )

# dfOutput <- GenerateTreatmentControlCSV( nSubjects = 500000, strFileName = "SimPatientDataNull.csv", dTreatmentEffect = 0 )
