# GenerateTreatmentControlCSV.R

# Creates a dataset of treatment vs control effects across 5 visits

# and saves it to a CSV file.

GenerateTreatmentControlCSV <- function( nSubjects = 100000, strFileName = "SimPatientDataNull.csv", dTreatmentEffect = 2  ) {
    
    set.seed( 123 )  # reproducibility
    
    # create treatment assignment: 0 = control, 1 = experimental
    
    vTreatment <- sample( 0:1, nSubjects, replace = TRUE )
    
    # simulate effects for 5 visits (normally distributed with some treatment effect)
    
    mVisits <- matrix( nrow = nSubjects, ncol = 5 )
    
    for( iVisit in 1:5 ) {
        
        # baseline mean difference grows with visit
        
        vMean <- 10 - dTreatmentEffect * vTreatment * (iVisit > 1)  * (iVisit-1)
        
        mVisits[ , iVisit ] <- rnorm( nSubjects, mean = vMean, sd = 3 )
        
    }
    
    # build data frame with appropriate column names
    
    vVisitNames <- paste0( "Visit ", 1:5 )
    
    dfData <- data.frame( Treatment = vTreatment, mVisits )
    
    colnames( dfData ) <- c( "Treatment", vVisitNames )
    
    # write to CSV
    
    write.csv( dfData, paste0( "", strFileName ) , row.names = FALSE )
    
    return( dfData )
    
}

# Example usage:

dfOutput <- GenerateTreatmentControlCSV( nSubjects = 500000, strFileName = "SimPatientDataAlt.csv", dTreatmentEffect = 2 )

mean(dfOutput[dfOutput$Treatment ==1,6])


dfOutput <- GenerateTreatmentControlCSV( nSubjects = 500000, strFileName = "SimPatientDataNull.csv", dTreatmentEffect = 0 )

#print( head( dfOutput ) )
