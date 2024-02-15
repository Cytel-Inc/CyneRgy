library( ggplot2 )
library( readxl )
library( readr )
library( dplyr )

######################################################################################################################## .
# Example 1 Post Process                                                                                               ####
# This file provides functions and R Code example to post process the East output for useful graphs and numeric values ####
# 
######################################################################################################################## .

# Sample the prior and plot the PDF
vMean1 <- rnorm(250000, mean = 0,   sd = 0.05)
vMean2 <- rnorm(750000, mean = 0.7, sd = 0.3)


vMean    <- c( vMean1, vMean2 )
dfSample <- data.frame( SampleMean = vMean )


# Assuming dfSample is your data frame
# Adjusted ggplot code
ggplot(dfSample, aes(SampleMean)) +
    geom_density(fill = "skyblue", color = "black") +
    labs(title = "Prior Distribution of True Treatment Effect (Delta)", x =expression( "True Treatment Effect" * mu_E - mu_C  ), y ="Density") +
    theme_bw() +
    theme(
        plot.title = element_text(hjust = 0.5),  # Center the title
        axis.text.x = element_text(size = 10),    # Adjust x-axis text size
        axis.title.x = element_text(size = 12),   # Adjust x-axis title size
        axis.title.y = element_text(size = 12),   # Adjust y-axis title size
        plot.margin = margin(20, 20, 20, 20),      # Adjust plot margins
        #panel.grid.major = element_blank(),        # Remove major grid lines
        #panel.grid.minor = element_blank(),        # Remove minor grid lines
        panel.border = element_blank(),            # Remove panel border
        axis.line = element_line(color = "black")  # Add black axis line
    ) +
    xlim(-0.25, 1.75)  # Set x-axis range from 0 to 1



# Example 1 - Process the East results ####

EastResults <- read_csv("EastOutput/Example1/Example1.csv")

# Compute the assurance - Probability of a Go 
mean( ifelse( EastResults$BdryStopCode==2,1,0) )

# Probability of a stop
mean( ifelse( EastResults$BdryStopCode==3,1,0) )

# Get trials that are successful so we can build posterior of true delta when a Go decision is made
dfSuccess <- EastResults[ EastResults$BdryStopCode==2, ]
mean( dfSuccess$dTrueDelta )

summary( dfSuccess$dTrueDelta )
quantile( dfSuccess$dTrueDelta, probs = c( 0.025, 0.05,0.1, 0.25, 0.5 ,0.75, 0.9, 0.95, 0.975))

dfDen <- density( dfSuccess$dTrueDelta)

dfDen2 <- data.frame( x = dfDen$x, y =dfDen$y, y2= dfDen$y/max(dfDen$y))
ggplot( dfDen2, aes( x = x, y = y2 )) +
    geom_line( color="red")


# Example 2 ####

EastResults <- read_csv("EastResultsWithPredProbFutility.csv")

# Select just the last analysis so we have 1 row per simulated trial
dfResult <- EastResults %>%
    group_by(SimIndex) %>%
    slice_max(LookIndex) %>%
    ungroup()

# Probability of end-of-study Go
mean( ifelse( dfResult$BdryStopCode==2 & dfResult$LookIndex ==2,1,0) )   

# Probability of end of study stop
mean( ifelse( dfResult$BdryStopCode==3 & dfResult$LookIndex ==2,1,0) )

# Probability of interim futility
mean( ifelse( dfResult$BdryStopCode==3 & dfResult$LookIndex ==1,1,0) )




dfSuccess <- dfResult[ dfResult$BdryStopCode==2, ]  # Get just the successful trial

# Probability of Go Conditional on not stopping for futility IA
dfEndOfStudy <- dfResult[ dfResult$LookIndex ==2, ]
mean( ifelse( dfEndOfStudy$BdryStopCode==2,1,0) )

# Probability of Stop Conditional on not stopping for futility at IA
mean( ifelse( dfEndOfStudy$BdryStopCode==3,1,0) )


dfSuccess <- EastResults[ EastResults$BdryStopCode==2 & EastResults$LookIndex ==2, ]

# Mean of true delta given a Go 
mean( dfSuccess$dTrueDelta )

# Summary of true delta given a Go
summary( dfSuccess$dTrueDelta )
quantile( dfSuccess$dTrueDelta, probs = c( 0.025, 0.05,0.1, 0.25, 0.5 ,0.75, 0.9, 0.95, 0.975))

dfDen <- density( dfSuccess$dTrueDelta)

# Scale the density to have a max of 1
dfDen2 <- data.frame( x = dfDen$x, y =dfDen$y, y2= dfDen$y/max(dfDen$y))
ggplot( dfDen2, aes( x = x, y = y2 )) +
    geom_line( color="red")





# Example 3 ####
vPriorPart1 <- rnorm( 250000, 0, 0.02)
vPriorPart2 <- rbeta( 750000, 2, 2 )
vPriorPart2 <- 0.4* (vPriorPart2)-0.4

vPrior <- sample( c(vPriorPart1, vPriorPart2), 20000 )

write.csv( t(vPrior), "PriorForSolara.csv", row.names = FALSE, col.names = FALSE )

cat( vPrior, file = "PriorForSolara.csv", sep =",\n")

plot( density( exp( vPrior )))
#vPriorPart2 <- pmax(pmin( vPriorPart2, 0), -0.4)

summary( vPriorPart2)

plot( density( vPriorPart2))

plot( density( c( vPriorPart1, vPriorPart2)))
length( vPriorPart2[ is.na(vPriorPart2)])


library(dplyr)
library( ggplot2 )

EastResults <- read_csv("EastResultsTTEAssurance.csv")

Patient <- read_csv("Patient.csv")

Patient <- Patient[ Patient$SubjectID == 1, ]

saveRDS( Patient, "Patient.rds")
EastResults <- dplyr::left_join( EastResults, Patient, by =join_by( "SimIndex" == "SimulationID") )

mean( EastResults$BdryStopCode )  # Pr( Go )

# Get trials that are successful so we can build posterior of true delta when a Go decision is made
dfSuccess <- EastResults[ EastResults$BdryStopCode==1, ]
mean( dfSuccess$TrueHR )

summary( dfSuccess$TrueHR )
quantile( dfSuccess$TrueHR, probs = c( 0.025, 0.05,0.1, 0.25, 0.5 ,0.75, 0.9, 0.95, 0.975))

dfDen <- density( log( dfSuccess$TrueHR) )

dfDen2 <- data.frame( x = dfDen$x, y =dfDen$y, y2= dfDen$y/max(dfDen$y))
ggplot( dfDen2, aes( x = x, y = y2 )) +
    geom_line( color="red")


# Example 4.1 ####


EastResults <- read_csv("Example4Ph2.1.csv")

# Compute the assurance - Probability of a Go 
mean( ifelse( EastResults$BdryStopCode==2,1,0) )

# Probability of a stop
mean( ifelse( EastResults$BdryStopCode==3,1,0) )

# Get trials that are successful so we can build posterior of true delta when a Go decision is made
dfSuccess <- EastResults[ EastResults$BdryStopCode==2, ]
mean( dfSuccess$dTrueDelta )

summary( dfSuccess$dTrueDelta )
quantile( dfSuccess$dTrueDelta, probs = c( 0.025, 0.05,0.1, 0.25, 0.5 ,0.75, 0.9, 0.95, 0.975))

vCDF <- seq( 0.001, 0.999, 0.0001)
vPh2Post <- quantile( dfSuccess$dTrueDelta, probs = vCDF)

mPost <- cbind( vCDF, as.vector( vPh2Post ) )
write.table( mPost, "Ex4Ph2Post.csv", row.names = FALSE, col.names = FALSE, sep= ","   )

vTrueDeltaGivenSuccess <- dfSuccess$dTrueDelta
write.table( vTrueDeltaGivenSuccess, "Ph2TrueDeltaGivenSuccess.csv",row.names = FALSE, col.names = FALSE, sep= "," )

class(vTrueDelta)
cbind( vCDF, vTrueDetla )

dfDen <- density( dfSuccess$dTrueDelta)
dfDen2 <- data.frame( x = dfDen$x, y =dfDen$y, y2= dfDen$y/max(dfDen$y))
ggplot( dfDen2, aes( x = x, y = y2 )) +
    geom_line( color="red") + 
    ggtitle( "Ph2: Posterior Distribution of True Delta Given Success")



# Sample the Ph2 Post using the mPost 

mPost <- read.table( "Ex4Ph2Post.csv",sep = ",")
vUnif <- runif( 10000, 0, 1 )
indices <- findInterval(vUnif, mPost[, 1], rightmost.closed = TRUE)
vPostSamples <- mPost[indices, 2]
summary( vPostSamples)

dfDen <- density( vPostSamples)
dfDen2 <- data.frame( x = dfDen$x, y =dfDen$y, y2= dfDen$y/max(dfDen$y))
ggplot( dfDen2, aes( x = x, y = y2 )) +
    geom_line( color="red") + 
    ggtitle( "Ph2: Posterior Distribution of True Delta Given Success")





mPost <- matrix(c(0.1, 0.3, 0.3, 0.6, 0.6, 0.9), ncol = 2, byrow = TRUE)

# Example vector
vUnif <- c(0.25, 0.55, 0.8)

# Find the indices where vUnif falls in the intervals
indices <- findInterval(vUnif, mPost[, 1], rightmost.closed = TRUE)

# Get the corresponding values from the second column
result <- mPost[indices, 2]



# Example 5 ####

EastResults <- read_csv("Example4Ph2.1.csv")

# Compute the assurance - Probability of a Go 
mean( ifelse( EastResults$BdryStopCode==2,1,0) )

# Probability of a stop
mean( ifelse( EastResults$BdryStopCode==3,1,0) )

# Get trials that are successful so we can build posterior of true delta when a Go decision is made
dfSuccess <- EastResults[ EastResults$BdryStopCode==2, ]
mean( dfSuccess$dTrueDelta )
nrow(dfSuccess)

vLogHR <- 0.1 - 0.4*dfSuccess$dTrueDelta

write.table( vLogHR, "TrueHRPriorForSolaraEx5.csv",row.names = FALSE, col.names = FALSE, sep= "," )
