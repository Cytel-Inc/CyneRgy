library( ggplot2 )
library( readxl )
library( readr )
library( dplyr )

######################################################################################################################## .
# Example 2 Post Process                                                                                               ####
# This file provides functions and R Code example to post process the East output for useful graphs and numeric values ####
# 
######################################################################################################################## .

dfEastResults <- read_csv("EastOutput/Example2/Example2.csv")

# Select just the last analysis so we have 1 row per simulated trial
dfResult <- dfEastResults %>%
    group_by(SimIndex) %>%
    slice_max(LookIndex) %>%
    ungroup()

# Probability of end-of-study Go
dfProbEndOfStudyGo <- mean( ifelse( dfResult$BdryStopCode==2 & dfResult$LookIndex == 2, 1, 0) )   

# Probability of end of study stop
dProbOfEndOfStudyStop <- mean( ifelse( dfResult$BdryStopCode==3 & dfResult$LookIndex ==2, 1, 0) )

# Probability of interim futility
dProbOfFutilityAtInterim <- mean( ifelse( dfResult$BdryStopCode==3 & dfResult$LookIndex ==1, 1, 0) )




dfSuccess <- dfResult[ dfResult$BdryStopCode==2, ]  # Get just the successful trial

# Probability of Go Conditional on not stopping for futility IA
dfEndOfStudy <- dfResult[ dfResult$LookIndex ==2, ]
dProbOfGoCondOnNotStopping <- mean( ifelse( dfEndOfStudy$BdryStopCode==2,1,0) )

# Probability of Stop Conditional on not stopping for futility at IA
dProbOfNoGoCondOnNotStopping <-mean( ifelse( dfEndOfStudy$BdryStopCode==3,1,0) )



dfSuccess <- dfEastResults[ dfEastResults$BdryStopCode==2 & dfEastResults$LookIndex ==2, ]

# Mean of true delta given a Go 
mean( dfSuccess$dTrueDelta )

# Summary of true delta given a Go
lSum <- summary( dfSuccess$dTrueDelta )
quantile( dfSuccess$dTrueDelta, probs = c( 0.025, 0.05,0.1, 0.25, 0.5 ,0.75, 0.9, 0.95, 0.975))

dfDen <- density( dfSuccess$dTrueDelta)

# Scale the density to have a max of 1
dfDen2 <- data.frame( x = dfDen$x, y =dfDen$y, y2= dfDen$y/max(dfDen$y))
ggplot( dfDen2, aes( x = x, y = y2 )) +
    geom_line( color="red")
