source('PlotTrialData.r')
source('PlotPatientData.r')
source('GenerateMMRMResponses.r')
source('AnalyzeMMRM.r')


# —————————————————————————————————————————————————————————————
# Install required packages if needed
# —————————————————————————————————————————————————————————————

#install.packages('MASS')
#install.packages('dplyr')
#install.packages('tidyr')
#install.packages('ggplot2')
#install.packages("nlme") 
#install.packages("rpact")
#install.packages("RColorBrewer")
#install.packages('remotes')
#remotes::install_github( "Cytel-Inc/CyneRgy@main" )


# —————————————————————————————————————————————————————————————
# Load libraries
# —————————————————————————————————————————————————————————————
library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)
library(rpact)
library(RColorBrewer)
library(remotes)
library(CyneRgy)



# —————————————————————————————————————————————————————————————
# -- Define parameters 
# —————————————————————————————————————————————————————————————
nNumSub        <- 600
nNumVisit      <- 5
vTreatmentID   <- sample( c(rep(0, nNumSub/2), rep(1, nNumSub/2)) )
nInputmethod   <- 0
vVisitTime     <- c(0, 1, 2, 3, 4)   # This means when we generate the data we will have Response1, Response2,... Response5
vMeanControl   <- c(90.1, 85.9, 82.6, 81.3, 79.8)
vMeanTrt       <- c(90.1, 82.2, 79.5, 77.3, 75.6)
vStdDevControl <- rep(9.5, nNumVisit)
vStdDevTrt     <- rep(11.1, nNumVisit)
vPlotPatients <- c(1:5) #Vector to plot select patients

mCorrMat       <- matrix(0.5, nrow = nNumVisit, ncol = nNumVisit) + diag(0.5, nNumVisit)
lUserParamDataGen <- NULL


# —————————————————————————————————————————————————————————————-------------------------------------------------
# Note: The functions that can be used for data generation for East Horizon all return a list. 
#       However, the Analysis functions require a dataframe so after generating the data with your function 
#       we need to make a dataframe to send to the analysis 
# —————————————————————————————————————————————————————————————-------------------------------------------------

lGeneratedData <- GenerateMMRMResponses( nNumSub, nNumVisit, vTreatmentID, nInputmethod, vVisitTime,
                                         vMeanControl, vMeanTrt, vStdDevControl, vStdDevTrt, mCorrMat,
                                         UserParam = lUserParamDataGen)

# —————————————————————————————————————————————————————————————
# Create arrival times in order
# —————————————————————————————————————————————————————————————

vArrivalTime <- sort( runif( nNumSub, 0, 36 ))  # Simulated arrival times for patients

# —————————————————————————————————————————————————————————————-----------------------------------------------
# Convert the list of patient data generated above to the needed dataframe for the call to MMRMAnalysis below
# —————————————————————————————————————————————————————————————-----------------------------------------------

SimData <- data.frame(
    ArrivalTime = vArrivalTime,
    TreatmentID = vTreatmentID,
    Response1 = lGeneratedData$Response1,
    Response2 = lGeneratedData$Response2,
    Response3 = lGeneratedData$Response3,
    Response4 = lGeneratedData$Response4,
    Response5 = lGeneratedData$Response5,
    ArrTimeVisit1 = rep( vVisitTime[ 1 ], nNumSub),
    ArrTimeVisit2 = rep( vVisitTime[ 2 ], nNumSub),
    ArrTimeVisit3 = rep( vVisitTime[ 3 ], nNumSub),
    ArrTimeVisit4 = rep( vVisitTime[ 4 ], nNumSub),
    ArrTimeVisit5 = rep( vVisitTime[ 5 ], nNumSub)
)

DesignParam <- list( SampleSize = nNumSub, Alpha = 0.05, NumVisit = length( vVisitTime ), TailType =0 )

LookInfo <- list( NumLooks = 2, CurrLookIndex = 1, CumCompleters = c(nNumSub/2, nNumSub), InterimVisit = 2, IncludePipeline = 0, RejType=2 )

# —————————————————————————————————————————————————————————————
# Run Analysis
# —————————————————————————————————————————————————————————————

lAnalysis <- MMRMAnalysis(SimData, DesignParam, LookInfo, UserParam = NULL)

# —————————————————————————————————————————————————————————————
# Plot both Control and Treatment
# —————————————————————————————————————————————————————————————
TrialPlot <- PlotTreatmentControlCI(SimData, DesignParam)

# —————————————————————————————————————————————————————————————
# Plot Select Patients
# —————————————————————————————————————————————————————————————
PatientPlot <- PlotSelectedPatients(SimData, DesignParam, vPatientIDs = vPlotPatients) 







# —————————————————————————————————————————————————————————————
# -- How many repetitions in the for loop
# —————————————————————————————————————————————————————————————
nQtyReps = 10


# —————————————————————————————————————————————————————————————
# -- Define Matrix to store results
# —————————————————————————————————————————————————————————————
mResultsIA <- matrix(0, nrow = nQtyReps, ncol = 4)
colnames(mResultsIA) <- c('Decision', 'Prime Delta', 'P-Value', 'Error')

mResultsFA <- matrix(0, nrow = nQtyReps, ncol = 4)
colnames(mResultsFA) <- c('Decision', 'Prime Delta', 'P-Value', 'Error')

# —————————————————————————————————————————————————————————————
# -- Create a list to store Simulated Data Across Simulations
# —————————————————————————————————————————————————————————————
lLoopSimData <- list()


# —————————————————————————————————————————————————————————————
# -- Create a list to store ggplots for Trails and Select Patients
# —————————————————————————————————————————————————————————————
lLoopTrialPlots <- list()
lLoopPlotPatients <- list()




# —————————————————————————————————————————————————————————————
# -- Get the start time before the for loop
# —————————————————————————————————————————————————————————————
dStartTime <- Sys.time()


# —————————————————————————————————————————————————————————————
# -- For loop for running multiple simulations
# —————————————————————————————————————————————————————————————
for(iRep in 1:nQtyReps){
  
    # —————————————————————————————————————————————————————————————-------------------------------------------------
    # Note: The functions that can be used for data generation for East Horizon all return a list. 
    #       However, the Analysis functions require a dataframe, so after generating the data with your function 
    #       we need to make a dataframe to send to the analysis 
    # —————————————————————————————————————————————————————————————-------------------------------------------------
    
    lGeneratedData <- GenerateMMRMResponses( nNumSub, nNumVisit, vTreatmentID, nInputmethod, vVisitTime,
                                             vMeanControl, vMeanTrt, vStdDevControl, vStdDevTrt, mCorrMat,
                                             UserParam = lUserParamDataGen)
    
    # —————————————————————————————————————————————————————————————
    # Create arrival times in order
    # —————————————————————————————————————————————————————————————
    
    vArrivalTime <- sort( runif( nNumSub, 0, 36 ))  # Simulated arrival times for patients
    
    # —————————————————————————————————————————————————————————————-----------------------------------------------
    # Convert the list of patient data generated above to the needed dataframe for the call to MMRMAnalysis below
    # —————————————————————————————————————————————————————————————-----------------------------------------------
    
    SimData <- data.frame(
        ArrivalTime = vArrivalTime,
        TreatmentID = vTreatmentID,
        Response1 = lGeneratedData$Response1,
        Response2 = lGeneratedData$Response2,
        Response3 = lGeneratedData$Response3,
        Response4 = lGeneratedData$Response4,
        Response5 = lGeneratedData$Response5,
        ArrTimeVisit1 = rep( vVisitTime[ 1 ], nNumSub),
        ArrTimeVisit2 = rep( vVisitTime[ 2 ], nNumSub),
        ArrTimeVisit3 = rep( vVisitTime[ 3 ], nNumSub),
        ArrTimeVisit4 = rep( vVisitTime[ 4 ], nNumSub),
        ArrTimeVisit5 = rep( vVisitTime[ 5 ], nNumSub)
    )
    
    lLoopSimData[[iRep]] <- SimData
    
    DesignParam <- list( SampleSize = nNumSub, Alpha = 0.05, NumVisit = length( vVisitTime ), TailType =0 )
    
    LookInfoIA <- list( NumLooks = 2, CurrLookIndex = 1, CumCompleters = c(nNumSub/2, nNumSub), InterimVisit = 2, IncludePipeline = 0, RejType=2 )
    
    LookInfoFA <- list( NumLooks = 2, CurrLookIndex = 2, CumCompleters = c(nNumSub/2, nNumSub), InterimVisit = 2, IncludePipeline = 0, RejType=2 )
    
    # —————————————————————————————————————————————————————————————
    #Run Analysis
    # —————————————————————————————————————————————————————————————
    
    lAnalysisIA <- MMRMAnalysis(SimData, DesignParam, LookInfoIA, UserParam = NULL)
    mResultsIA[iRep, 1] <- lAnalysisIA$Decision
    mResultsIA[iRep, 2] <- lAnalysisIA$PrimDelta
    mResultsIA[iRep, 3] <- lAnalysisIA$p.value
    mResultsIA[iRep, 4] <- lAnalysisIA$Error
    
    lAnalysisFA <- MMRMAnalysis(SimData, DesignParam, LookInfoFA, UserParam = NULL)
    mResultsFA[iRep, 1] <- lAnalysisFA$Decision
    mResultsFA[iRep, 2] <- lAnalysisFA$PrimDelta
    mResultsFA[iRep, 3] <- lAnalysisFA$p.value
    mResultsFA[iRep, 4] <- lAnalysisFA$Error
    
    # —————————————————————————————————————————————————————————————
    # Plot both Control and Treatment and Store all Trial Plots in a List
    # —————————————————————————————————————————————————————————————
    PlotTreatmentControlCI(SimData, DesignParam)
    
    
    lTrialPlots[[iRep]] <- PlotTreatmentControlCI(SimData, DesignParam)
    
    # —————————————————————————————————————————————————————————————
    # Plot Select Patients and Store in a List Automatically
    # —————————————————————————————————————————————————————————————
    
    PlotSelectedPatients(SimData, DesignParam, vPatientIDs = vPlotPatients)
    
    lPlotPatients[[iRep]] <- PlotSelectedPatients(SimData, DesignParam, vPatientIDs = vPlotPatients) 
    
}



# —————————————————————————————————————————————————————————————
# -- Get the completion time of the simulation 
# —————————————————————————————————————————————————————————————
dEndTime <- Sys.time()
dEndTime - dStartTime


# —————————————————————————————————————————————————————————————------------
# -- Command to See individual patient plots from each simulation manually
# —————————————————————————————————————————————————————————————-------------

# PlotSelectedPatients(lLoopSimData[[iRep]], DesignParam, vPatientIDs = vPlotPatients)






