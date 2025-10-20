# ====================================================================================================
# Script Name: RunSimulation.R
# Description: 
#   This script allows you to run the entire simulation workflow directly in RStudio, 
#   outside of the East Horizon (EH) environment. It sources all necessary component scripts 
#   for data generation, model analysis, and visualization. 
#
#   Once executed, the script will:
#     - Generate simulated trial and patient-level data
#     - Perform model-based analyses using MMRM approaches
#     - Create analysis datasets
#     - Produce visualizations of the data
#
#   This setup is useful for debugging, local testing, or further extending 
#   simulation and plotting functionality outside of EH.
# ====================================================================================================

# —————————————————————————————————————————————————————————————
# Source Necessary Files
# —————————————————————————————————————————————————————————————
source( "PlotTreatmentControlCI.R" )
source( "PlotSelectedPatients.R" )
source( "GenerateMMRMResponses.R" )
source( "AnalyzeUsingMMRM.R" )
source( "AnalyzeUsingMMRMWithGLS.R" )

# —————————————————————————————————————————————————————————————
# Install and Load Required Packages
# —————————————————————————————————————————————————————————————
#install.packages( "MASS" )
#install.packages( "dplyr" )
#install.packages( "tidyr" )
#install.packages( "ggplot2" )
#install.packages( "nlme" ) 
#install.packages( "rpact" )
#install.packages( "RColorBrewer" )
#install.packages( "remotes" )
#remotes::install_github( "Cytel-Inc/CyneRgy@main" )

library( MASS )
library( dplyr )
library( tidyr )
library( ggplot2 )
library( nlme )
library( rpact )
library( RColorBrewer )
library( remotes )
library( CyneRgy )

# —————————————————————————————————————————————————————————————
# Run Single Simulation
# —————————————————————————————————————————————————————————————

# Step 1: Prepare Parameters
nNumSub        <- 266
nNumVisit      <- 5
vTreatmentID   <- sample( c( rep( 0, nNumSub / 2 ), rep( 1, nNumSub / 2 ) ) )
nInputmethod   <- 0
vVisitTime     <- c( 0, 1, 2, 3, 4 ) 
vMeanControl   <- c( 90.1, 85.9, 82.6, 81.3, 79.8 )
vMeanTrt       <- c( 90.1, 82.2, 79.5, 77.3, 74 ) 
vStdDevControl <- rep( 15, nNumVisit )
vStdDevTrt     <- rep( 15, nNumVisit )
mCorrMat       <- matrix( 0.5, nrow = nNumVisit, ncol = nNumVisit ) + diag( 0.5, nNumVisit )
lUserParamDataGen <- NULL

vPlotPatients  <- c( 1:5 ) 

# Step 2: Generate Enrollment Data  
vArrivalTime <- sort( runif( nNumSub, 0, 36 ) ) 

# Step 3: Generate Response Data 
lGeneratedData <- GenerateMMRMResponses( NumSub        = nNumSub, 
                                         NumVisit      = nNumVisit, 
                                         TreatmentID   = vTreatmentID, 
                                         Inputmethod   = nInputmethod, 
                                         VisitTime     = vVisitTime,
                                         MeanControl   = vMeanControl, 
                                         MeanTrt       = vMeanTrt, 
                                         StdDevControl = vStdDevControl, 
                                         StdDevTrt     = vStdDevTrt, 
                                         CorrMat       = mCorrMat,
                                         UserParam     = lUserParamDataGen )

# Step 4: Prepare Data for Analysis
# Data generation function returns responses in the form of a list. However, the analysis functions 
# require a dataframe which contains the Responses, as well as Arrival Times and Treatment IDs.
SimData <- data.frame( ArrivalTime   = vArrivalTime,
                       TreatmentID   = vTreatmentID,
                       Response1     = lGeneratedData$Response1,
                       Response2     = lGeneratedData$Response2,
                       Response3     = lGeneratedData$Response3,
                       Response4     = lGeneratedData$Response4,
                       Response5     = lGeneratedData$Response5,
                       ArrTimeVisit1 = rep( vVisitTime[ 1 ], nNumSub ),
                       ArrTimeVisit2 = rep( vVisitTime[ 2 ], nNumSub ),
                       ArrTimeVisit3 = rep( vVisitTime[ 3 ], nNumSub ),
                       ArrTimeVisit4 = rep( vVisitTime[ 4 ], nNumSub ),
                       ArrTimeVisit5 = rep( vVisitTime[ 5 ], nNumSub ) )

DesignParam <- list( SampleSize = nNumSub, 
                     Alpha      = 0.05, 
                     NumVisit   = length( vVisitTime ), 
                     TailType   = 0 )

LookInfo <- list( NumLooks        = 2, 
                  CurrLookIndex   = 1, 
                  CumCompleters   = c( nNumSub / 2, nNumSub ), 
                  InterimVisit    = 2, 
                  IncludePipeline = 0, 
                  RejType         = 2 )

# Step 5: Run Analysis
lAnalysis        <- AnalyzeUsingMMRM( SimData, DesignParam, LookInfo, UserParam = NULL )
lAnalysisGLS     <- AnalyzeUsingMMRMWithGLS( SimData, DesignParam, LookInfo, UserParam = NULL )

# Step 6: Plot both Control and Treatment
TrialPlot <- PlotTreatmentControlCI( SimData )

# Step 7: Plot Individual Patient Trajectories (subset of patients)
PatientPlot <- PlotSelectedPatients( SimData, vPatientIDs = vPlotPatients ) 

# —————————————————————————————————————————————————————————————
# Run Multiple Simulations
# —————————————————————————————————————————————————————————————

# Step 1: Setup the number of iterations
nQtyReps <- 10

# Step 2: Define Objects to store results
mResultsIA <- matrix( 0, nrow = nQtyReps, ncol = 4 )
colnames(mResultsIA) <- c( "Decision", "Prime Delta", "P-Value", "Error" )

mResultsIAGLS     <- mResultsIA

mResultsFA <- matrix(0, nrow = nQtyReps, ncol = 4)
colnames(mResultsFA) <- c( "Decision", "Prime Delta", "P-Value", "Error" )

mResultsFAGLS     <- mResultsFA

lLoopSimData      <- list( )
lLoopTrialPlots   <- list( )
lLoopPlotPatients <- list( )

# Step 3: Run simulations in a loop
dStartTime <- Sys.time( )

for( iRep in 1:nQtyReps ){
    
    vArrivalTime <- sort( runif( nNumSub, 0, 36 ) )
    
    # if the Treatment assignment should be different for each simulation
    # vTreatmentID <- sample( c( rep( 0, nNumSub / 2 ), rep( 1, nNumSub / 2 ) ) )
    
    lGeneratedData <- GenerateMMRMResponses( nNumSub, nNumVisit, vTreatmentID, nInputmethod, vVisitTime,
                                             vMeanControl, vMeanTrt, vStdDevControl, vStdDevTrt, mCorrMat,
                                             lUserParamDataGen )
    
    SimData <- data.frame( ArrivalTime = vArrivalTime,
                           TreatmentID = vTreatmentID,
                           Response1 = lGeneratedData$Response1,
                           Response2 = lGeneratedData$Response2,
                           Response3 = lGeneratedData$Response3,
                           Response4 = lGeneratedData$Response4,
                           Response5 = lGeneratedData$Response5,
                           ArrTimeVisit1 = rep( vVisitTime[ 1 ], nNumSub ),
                           ArrTimeVisit2 = rep( vVisitTime[ 2 ], nNumSub ),
                           ArrTimeVisit3 = rep( vVisitTime[ 3 ], nNumSub ),
                           ArrTimeVisit4 = rep( vVisitTime[ 4 ], nNumSub ),
                           ArrTimeVisit5 = rep( vVisitTime[ 5 ], nNumSub ) )
    
    lLoopSimData[[ iRep ]] <- SimData
    
    DesignParam <- list( SampleSize = nNumSub, Alpha = 0.025, NumVisit = length( vVisitTime ), TailType =0 )
    LookInfoIA  <- list( NumLooks = 2, CurrLookIndex = 1, CumCompleters = c( nNumSub / 2, nNumSub ), 
                         InterimVisit = 2, IncludePipeline = 0, RejType=2 )
    LookInfoFA  <- list( NumLooks = 2, CurrLookIndex = 2, CumCompleters = c( nNumSub / 2, nNumSub ), 
                         InterimVisit = 2, IncludePipeline = 0, RejType=2 )
    
    # Analysis for IA  using 2 methods
    lAnalysisIA <- AnalyzeUsingMMRM( SimData, DesignParam, LookInfoIA, UserParam = NULL )
    mResultsIA[ iRep, 1 ] <- lAnalysisIA$Decision
    mResultsIA[ iRep, 2 ] <- lAnalysisIA$PrimDelta
    mResultsIA[ iRep, 3 ] <- lAnalysisIA$p.value
    mResultsIA[ iRep, 4 ] <- lAnalysisIA$Error
    
    lAnalysisIA <- AnalyzeUsingMMRMWithGLS( SimData, DesignParam, LookInfoIA, UserParam = NULL )
    mResultsIAGLS[ iRep, 1 ] <- lAnalysisIA$Decision
    mResultsIAGLS[ iRep, 2 ] <- lAnalysisIA$PrimDelta
    mResultsIAGLS[ iRep, 3 ] <- lAnalysisIA$p.value
    mResultsIAGLS[ iRep, 4 ] <- lAnalysisIA$Error
    
    # Analysis for FA  using 2 methods
    lAnalysisFA <- AnalyzeUsingMMRM( SimData, DesignParam, LookInfoFA, UserParam = NULL )
    mResultsFA[ iRep, 1 ] <- lAnalysisFA$Decision
    mResultsFA[ iRep, 2 ] <- lAnalysisFA$PrimDelta
    mResultsFA[ iRep, 3 ] <- lAnalysisFA$p.value
    mResultsFA[ iRep, 4 ] <- lAnalysisFA$Error
    
    lAnalysisFA <- AnalyzeUsingMMRMWithGLS( SimData, DesignParam, LookInfoFA, UserParam = NULL )
    mResultsFAGLS[ iRep, 1 ] <- lAnalysisFA$Decision
    mResultsFAGLS[ iRep, 2 ] <- lAnalysisFA$PrimDelta
    mResultsFAGLS[ iRep, 3 ] <- lAnalysisFA$p.value
    mResultsFAGLS[ iRep, 4 ] <- lAnalysisFA$Error
}

dEndTime <- Sys.time()

dSimulationDuration <- dEndTime - dStartTime

# Step 4: Power estimates
# Assessing Decision - proportion of simulations that had a decision to reject the null hypothesis at either look
mean( mResultsFA[ , 1 ] == 1 | mResultsIA[ , 1 ] == 1 ) 
mean( mResultsFAGLS[ , 1 ] == 1 | mResultsIAGLS[ , 1 ] == 1 ) 

# Assessing p-value - proportion of simulations that had a decision to reject the null hypothesis at either look (given alpha = 2.5%)
mean( mResultsFA[ , 3 ] < 0.025 | mResultsIA[ , 3 ] < 0.025 ) 
mean( mResultsFAGLS[ , 3 ] < 0.025 | mResultsIAGLS[ , 3 ] < 0.025 ) 

# Step 5: Examine treatment effect estimates for each analysis
mean( mResultsFA[ , 2 ] )
mean( mResultsFAGLS[ , 2 ] )







