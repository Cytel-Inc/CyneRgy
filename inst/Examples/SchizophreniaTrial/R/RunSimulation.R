######################################################################################################################## .
#' @name RunSimulation
#' @title Run Simulation
#' @description Run the repeated-measures trial simulation directly in R. Source the response-generation,
#'   MMRM analysis, and plotting functions, inspect a single simulated trial, and estimate power across
#'   repeated trials using both mixed-model and generalized least-squares analyses.
#' @author Jacob Wathen
#' @return This demonstration script creates simulated subject data, analysis results, plots, and matrices
#'   of interim and final results in the R session.
#' @details Run from the example R directory with MASS, dplyr, tidyr, ggplot2, nlme, rpact, RColorBrewer,
#'   and CyneRgy installed. The final expressions report empirical efficacy rates and treatment effects.
######################################################################################################################## .

source( "PlotTreatmentControlCI.R" )
source( "PlotSelectedPatients.R" )
source( "GenerateMMRMResponses.R" )
source( "AnalyzeUsingMMRM.R" )
source( "AnalyzeUsingMMRMWithGLS.R" )

# —————————————————————————————————————————————————————————————
# Install and Load Required Packages
# —————————————————————————————————————————————————————————————
# install.packages( "MASS" )
# install.packages( "dplyr" )
# install.packages( "tidyr" )
# install.packages( "ggplot2" )
# install.packages( "nlme" )
# install.packages( "rpact" )
# install.packages( "RColorBrewer" )
# install.packages( "remotes" )
# remotes::install_github( "Cytel-Inc/CyneRgy@main" )

# —————————————————————————————————————————————————————————————
# Run Single Simulation
# —————————————————————————————————————————————————————————————

# Step 1: Prepare Parameters
nNumSub <- 266
nNumVisit <- 5
vTreatmentID <- sample( c( rep( 0, nNumSub / 2 ), rep( 1, nNumSub / 2 ) ) )
nInputmethod <- 0
vVisitTime <- c( 0, 1, 2, 3, 4 )
vMeanControl <- c( 90.1, 85.9, 82.6, 81.3, 79.8 )
vMeanTrt <- c( 90.1, 82.2, 79.5, 77.3, 74 )
vStdDevControl <- rep( 15, nNumVisit )
vStdDevTrt <- rep( 15, nNumVisit )
mCorrMat <- matrix( 0.5, nrow = nNumVisit, ncol = nNumVisit ) + diag( 0.5, nNumVisit )
lUserParamDataGen <- NULL

vPlotPatients <- c( 1:5 )

# Step 2: Generate Enrollment Data
vArrivalTime <- sort( stats::runif( nNumSub, 0, 36 ) )

# Step 3: Generate Response Data
lGeneratedData <- GenerateMMRMResponses(
    NumSub = nNumSub,
    NumVisit = nNumVisit,
    ArrivalTime = vArrivalTime,
    TreatmentID = vTreatmentID,
    Inputmethod = nInputmethod,
    VisitTime = vVisitTime,
    MeanControl = vMeanControl,
    MeanTrt = vMeanTrt,
    StdDevControl = vStdDevControl,
    StdDevTrt = vStdDevTrt,
    CorrMat = mCorrMat,
    UserParam = lUserParamDataGen
)

# Step 4: Prepare Data for Analysis
# Data generation function returns responses in the form of a list. However, the analysis functions
# Require a data frame that contains the responses, arrival times, and treatment IDs.
dfSimData <- data.frame(
    ArrivalTime = vArrivalTime,
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
    ArrTimeVisit5 = rep( vVisitTime[ 5 ], nNumSub )
)

lDesignParam <- list(
    SampleSize = nNumSub,
    Alpha = 0.05,
    NumVisit = length( vVisitTime ),
    TailType = 0
)

lLookInfo <- list(
    NumLooks = 2,
    CurrLookIndex = 1,
    CumCompleters = c( nNumSub / 2, nNumSub ),
    InterimVisit = 2,
    IncludePipeline = 0,
    RejType = 2
)

# Step 5: Run Analysis
lAnalysis <- AnalyzeUsingMMRM( dfSimData, lDesignParam, lLookInfo, UserParam = NULL )
lAnalysisGLS <- AnalyzeUsingMMRMWithGLS( dfSimData, lDesignParam, lLookInfo, UserParam = NULL )

# Step 6: Plot both Control and Treatment
cTrialPlot <- PlotTreatmentControlCI( dfSimData )

# Step 7: Plot Individual Patient Trajectories (subset of patients)
cPatientPlot <- PlotSelectedPatients( dfSimData, vPatientIDs = vPlotPatients )

# —————————————————————————————————————————————————————————————
# Run Multiple Simulations
# —————————————————————————————————————————————————————————————

# Step 1: Setup the number of iterations
nQtyReps <- 10

# Step 2: Define Objects to store results
mResultsIA <- matrix( 0, nrow = nQtyReps, ncol = 4 )
colnames( mResultsIA ) <- c( "Decision", "Primary Delta", "P-Value", "Error" )

mResultsIAGLS <- mResultsIA

mResultsFA <- matrix( 0, nrow = nQtyReps, ncol = 4 )
colnames( mResultsFA ) <- c( "Decision", "Primary Delta", "P-Value", "Error" )

mResultsFAGLS <- mResultsFA

lLoopSimData <- list( )
lLoopTrialPlots <- list( )
lLoopPlotPatients <- list( )

# Step 3: Run simulations in a loop
dStartTime <- Sys.time( )

for ( iRep in 1:nQtyReps ) {
    vArrivalTime <- sort( stats::runif( nNumSub, 0, 36 ) )

    # if the Treatment assignment should be different for each simulation
    # vTreatmentID <- sample( c( rep( 0, nNumSub / 2 ), rep( 1, nNumSub / 2 ) ) )

    lGeneratedData <- GenerateMMRMResponses(
        nNumSub, nNumVisit, vArrivalTime, vTreatmentID, nInputmethod, vVisitTime,
        vMeanControl, vMeanTrt, vStdDevControl, vStdDevTrt, mCorrMat,
        lUserParamDataGen
    )

    dfSimData <- data.frame(
        ArrivalTime = vArrivalTime,
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
        ArrTimeVisit5 = rep( vVisitTime[ 5 ], nNumSub )
    )

    lLoopSimData[[ iRep ]] <- dfSimData

    lDesignParam <- list( SampleSize = nNumSub, Alpha = 0.025, NumVisit = length( vVisitTime ), TailType = 0 )
    lLookInfoIA <- list(
        NumLooks = 2, CurrLookIndex = 1, CumCompleters = c( nNumSub / 2, nNumSub ),
        InterimVisit = 2, IncludePipeline = 0, RejType = 2
    )
    lLookInfoFA <- list(
        NumLooks = 2, CurrLookIndex = 2, CumCompleters = c( nNumSub / 2, nNumSub ),
        InterimVisit = 2, IncludePipeline = 0, RejType = 2
    )

    # Analysis for IA  using 2 methods
    lAnalysisIA <- AnalyzeUsingMMRM( dfSimData, lDesignParam, lLookInfoIA, UserParam = NULL )
    mResultsIA[ iRep, 1 ] <- lAnalysisIA$Decision
    mResultsIA[ iRep, 2 ] <- lAnalysisIA$PrimDelta
    mResultsIA[ iRep, 3 ] <- lAnalysisIA$p.value
    mResultsIA[ iRep, 4 ] <- lAnalysisIA$ErrorCode

    lAnalysisIA <- AnalyzeUsingMMRMWithGLS( dfSimData, lDesignParam, lLookInfoIA, UserParam = NULL )
    mResultsIAGLS[ iRep, 1 ] <- lAnalysisIA$Decision
    mResultsIAGLS[ iRep, 2 ] <- lAnalysisIA$PrimDelta
    mResultsIAGLS[ iRep, 3 ] <- lAnalysisIA$p.value
    mResultsIAGLS[ iRep, 4 ] <- lAnalysisIA$ErrorCode

    # Analysis for FA  using 2 methods
    lAnalysisFA <- AnalyzeUsingMMRM( dfSimData, lDesignParam, lLookInfoFA, UserParam = NULL )
    mResultsFA[ iRep, 1 ] <- lAnalysisFA$Decision
    mResultsFA[ iRep, 2 ] <- lAnalysisFA$PrimDelta
    mResultsFA[ iRep, 3 ] <- lAnalysisFA$p.value
    mResultsFA[ iRep, 4 ] <- lAnalysisFA$ErrorCode

    lAnalysisFA <- AnalyzeUsingMMRMWithGLS( dfSimData, lDesignParam, lLookInfoFA, UserParam = NULL )
    mResultsFAGLS[ iRep, 1 ] <- lAnalysisFA$Decision
    mResultsFAGLS[ iRep, 2 ] <- lAnalysisFA$PrimDelta
    mResultsFAGLS[ iRep, 3 ] <- lAnalysisFA$p.value
    mResultsFAGLS[ iRep, 4 ] <- lAnalysisFA$ErrorCode
}

dEndTime <- Sys.time( )

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
