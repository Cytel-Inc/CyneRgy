source("ImprovedGenerateResponseEmaxModel")
source("Analayze.EmaxModel")

# —————————————————————————————————————————————————————————————

# RunSimulationEmax

# —————————————————————————————————————————————————————————————

RunSimulationEmax <- function(nQtyReps = 10) {
    
    # —————————————————————————————————————————————————————————————
    # Define simulation parameters
    # —————————————————————————————————————————————————————————————
    
    nNumSub        <- 20
    nNumVisit      <- 5
    vTreatmentID   <- rep(c(0,1), each = nNumSub/2)  # 0 = Control, 1 = Treatment
    vVisitTime     <- c(0, 1, 2, 4, 8)              # hours or days
    vMeanControl   <- rep(5, nNumVisit)             # placeholder for control mean
    vStdDevControl <- rep(1, nNumVisit)
    vStdDevTrt     <- rep(1.5, nNumVisit)
    UserParam      <- list(E0 = 5, Emax = 20, EC50 = 50, Concentration = 100, Ke = 0.2)
    
    # —————————————————————————————————————————————————————————————
    # Initialize storage
    # —————————————————————————————————————————————————————————————
    
    mResults <- matrix(0, nrow = nQtyReps, ncol = 4)
    colnames(mResults) <- c("Decision", "PrimDelta", "SecDelta", "Error")
    lLoopSimData <- list()  # store simulated datasets if needed
    
    # —————————————————————————————————————————————————————————————
    # Loop over simulations
    # —————————————————————————————————————————————————————————————
    
    for(iRep in 1:nQtyReps){
        
        # 1) Generate Emax responses
        lGeneratedData <- GenerateResponseEmaxModel(
            NumSub        = nNumSub,
            NumVisit      = nNumVisit,
            TreatmentID   = vTreatmentID,
            Inputmethod   = 0,
            VisitTime     = vVisitTime,
            MeanControl   = vMeanControl,
            StdDevControl = vStdDevControl,
            StdDevTrt     = vStdDevTrt,
            CorrMat       = diag(0.5, nNumVisit),  # simple default correlation
            UserParam     = UserParam
            
        )
        
        # 2) Convert to dataframe
        SimData <- data.frame(TreatmentID = vTreatmentID)
        
        for(j in 1:nNumVisit){
          SimData[[paste0("Response", j)]] <- lGeneratedData[[paste0("Response", j)]]
        }
        
        lLoopSimData[[iRep]] <- SimData
        
        # 3) Define Design Parameters
        DesignParam <- list(
            SampleSize = nNumSub,
            Alpha      = 0.05,
            NumVisit   = nNumVisit,
            TailType   = 0
        )
        
        # 4) Run Analysis
        lAnalysis <- Analyze.EmaxModel(SimData, DesignParam, LookInfo = NULL, UserParam = UserParam)
        
        # 5) Store results
        mResults[iRep, "Decision"]  <- lAnalysis$Decision
        mResults[iRep, "PrimDelta"] <- lAnalysis$PrimDelta
        mResults[iRep, "SecDelta"]  <- lAnalysis$SecDelta
        mResults[iRep, "Error"]     <- lAnalysis$ErrorCode
    }
    
    # —————————————————————————————————————————————————————————————
    # Return results
    # —————————————————————————————————————————————————————————————
    
    return(list(
        Results = mResults,
        SimDataList = lLoopSimData
    ))
}

simOut <- RunSimulationEmax(nQtyReps = 5)

