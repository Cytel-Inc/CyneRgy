# ————————————————————————————————————————————————————————————— 
# Define the Analysis function
# ————————————————————————————————————————————————————————————— 

MMRMAnalysis <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL) {
    
    # —————————————————————————————————————————————————————————————
    # load required packages
    # —————————————————————————————————————————————————————————————
    
    library(nlme)   
    library(rpact)   
    library(dplyr) 
    library(tidyr)  
    library( CyneRgy )
    # —————————————————————————————————————————————————————————————
    #— initialize outputs
    # —————————————————————————————————————————————————————————————
    nError     <- 0
    nDecision  <- 0
    dPrimDelta <- 0
    dSecDelta  <- 0
    
    # —————————————————————————————————————————————————————————————
    # Step 1: Setup looks
    # —————————————————————————————————————————————————————————————
    if (!is.null(LookInfo)) {
        nQtyOfLooks          <- LookInfo$NumLooks
        nLookIndex           <- LookInfo$CurrLookIndex
        nQtyOfPatsForInterim <- LookInfo$CumCompleters[nLookIndex]
        nAnalysisVisit       <- LookInfo$InterimVisit
    } else {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsForInterim <- nrow(SimData)
    }
    
    # —————————————————————————————————————————————————————————————
    # Step 2: Create Analysis Dataset
    # —————————————————————————————————————————————————————————————
    dfNoBaselineAnalysisData <- CreateAnalysisDataset( SimData, LookInfo )
    
    
    # —————————————————————————————————————————————————————————————
    # Step 5: Fit the MMRM with default CS + varIdent
    # —————————————————————————————————————————————————————————————
    
    lmeCtrls <- lmeControl(
        opt         = "optim",        # switch to optim()
        optimMethod = "BFGS",         # or "Nelder-Mead"
        optimCtrl   = list(maxit = 500),         # increase max iterations for optim
        nlminb      = list(iter.max = 100, eval.max = 200)  # also bump nlminb just in case
    )
    
    # Make LAST visit the reference level so TreatmentID main effect = effect at last visit
    last_visit <- tail(levels(dfNoBaselineAnalysisData$Visit), 1)
    dfNoBaselineAnalysisData$Visit <- relevel(dfNoBaselineAnalysisData$Visit, ref = last_visit)
    
    
    
    mmrm_model <- tryCatch({
        lme(
            Response ~ Baseline + TreatmentID * Visit ,
            random      = ~ 1 | id,
            correlation = corCompSymm(form = ~ 1 | id),
            weights     = varIdent(form = ~ 1 | Visit),
            data        = dfNoBaselineAnalysisData,
            method      = "REML",
            na.action   = na.omit,
            control     = lmeCtrls
        )
        
    }, error = function(e) {
        #error("MMRM model fitting failed: ", e$message)
        nError <- 0  # Non-fatal error should skip the simulation if it does not work
        NULL
    })
    
    # —————————————————————————————————————————————————————————————
    # Step 5b: Extract the treatment×last‐visit effect
    # —————————————————————————————————————————————————————————————
    
    if (!is.null(mmrm_model)) {
        # Now the treatment effect at the LAST visit is simply the TreatmentID main effect
        ttab <- summary(mmrm_model)$tTable
        trt_row <- grep("^TreatmentID", rownames(ttab), value = TRUE)[1]
        
        if (trt_row %in% rownames(ttab)) {
            dPrimDelta <- ttab[trt_row, "Value"]
            stdErr     <- ttab[trt_row, "Std.Error"]
            df         <- ttab[trt_row, "DF"]
            dpValue    <- max(ttab[trt_row, "p-value"], .Machine$double.eps)
            # cat(sprintf(
            #     "Treatment effect at Visit %s: Estimate=%.3f, SE=%.3f, DF=%.1f, p=%.2e\n",
            #     lv[length(lv)], dPrimDelta, stdErr, df, dpValue
            # ))
        } else {
            warning("Interaction term not found: ", term)
            dpValue <- 1.0
        }
    } else {
        dpValue <- 1.0
        nError <- 0  # Non-fatal error should skip the simulation if it does not work
    }
    
    # —————————————————————————————————————————————————————————————
    # Step 6: Obtain group‐sequential alpha
    # —————————————————————————————————————————————————————————————
    if (!is.null(LookInfo)) {
        gs     <- rpact::getDesignGroupSequential(
            kMax         = nQtyOfLooks,
            alpha        = DesignParam$Alpha,
            sided        = 1,
            typeOfDesign = "OF"
        )
        dAlpha <- gs$alphaSpent[nLookIndex]
    } else {
        dAlpha <- DesignParam$Alpha
    }
    
    # —————————————————————————————————————————————————————————————
    # Step 7: Decision rules
    # —————————————————————————————————————————————————————————————
    
    
    if(dpValue <= dAlpha) 
    {
        if( nLookIndex == nQtyOfLooks )
        {
            # FA Efficacy Condition 
            bIAEfficacyCondition <- FALSE
            bFAEfficacyCondition <- TRUE
        }
        else 
        {
            # IA Efficacy condition
            bIAEfficacyCondition <- TRUE
            bFAEfficacyCondition <- FALSE
        }
        # Efficacy decision
        strDecision <- CyneRgy::GetDecisionString(
            LookInfo = LookInfo,
            nLookIndex = nLookIndex,
            nQtyOfLooks = nQtyOfLooks,
            bIAEfficacyCondition = bIAEfficacyCondition,
            bFAEfficacyCondition = bFAEfficacyCondition )
        nDecision <- CyneRgy::GetDecision(strDecision, DesignParam, LookInfo )
    } 
    else 
    {
        
        strDecision <- CyneRgy::GetDecisionString(
            LookInfo = LookInfo,
            nLookIndex = nLookIndex, 
            nQtyOfLooks = nQtyOfLooks)
        
        nDecision <- CyneRgy::GetDecision(strDecision, DesignParam, LookInfo )
    }
    
    
    # —————————————————————————————————————————————————————————————
    # Final return
    # —————————————————————————————————————————————————————————————
    lRet <- list( Decision  = as.integer(nDecision),
                  PrimDelta = as.double(dPrimDelta),
                  p.value   = as.double(dpValue),
                  ErrorCode = as.integer(nError) )
    return( lRet )
}