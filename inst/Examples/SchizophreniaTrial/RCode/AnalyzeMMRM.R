
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
    # Step 2: Reshape wide → long in one shot
    # —————————————————————————————————————————————————————————————
    dfLongData <- SimData %>%
        mutate(id = row_number()) %>%
        pivot_longer(
            cols          = matches("^(Response|ArrTimeVisit)\\d+$"),
            names_to      = c(".value", "Visit"),
            names_pattern = "(Response|ArrTimeVisit)(\\d+)"
        ) %>%
        mutate(
            Visit             = as.integer(Visit),
            CalendarVisitTime = ArrivalTime + ArrTimeVisit
        ) %>%
        dplyr::select(id, TreatmentID, Visit, Response, CalendarVisitTime) %>%
        arrange(Visit, CalendarVisitTime)
    
    # —————————————————————————————————————————————————————————————
    # Step 3: Interim‐look filtering using dplyr
    # —————————————————————————————————————————————————————————————
    if (!is.null(LookInfo)) {
        # 3a) compute cutoff time
        dAnalysisTime <- dfLongData %>%
            filter(Visit == nAnalysisVisit) %>%
            slice(nQtyOfPatsForInterim) %>%
            pull(CalendarVisitTime)
        
        # —————————————————————————————————————————————————————————————
        # 3b) pick subjects
        # —————————————————————————————————————————————————————————————
        
        # TODO(Kyle) I think we always want to include the pipleline patients
        
        if (LookInfo$IncludePipeline == 0) {
            vSubjectsForAnalysis <- dfLongData %>%
                filter(
                    Visit == nAnalysisVisit,
                    CalendarVisitTime <= dAnalysisTime
                ) %>%
                distinct(id) %>%
                pull(id)
        } else {
            vSubjectsForAnalysis <- dfLongData %>%
                filter(CalendarVisitTime <= dAnalysisTime) %>%
                distinct(id) %>%
                pull(id)
        }
        
        dfAnalysisData <- dfLongData %>%
            filter(id %in% vSubjectsForAnalysis)
    } else {
        dfAnalysisData <- dfLongData
    }
    
    # —————————————————————————————————————————————————————————————
    # Step 4: Prepare for MMRM
    # —————————————————————————————————————————————————————————————
    dfAnalysisData <- dfAnalysisData %>%
        mutate(
            Visit       = factor(Visit),
            TreatmentID = factor(TreatmentID),
            id          = factor(id)
        )
    
    # Create a dataset that removes the baseline visit (Visit == 1) from the long form and adds the baseline response as a new column
    dfNoBaselineAnalysisData <- dplyr::filter( dfAnalysisData, Visit != 1)
    dfBaselineAnalysisData   <- dplyr::filter( dfAnalysisData, Visit == 1) %>% select( id, Baseline = Response)
    dfNoBaselineAnalysisData <- dplyr::left_join(dfNoBaselineAnalysisData,dfBaselineAnalysisData, by = "id" )
    
    
    # —————————————————————————————————————————————————————————————
    # Step 5: Fit the MMRM with default CS + varIdent
    # —————————————————————————————————————————————————————————————
    
    lmeCtrls <- lmeControl(
        opt         = "optim",        # switch to optim()
        optimMethod = "BFGS",         # or "Nelder-Mead"
        optimCtrl   = list(maxit = 500),         # increase max iterations for optim
        nlminb      = list(iter.max = 100, eval.max = 200)  # also bump nlminb just in case
    )
    
    mmrm_model <- tryCatch({
        lme(
            Response ~ Baseline + TreatmentID * Visit + CalendarVisitTime,
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
        ttab <- summary(mmrm_model)$tTable
        lv   <- levels(dfAnalysisData$Visit)
        term <- paste0("TreatmentID1:Visit", lv[length(lv)])
        
        if (term %in% rownames(ttab)) {
            dpValue    <- max(ttab[term, "p-value"], .Machine$double.eps)
            dPrimDelta <- ttab[term, "Value"]
            stdErr     <- ttab[term, "Std.Error"]
            df         <- ttab[term, "DF"]
            cat(sprintf(
                "Treatment effect at Visit %s: Estimate=%.3f, SE=%.3f, DF=%.1f, p=%.2e\n",
                lv[length(lv)], dPrimDelta, stdErr, df, dpValue
            ))
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
    
    #Remove this line later
    #dAlpha <- 0.025
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
    # if (nDecision == 0L && nLookIndex == nQtyOfLooks) {
    #     
    #     strDecision <- CyneRgy::GetDecisionString(
    #         LookInfo = LookInfo,
    #         nLookIndex = nQtyOfLooks, 
    #         nQtyOfLooks = nQtyOfLooks)
    #     
    #     nDecision <- CyneRgy::GetDecision(strDecision, DesignParam, LookInfo )
    # }
       
    # —————————————————————————————————————————————————————————————
    # Final return
    # —————————————————————————————————————————————————————————————
    list(
        Decision  = as.integer(nDecision),
        PrimDelta = as.double(dPrimDelta),
        SecDelta  = as.double(dSecDelta),
        p.value   = as.double(dpValue),
        ErrorCode = as.integer(nError)
    )
}


