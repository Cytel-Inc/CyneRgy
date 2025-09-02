
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


MMRMAnalysisGLS <- function(SimData, DesignParam, LookInfo = NULL, UserParam = NULL) 
{
    
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
    } 
    else {
        nLookIndex           <- 1
        nQtyOfLooks          <- 1
        nQtyOfPatsForInterim <- nrow(SimData)
    }
    
    
    # —————————————————————————————————————————————————————————————
    # Step 2: Create Analysis Dataset
    # —————————————————————————————————————————————————————————————
    dfNoBaselineAnalysisData <- CreateAnalysisDataset( SimData, LookInfo )
    
    # —————————————————————————————————————————————————————————————
    # Step 3: Fit the MMRM using nlme::gls
    # —————————————————————————————————————————————————————————————
    
    dfNoBaselineAnalysisData$TreatmentID <- as.factor(dfNoBaselineAnalysisData$TreatmentID )
    dfNoBaselineAnalysisData$Visit       <- factor(dfNoBaselineAnalysisData$Visit, levels = c(2,3,4,5) )
    
    # Create the vectors for analysis, using the names needed for the GetLSDiffGLS
    
    vOut      <- dfNoBaselineAnalysisData$Response
    vBaseline <- dfNoBaselineAnalysisData$Baseline
    vTrt      <- dfNoBaselineAnalysisData$TreatmentID
    vTime     <- dfNoBaselineAnalysisData$Visit
    vIND      <- dfNoBaselineAnalysisData$id
    
    
    fit <-   nlme::gls(vOut ~ vBaseline + vTrt * vTime ,
                       weights = varIdent(form = ~1|vTime),
                       correlation = corSymm(form=~1|vIND), na.action = na.omit ) #, silent=TRUE) #corr = corSymm(form = ~as.integer(Time)|IDN)), silent = TRUE )
    
    
    lRetGLS <- GetLSDiffGLS( fit, 1, 5, FALSE)
    
    
    # —————————————————————————————————————————————————————————————
    # Step 4: Obtain group‐sequential alpha
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
    # Step 5: Decision rules
    # —————————————————————————————————————————————————————————————
    if(lRetGLS$dPVal <= dAlpha) 
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
        strDecision <- CyneRgy::GetDecisionString(  LookInfo   = LookInfo,
                                                    nLookIndex  = nLookIndex,
                                                    nQtyOfLooks = nQtyOfLooks,
                                                    bIAEfficacyCondition = bIAEfficacyCondition,
                                                    bFAEfficacyCondition = bFAEfficacyCondition )
        nDecision <- CyneRgy::GetDecision(strDecision, DesignParam, LookInfo )
    } 
    else 
    {
        
        strDecision <- CyneRgy::GetDecisionString( LookInfo = LookInfo,
                                                   nLookIndex = nLookIndex, 
                                                   nQtyOfLooks = nQtyOfLooks )
        
        nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    }
    
    
    # —————————————————————————————————————————————————————————————
    # Step 6: Final return
    # —————————————————————————————————————————————————————————————
    lRet <- list(  Decision  = as.integer(nDecision),
                   PrimDelta = as.double(lRetGLS$dEst),
                   p.value   = as.double(lRetGLS$dPVal),
                   ErrorCode = as.integer(nError) )
    
    #Explicit Return 
    return( lRet )
}


CreateAnalysisDataset <- function( SimData, LookInfo )
{
    
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
        
        # —————————————————————————————————————————————————————————————
        # 3a) compute cutoff time
        # —————————————————————————————————————————————————————————————
        
        dAnalysisTime <- dfLongData %>%
            filter(Visit == nAnalysisVisit) %>%
            slice(nQtyOfPatsForInterim) %>%
            pull(CalendarVisitTime)
        
        # —————————————————————————————————————————————————————————————
        # 3b) pick subjects
        # —————————————————————————————————————————————————————————————
        
        
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
    
    return( dfNoBaselineAnalysisData )
}

GetLSDiffGLS <-  function( glsFit, nTrt, nTime, bPlacMinusTrt )
{
    strWhichTrt     <- paste("vTrt" , nTrt, sep = "")
    strWhichTime    <- paste("vTime", nTime, sep = "")
    strIntercept    <- "(Intercept)"
    vCoeff          <- coef( glsFit )
    if( !any(names(vCoeff ) == strWhichTime ) )  #The time is the "baseline" so no need to include the interaction
    {
        vVarNames       <- strWhichTrt
        vVarNamesPlac   <- strIntercept
        vVarNamesTrt    <- c( strIntercept, strWhichTrt )
    }
    else  #The time is not the baseline so we need to include the trt*time interaction in the estimate
    {
        vVarNames       <- c(strWhichTrt, paste(strWhichTrt, strWhichTime, sep = ":"))
        vVarNamesPlac   <- c( strIntercept, strWhichTime )
        vVarNamesTrt    <- c( vVarNamesPlac,  vVarNames )
    }
    if( any( names( vCoeff) == "vBaseline") )
    {
        vVarNames <- c( vVarNames)
        vVarNamesPlac <- c( vVarNamesPlac, "vBaseline")
        vVarNamesTrt <- c( vVarNamesTrt, "vBaseline")
    }
    nDOF        <- diff(unlist(glsFit$dims)[2:1])
    
    #This gives an estimate of Exp - Plac (if you want Plac - Exp use a -sum(...))
    dEst        <- sum(vCoeff[ vVarNames ])
    
    if( bPlacMinusTrt )
        dEst <- dEst*-1

    dSE         <- sqrt(sum(vcov(glsFit)[ vVarNames, vVarNames ]))
    dTStat      <- dEst/dSE
    dPVal       <- pt( dTStat, nDOF )
    
    

    lRet <- list( dPVal = dPVal, dEst = dEst, nDOF = nDOF, dSE = dSE, dTStat = dTStat )
    return( lRet)
    
}


