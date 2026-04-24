######################################################################################################################## .
#' @name SelectArmAndAnalyzePFSTwoStages
#' @title Two-Stage Arm Selection and PFS Analysis
#' @description This function performs a two-stage adaptive analysis for multi-arm clinical trials.
#' Stage 1 selects the best treatment arm based on binary response endpoint.
#' Stage 2 tests efficacy using progression-free survival (PFS) via log-rank test.
#'
#' @details **IMPORTANT**: Type I error rate is NOT adjusted for interim data unblinding at Stage 1.
#'
#' **Stage 1 - Arm Selection:**
#' \itemize{
#'   \item Uses first `Stage1NumCompleters` enrolled patients
#'   \item Calculates mean binary response for each arm vs control
#'   \item Selects arm with maximum response difference (ties broken randomly)
#'   \item No formal hypothesis testing or multiplicity adjustment
#' }
#'
#' **Stage 2 - Efficacy Analysis:**
#' \itemize{
#'   \item Filters data for control and selected arm
#'   \item Analysis timing determined by `TargetNumPFSEvents`
#'   \item Computes log-rank test statistic (EAST formulas Q.242, Q.243)
#'   \item Estimates the hazard ratio using a Cox proportional hazards model
#'   \item Makes efficacy decision using critical boundary
#' }
#'
#' @param SimData Data frame with subject data generated in current simulation with one row per patient. 
#'        It will have headers indicating the names of the columns. These names will be the same as those used in 
#'        Data Generation. For analysis the most relevant variables are:
#'        \describe{
#'          \item{ArrivalTime}{Numeric vector of subject arrival times}
#'          \item{TreatmentID}{Integer vector indicating treatment assignment for each subject (0 = control, 1...n = treatment arms)}
#'          \item{Response}{Integer vector of binary response outcomes}
#'          \item{PFSNonCens}{Numeric vector of PFS times relative to patient enrollment}
#'        }
#' @param DesignParam A list containing design and simulation parameters required to compute test statistics and perform testing. Key elements include:
#'        \describe{
#'          \item{Alpha}{Type I error rate (significance level).}
#'          \item{TestType}{Type of test:
#'              \describe{
#'                \item{0}{One-sided test.}
#'                \item{1}{Two-sided symmetric test.}
#'                \item{2}{Two-sided asymmetric test.}
#'              }}
#'          \item{TailType}{Tail direction:
#'              \describe{
#'                \item{0}{Left-tailed test.}
#'                \item{1}{Right-tailed test.}
#'              }}
#'          \item{NumTreatments}{Number of treatment arms (excluding control)}
#'          \item{CriticalPoint}{Critical value for testing (e.g., 1.96)}
#'        }
#' @param LookInfo List with interim analysis information, or NULL for fixed design. **Currently only fixed design is supported**, adaptive designs not yet implemented.
#' @param UserParam A list of user-defined parameters. These custom scalar variables can be of types Integer, Numeric, or Character. Relevant elements include:
#'        \describe{
#'          \item{Stage1NumCompleters}{Number of patients for Stage 1 analysis}
#'          \item{Stage1FutThreshold}{Stage 1 futility threshold}
#'          \item{DropoutRate}{Drop-out rate in PFS follow-up}
#'          \item{TargetNumPFSEvents}{Target number of PFS events for Stage 2 timing}
#'          \item{SwitchSign}{Character entry 'yes' or 'no, informing the function if the critical point sign has to be reverted for the PFS analysis}        
#'        }
#'
#' @return A list containing the following elements:
#'         \describe{
#'          \item{TestStat}{Numeric vector of log-rank test statistics. In the summary statistics file, this appears as separate columns (TestStat1, TestStat2, ..., TestStat_n). 
#'                          Vector length equals `NumTreatments` per engine requirements. Only the element corresponding to the selected arm contains the actual test statistic; 
#'                          all other elements are zero and should be disregarded. For example, if arm 2 is selected at Stage 1, `TestStat2` contains the log-rank statistic while 
#'                          `TestStat1` is zero.}
#'          \item{Decision}{Integer vector of efficacy decisions. Only the selected arm's element is non-zero; others are zero and should be disregarded. For example, if arm 2 is selected, 
#'                          only `Decision2` contains the decision value. Engine requirement mandates vector length equals `NumTreatments`.}
#'          \item{ChosenArm_i}{Selected treatment arm indicator (i = 1 to `NumTreatments`).
#'                          Non-zero only for the selected arm, containing its index value; others are zero.}
#'          \item{NumPatientInStage_i}{Number of patients in stage i (i = 1 for arm selection stage,
#'                          i = 2 for PFS analysis stage). Both values populated; other indices are zero.}
#'          \item{CriticalPoint_i}{Critical point for arm i (i = 1 to `NumTreatments`).
#'                          Non-zero only for the selected arm; others are zero.}
#'          \item{HazardRatio_i}{Observed hazard ratio for arm i (i = 1 to `NumTreatments`).
#'                          Non-zero only for the selected arm; others are zero.}
#'          \item{ErrorCode}{Status code indicating success or error type:
#'              \describe{ 
#'                \item{ErrorCode = 0}{No Error}
#'                \item{ErrorCode > 0}{Non fatal error, current simulation is aborted but the next simulations will run}
#'                \item{ErrorCode = -1}{Missing required UserParam (`Stage1NumCompleters` or `TargetNumPFSEvents`)}
#'                \item{ErrorCode = -2}{LookInfo not NULL (adaptive designs not supported)}
#'                \item{ErrorCode = -3}{Insufficient patients for Stage 1 analysis}
#'                \item{ErrorCode = -4}{No valid treatment arm deltas computed}
#'                \item{ErrorCode = -5}{Insufficient patients for Stage 2 analysis}
#'                \item{ErrorCode = -6}{Test statistic denominator is zero}
#'              }}
#'        }      
######################################################################################################################## .

SelectArmAndAnalyzePFSTwoStages <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    library( survival )
    library( Mediana )
    
    nTrtArms   <- DesignParam$NumTreatments
    nErrorCode <- 0
    SimData$PatientID <- seq_len(nrow(SimData))
    
    # Step 1. Initial Check: User must provide nStage1NumCompleters and TargetNumPFSEvents in UserParam ####
    if( is.null( UserParam ) || is.null( UserParam$Stage1NumCompleters ) )
    {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -1 ) )
    }
    if (is.null(UserParam$Stage1FutThreshold))
    {
        return(ReturnResult(nTrtArms = nTrtArms,  nErrorCode = -1))
    }
    if( is.null( UserParam$TargetNumPFSEvents ) )
    {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -1 ) )
    }
    
    # Step 2. This only works for fixed design:
    if( is.null( LookInfo ) )
    { 
        nQtyOfLooks  <- 1
        nLookIndex   <- 1
        # The log-rank test statistic is defined such that treatment superiority corresponds to negative values. In contrast, 
        # the binary difference-in-proportions statistic used in Stage 1 (and in project setup) is defined such that superiority 
        # corresponds to positive values. To ensure consistency in the decision rule, we negate the critical point for the log-rank test.
        dSwitchSign    <- ifelse( tolower(UserParam$SwitchSign) == "yes", -1, 1 )
        dCriticalPoint <- dSwitchSign * DesignParam$CriticalPoint
    } else {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -2 ) )   
    }
    
    # Step 3. Setting up the variables ####
    nStage1NumCompleters <- UserParam$Stage1NumCompleters
    nTargetNumPFSEvents  <- UserParam$TargetNumPFSEvents 
    
    # Step 4. Adding Absolute PFS Event Time: ####
    SimData$TimeOfPFSEvent <- SimData$ArrivalTime + SimData$PFSNonCens
    
    # now SimData contains: 
    # - Response (binary outcome)
    # - PFSNonCens (PFS)
    # - ArrivalTime
    # - TreatmentID
    # - PatientID
    # - TimeOfPFSEvent
    
    
    #-------------------------------------------------------------------------------------------------------------------------
    # Stage 1 ####
    
    # Check there are enough patients in the dataset to even do the Stage 1 analysis:
    if ( nrow( SimData ) < nStage1NumCompleters )
    {
        return( ReturnResult( nTrtArms = nTrtArms, 
                              nErrorCode = -3 ) )
    }
    
    # NOTE: the code below can be substituted by any other functionality to perform binary endpoint testing.
    # The current code is just an outline of what can be done
    
    # Use only first nStage1NumCompleters patients
    dfSimDataStage1   <- SimData[ 1:nStage1NumCompleters, ]
    
    # MY CHECK
    head(dfSimDataStage1)
    nrow(dfSimDataStage1)==nStage1NumCompleters
    
    #assigning these patients to Stage 1, and all the other patients in SimData to Stage 2 (note that later we will filter out patients who enrolled after
    # Stage 2 analysis, and those patients would be ignored.)
    dfSimDataStage1$Stage <- 1
    SimData$Stage <- ifelse(seq_len(nrow(SimData)) <= nStage1NumCompleters, 1, 2)
    
    vPatientOutcome   <- dfSimDataStage1$Response
    vPatientTreatment <- dfSimDataStage1$TreatmentID
    
    # Control group outcomes
    vOutcomesCtrl <- vPatientOutcome[ vPatientTreatment == 0 ]
    dMeanCtrl     <- mean(vOutcomesCtrl)    
    
    # For each arm present, compute response rate difference vs control
    vDelta <- rep( NA, nTrtArms )
    
    for( nTrtID in 1:nTrtArms )
    {
        vOutcomesTrt <- vPatientOutcome[ vPatientTreatment == nTrtID ]
        
        if( length( vOutcomesTrt ) > 0 && length( vOutcomesCtrl ) > 0 )
        {
            dMeanTrt  <- mean(vOutcomesTrt)
            vDelta[nTrtID] <- dMeanTrt - dMeanCtrl
        }
    }
    
    if( all( is.na( vDelta ) ) )
    {
        return( ReturnResult( nTrtArms = nTrtArms,
                              nErrorCode = -4 ) )
    }
    
    #-----------------------------
    # [ADD ON] Stage 1: Futility Assessment
    #-----------------------------
    
    # If no arm meets minimum effectiveness threshold → stop trial    
    dBestDelta <- max(vDelta, na.rm = TRUE)
    if ( dBestDelta < UserParam$Stage1FutThreshold )
    {
        # return( ReturnResult( nTrtArms = nTrtArms,
        #                       nErrorCode = 10 ) )
        strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, 
                                                   bIAFutilityCondition = TRUE)
        
        nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
        return(ReturnResult( nTrtArms = nTrtArms,    
                             nErrorCode = nErrorCode,
                             nBestArm = NA_integer_,
                             dCriticalPoint = NA_real_,
                             ZComb = NA_real_,
                             nDecision = nDecision))
    }
    
    #-[ END of ADD ON] ---------------
    
    # Selects the arm with the largest response difference (ties broken at random)
    
    nBestArm <- which( vDelta == max( vDelta, na.rm=TRUE ) )
    
    if( length(nBestArm) > 1 ) nBestArm <- sample( nBestArm, 1 )
    
    # Keep only data for patients for control and arm of interest:
    vIndxPatientsToBeUsedInStage2 <- which( SimData$TreatmentID == 0 | SimData$TreatmentID == nBestArm )

    #-------------------------------------------------------------------------------------------------------------------------
    # Stage 2: Efficacy analysis using PFS data for selected arm vs control ####
    
    # Subset SimData to only include patients from control and chosen arm (stage 1)
    dfSimDataStage2 <- SimData[ vIndxPatientsToBeUsedInStage2, ]

    # MY CHECK
    head(dfSimDataStage2)
    sum(dfSimDataStage2$PatientID != vIndxPatientsToBeUsedInStage2) == 0 # Patient IDs must match indeces of patients chosen

    # Create a treatment indicator: 1 = selected treatment arm, 0 = control
    dfSimDataStage2$Trt <- ifelse( dfSimDataStage2$TreatmentID == nBestArm, 1, 0 )
    
    
    # dfSimDataStage2 now contains:
    # - Response (binary outcome)
    # - PFSNonCens (PFS)
    # - ArrivalTime
    # - TreatmentID
    # - PatientID
    # - TimeOfPFSEvent
    # - Stage
    # - Trt (0 - control, 1 - treatment arm (regardless which one was chosen))
    
    
    #-----------------------------
    # [ADD ON] PFS dropout
    #-----------------------------
    if (!is.null(UserParam$DropoutRate) & UserParam$DropoutRate != 0)
    {
         # Generate Dropout Time per patient (time elapsed after the patient arrived)
         dfSimDataStage2$DropoutTime <- rexp( nrow( dfSimDataStage2 ),
                                              rate = UserParam$DropoutRate )

    } else {
        dfSimDataStage2$DropoutTime <- Inf
    }
    #Calculate actual Time of Dropout :
    dfSimDataStage2$TimeOfDropout <- dfSimDataStage2$ArrivalTime + dfSimDataStage2$DropoutTime

    # MY CHECK: 
    head(dfSimDataStage2)
    
    # As some patients have dropped out before showing an Event, we would consider PFS event for these patients when calculating the Time of analysis
    #PFSEventWithDropout = 0 if event was NOT observed (as patient dropped out before PFS)
    #                    = 1 if an event was observed (as patient did not drop out or dropped out after the PFS event)
    dfSimDataStage2$PFSEventWithDropout <- as.integer(dfSimDataStage2$TimeOfDropout >= dfSimDataStage2$TimeOfPFSEvent)
    
    # MY CHECK: 
    head(dfSimDataStage2)
    
    #total number of PFS events observed in the data
    vPFSEventTimes <- dfSimDataStage2$TimeOfPFSEvent[dfSimDataStage2$PFSEventWithDropout == 1]
    
    # MY CHECK:
    if (UserParam$DropoutRate == 0) {length(vPFSEventTimes) == nrow(dfSimDataStage2)} #must be true if no dropout
    if (UserParam$DropoutRate > 0)  {length(vPFSEventTimes) < nrow(dfSimDataStage2)}
    
    
    # Check that the number of patients is at least the target number of PFS events:
    if ( length(vPFSEventTimes) < nTargetNumPFSEvents )
    {
        return( ReturnResult( nTrtArms = nTrtArms,
                              nErrorCode = -5 ) )  
    }
    # we sort and using the patients who actually showed PFS events  (and not dropped out before showing PFS event) we determine the analysis timing.
    dStage2AnalysisTiming <- sort(vPFSEventTimes)[nTargetNumPFSEvents]
    
    #-[ END of ADD ON] ---------------
    

    # We now need to calculate:
    # PFSEvent: 1 = event, 0 = censored
    # PFSObservedTime: time for PFS analysis (time relative to the patient)    
    # Note that as we have full data, we'll have to filter out patients that arrived after the Stage 2 analysis, then calculate PFS parameters:
    dfSimDataStage2 <- dfSimDataStage2[ dfSimDataStage2$ArrivalTime <= dStage2AnalysisTiming ,]
    
    dfSimDataStage2$PFSEvent        <- ifelse( dfSimDataStage2$TimeOfPFSEvent <= dStage2AnalysisTiming & 
                                               dfSimDataStage2$TimeOfPFSEvent <= dfSimDataStage2$TimeOfDropout, 
                                               1, 0 )   
    # dfSimDataStage2$PFSObservedTime <- ifelse( dfSimDataStage2$TimeOfPFSEvent > dStage2AnalysisTiming | dfSimDataStage2$TimeOfPFSEvent > dfSimDataStage2$TimeOfDropout, 
    #                                            pmin(dStage2AnalysisTiming, dfSimDataStage2$TimeOfDropout) - dfSimDataStage2$ArrivalTime, 
    #                                            dfSimDataStage2$PFSNonCens )
    dfSimDataStage2$PFSObservedTime <- pmin( dfSimDataStage2$TimeOfPFSEvent,
                                             dfSimDataStage2$TimeOfDropout,
                                             dStage2AnalysisTiming) - dfSimDataStage2$ArrivalTime
    
    nStage2NumPatients <- nrow( dfSimDataStage2 )
    
    # NOTE: the code below can be substituted by any other functionality to perform efficacy testing
    # The current code is just an outline of what can be done
    
    # Order the data by observed time for the remainder of the computations
    dfSimDataStage2   <- dfSimDataStage2[ order( dfSimDataStage2$PFSObservedTime), ]
    
    ##### Analysing Stage 1 patients only -> Get Z1
    dfStage1PatientPFSData <- dfSimDataStage2[dfSimDataStage2$Stage == 1, ]
    nrow(dfStage1PatientPFSData) # total number of patients for Stage1
    Stage1coxModel <- coxph( Surv( PFSObservedTime, PFSEvent ) ~ Trt, data = dfStage1PatientPFSData )
    Z1 <- summary(Stage1coxModel)$coef[1, "z"]
    
    #Z1_ajd to be calculated (rank-based Dunnett)
    p1     <- 1 - pnorm(Z1)
    p1_adj <- AdjustPvalues(pval = p1 ,
                          proc = "DunnettAdj",
                          par = parameters(n = Inf))
    
    ##### Analysing Stage 2 patients only > Get Z2
    dfStage2PatientPFSData <- dfSimDataStage2[dfSimDataStage2$Stage == 2, ]
    nrow(dfStage2PatientPFSData)
    Stage2coxModel <- coxph( Surv( PFSObservedTime, PFSEvent ) ~ Trt, data = dfStage2PatientPFSData )
    Z2 <- summary(Stage2coxModel)$coef[1, "z"]
    p2 <- 1-pnorm(Z2)
    
    
    ##### Deriving weights:
    nPatientsStage1 = nrow(dfStage1PatientPFSData)
    nPatientsStage2 = nrow(dfStage2PatientPFSData)
    
    dWeightStage1 = sqrt(nPatientsStage1/(nPatientsStage1+nPatientsStage2))
    dWeightStage2 = sqrt(nPatientsStage2/(nPatientsStage1+nPatientsStage2))
    
    p_comb = 1 - pnorm(dWeightStage1*qnorm(1-p1_adj)+ dWeightStage2*qnorm(1-p2))
    print(p_comb)
    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks, 
                                               bIAEfficacyCondition = p_comb <  DesignParam$Alpha, 
                                               bFAEfficacyCondition = p_comb <  DesignParam$Alpha )
    
    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )
    
    lRet <- ReturnResult( nTrtArms,
                          nErrorCode,
                          nBestArm,
                          dCriticalPoint,
                          ZComb,
                          nDecision,
                          nStage1NumCompleters,
                          nStage2NumPatients)
    
    return( lRet )
    
}


##################
ReturnResult <- function( nTrtArms,    
                          nErrorCode,
                          nBestArm = NA_integer_,
                          dCriticalPoint = NA_real_,
                          ZComb = NA_real_,
                          nDecision = NA_integer_,
                          nStage1NumCompleters = NA_integer_,
                          nStage2NumPatients = NA_integer_)
{
    vTestStat <- rep( 0, nTrtArms )
    vTestStat[ nBestArm ] <- ZComb
    
    vDecision <- rep( 0, nTrtArms )
    vDecision[ nBestArm ] <- nDecision
    
    lRet <- list( TestStat = as.double( vTestStat ),
                  Decision = as.integer( vDecision ), 
                  ErrorCode = as.integer( nErrorCode ) )
    
    for ( i in 1:nTrtArms ) 
    {
        lRet[[paste0("ChosenArm_", i)]]         <- as.integer( ifelse( i == nBestArm, nBestArm, 0 ) )
        lRet[[paste0("NumPatientInStage_", i)]] <- as.integer( ifelse( i == 1, nStage1NumCompleters, 
                                                                       ifelse( i == 2, nStage2NumPatients, 0 ) ) )
        lRet [[paste0("CriticalPoint_", i)]]    <- as.double( ifelse( i == nBestArm, dCriticalPoint, 0 ) )
    }
    
    return( lRet )
}
