######################################################################################################################## .
#' @name SelectArmAndAnalyzePFSTwoStages
#' @title Two-Stage Arm Selection and PFS Analysis
#' @description This function performs a two-stage adaptive analysis for multi-arm clinical trials.
#' Stage 1 selects the best treatment arm based on binary response endpoint.
#' Stage 2 tests efficacy using progression-free survival (PFS) via log-rank test.
#' @author Julija Saltane, J. Kyle Wathen
#'
#' @details **IMPORTANT**: Type I error rate is NOT adjusted for interim data unblinding at Stage 1.
#'
#' **Stage 1 - Arm Selection:**
#' \itemize{
#'   \item Uses first `Stage1NumCompleters` enrolled patients
#'   \item Computes the observed binary response-rate difference for each treatment arm versus control
#'   \item Selects arm with maximum response difference (in case of a tie, arm with lower index is chosen)
#'   \item No formal hypothesis testing or multiplicity adjustment
#'   \item If the best observed treatment effect is below `Stage1FutThreshold`, the trial stops early for futility.
#' }
#'
#' **Stage 2 - Efficacy Analysis:**
#' \itemize{
#'   \item Filters data for control and selected arm
#'   \item Analysis timing determined by `TargetNumPFSEvents`
#'   \item Computes log-rank test statistic (EAST formulas Q.242, Q.243)
#'   \item Estimates the hazard ratio using a Cox proportional hazards model
#'   \item Makes efficacy decision using critical point
#'   \item Dropout can be incorporated.
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
#'          \item{DropoutProportion}{Proportion of patients who drop out during PFS follow-up}
#'          \item{TargetNumPFSEvents}{Target number of PFS events for Stage 2 timing}
#'          \item{SwitchSign}{Character value ('yes' or 'no') indicating whether the critical-point sign should be reversed for the PFS analysis}
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
#'          \item{ErrorCode}{Status code indicating success or error type:
#'              \describe{
#'                \item{ErrorCode = 0}{No Error}
#'                \item{ErrorCode > 0}{Nonfatal error, current simulation is aborted but the next simulations will run}
#'                \item{ErrorCode = -1}{Missing required UserParam (`Stage1NumCompleters` or `TargetNumPFSEvents`)}
#'                \item{ErrorCode = -2}{LookInfo not NULL (adaptive designs not supported)}
#'                \item{ErrorCode = -3}{Insufficient patients for Stage 1 analysis}
#'                \item{ErrorCode = -4}{No valid treatment arm deltas computed}
#'                \item{ErrorCode = -5}{Insufficient patients for Stage 2 analysis}
#'                \item{ErrorCode = -6}{Test statistic denominator is zero}
#'              }}
#'          \item{NumPatientInStage_i}{Number of patients in stage i (i = 1 for arm selection stage,
#'                          i = 2 for PFS analysis stage). Both values populated; other indices are zero.}
#'          \item{NumCompleters_i}{Number of completers (all patients in Stage 1, and additional patients in Stage 2).
#'                          Non-zero only for the selected arm, containing its index value; others are zero.}
#'          \item{ChosenArm_i}{Selected treatment arm indicator (i = 1 to `NumTreatments`).
#'                          Non-zero only for the selected arm, containing its index value; others are zero.}
#'          \item{Stage2AnalysisTiming_i}{Time of Stage 2 analysis. Non-zero only for the selected arm, containing its index value; others are zero.}
#'          \item{CriticalPoint_i}{Critical point for arm i (i = 1 to `NumTreatments`). Non-zero only for the selected arm; others are zero.}
#'          \item{HazardRatio_i}{Estimated hazard ratio from Cox model (only for selected arm).}
#'          \item{Control_Stage_i_Patients}{Number of control patients used in Stage 1 and Stage 2 analysis population.}
#'          \item{Stage1Patients_Arm_i}{Number of Stage 1 patients per treatment arm.}
#'          \item{Stage2Patients_Arm_i}{Number of Stage 2 patients per treatment arm.}
#' }
######################################################################################################################## .

SelectArmAndAnalyzePFSTwoStages <- function( SimData, DesignParam, LookInfo = NULL, UserParam = NULL )
{
    nTrtArms   <- DesignParam$NumTreatments
    nErrorCode <- 0

    # Arranging patients by their arrival time and assigning Patient IDs
    SimData <- SimData[ order( SimData$ArrivalTime ), ]
    SimData$PatientID <- seq_len( nrow( SimData ) )

    # Step 1. Initial Check: User must provide nStage1NumCompleters and TargetNumPFSEvents in UserParam ####
    if( is.null( UserParam ) || is.null( UserParam$Stage1NumCompleters ) )
    {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -1 ) )
    }
    if( is.null( UserParam$Stage1FutThreshold ) )
    {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -1 ) )
    }
    if( is.null( UserParam$TargetNumPFSEvents ) )
    {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -1 ) )
    }
    if( is.null( UserParam$SwitchSign ) )
    {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -1 ) )
    }

    # Step 2. Verify that a fixed design is being used ####
    if( is.null( LookInfo ) )
    {
        nQtyOfLooks  <- 1
        nLookIndex   <- 1
        # In this implementation, treatment superiority for the log-rank statistic corresponds to negative values. In contrast,
        # the binary difference-in-proportions statistic used in Stage 1 (and in project setup) is defined such that superiority
        # corresponds to positive values. To ensure consistency in the decision rule, we negate the critical point for the log-rank test.
        dSwitchSign    <- ifelse( tolower( UserParam$SwitchSign ) == "yes", -1, 1 )
        dCriticalPoint <- dSwitchSign * DesignParam$CriticalPoint
    }
    else
    {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -2 ) )
    }

    # Step 3. Setting up the variables ####
    nStage1NumCompleters <- UserParam$Stage1NumCompleters
    nTargetNumPFSEvents  <- UserParam$TargetNumPFSEvents

    # Step 4. Adding Absolute PFS Event Time: ####
    SimData$TimeOfPFSEvent <- SimData$ArrivalTime + SimData$PFSNonCens

    #-------------------------------------------------------------------------------------------------------------------------
    # Stage 1 ####

    # Check there are enough patients in the dataset to perform Stage 1 analysis
    if( nrow( SimData ) < nStage1NumCompleters )
    {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -3 ) )
    }

    # Use only first nStage1NumCompleters patients
    dfSimDataStage1   <- SimData[ 1:nStage1NumCompleters, ]

    # Number of Stage 1 patients per arm
    vStage1RecruitedPatientsPerArm <- as.vector( table( dfSimDataStage1$TreatmentID ) )

    # Assign patients enrolled before the Stage 1 cutoff to Stage 1; remaining patients are assigned to Stage 2
    dfSimDataStage1$Stage <- 1
    SimData$Stage <- ifelse( seq_len( nrow( SimData ) ) <= nStage1NumCompleters, 1, 2 )

    vPatientOutcome   <- dfSimDataStage1$Response
    vPatientTreatment <- dfSimDataStage1$TreatmentID

    # Control group outcomes
    vOutcomesCtrl <- vPatientOutcome[ vPatientTreatment == 0 ]
    dMeanCtrl     <- mean( vOutcomesCtrl )

    # For each treatment arm, compute response rate difference vs control
    vDelta <- rep( NA, nTrtArms )

    for( nTrtID in 1:nTrtArms )
    {
        vOutcomesTrt <- vPatientOutcome[ vPatientTreatment == nTrtID ]

        if( length( vOutcomesTrt ) > 0 && length( vOutcomesCtrl ) > 0 )
        {
            dMeanTrt       <- mean( vOutcomesTrt )
            vDelta[ nTrtID ] <- dMeanTrt - dMeanCtrl
        }
    }

    if( all( is.na( vDelta ) ) )
    {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -4 ) )
    }

    # Futility Assessment: if no arm meets minimum effectiveness threshold → stop trial
    dBestDelta <- max( vDelta, na.rm = TRUE )

    if( dBestDelta < UserParam$Stage1FutThreshold )
    {
        strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                                   bIAFutilityCondition = TRUE )

        nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

        return( ReturnResult( nTrtArms = nTrtArms,
                              nErrorCode = nErrorCode,
                              nBestArm = NA_integer_,
                              dCriticalPoint = NA_real_,
                              dTS = NA_real_,
                              nDecision = nDecision ) )
    }

    # Select the arm with the largest response difference (in case of a tie, arm with lower index is chosen)
    nBestArm <- which( vDelta == max( vDelta, na.rm = TRUE ) )

    if( length( nBestArm ) > 1 ) nBestArm <- min( nBestArm )

    #-------------------------------------------------------------------------------------------------------------------------
    # Stage 2: Efficacy analysis using PFS data for selected arm vs control ####

    # Subset SimData to only include patients from control and selected arm
    vIndxPatientsToBeUsedInStage2 <- which( SimData$TreatmentID == 0 | SimData$TreatmentID == nBestArm )
    dfSimDataStage2 <- SimData[ vIndxPatientsToBeUsedInStage2, ]

    # Create a treatment indicator: 1 = selected treatment arm, 0 = control
    dfSimDataStage2$Trt <- ifelse( dfSimDataStage2$TreatmentID == nBestArm, 1, 0 )

    # Simulate dropout during PFS follow-up
    if( !is.null( UserParam$DropoutProportion ) && UserParam$DropoutProportion > 0 )
    {
        # Number of patients who drop out
        nDropouts <- floor( nrow( dfSimDataStage2 ) * UserParam$DropoutProportion )

        # Randomly select dropout patients
        vDropoutIndices <- if( nDropouts > 0 )
        {
            sample( seq_len( nrow( dfSimDataStage2 ) ), nDropouts )
        }
        else
        {
            integer( 0 )
        }

        # Create dropout vector: 0 = patient stays, 1 = patient drops out
        dfSimDataStage2$Dropout <- 0
        dfSimDataStage2$Dropout[ vDropoutIndices ] <- 1

        # Default dropout time = Inf
        dfSimDataStage2$DropoutTime <- Inf

        if( nDropouts > 0 )
        {
            # For dropout patients, simulate uniform dropout time between 0 and PFS
            dfSimDataStage2$DropoutTime[ vDropoutIndices ] <- runif( n = length( vDropoutIndices ),
                                                                     min = 0,
                                                                     max = dfSimDataStage2$PFSNonCens[ vDropoutIndices ] )
        }
    }
    else
    {
        # No dropout scenario
        dfSimDataStage2$Dropout     <- 0
        dfSimDataStage2$DropoutTime <- Inf
    }

    # Calculate actual time of dropout
    dfSimDataStage2$TimeOfDropout <- dfSimDataStage2$ArrivalTime + dfSimDataStage2$DropoutTime

    # PFSEventWithDropout:
    #   0 = patient censored due to dropout before PFS event
    #   1 = PFS event observed before dropout
    dfSimDataStage2$PFSEventWithDropout <- as.integer( dfSimDataStage2$TimeOfDropout >= dfSimDataStage2$TimeOfPFSEvent )

    # Total number of PFS events observed in the data
    vPFSEventTimes <- dfSimDataStage2$TimeOfPFSEvent[ dfSimDataStage2$PFSEventWithDropout == 1 ]

    # Check that the target number of observed PFS events has been reached
    if( length( vPFSEventTimes ) < nTargetNumPFSEvents )
    {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -5 ) )
    }
    dStage2AnalysisTiming <- sort( vPFSEventTimes )[ nTargetNumPFSEvents ]

    # Preparing data for analysis
    # PFSEvent: 1 = event, 0 = censored
    # PFSObservedTime: time for PFS analysis (time relative to the patient)
    # Note that as we have full data, we'll have to filter out patients that arrived after the Stage 2 analysis, then calculate PFS parameters:
    dfSimDataStage2 <- dfSimDataStage2[ dfSimDataStage2$ArrivalTime <= dStage2AnalysisTiming, ]

    dfSimDataStage2$PFSEvent        <- ifelse( dfSimDataStage2$TimeOfPFSEvent <= dStage2AnalysisTiming &
                                                   dfSimDataStage2$TimeOfPFSEvent <= dfSimDataStage2$TimeOfDropout,
                                               1, 0 )

    dfSimDataStage2$PFSObservedTime <- pmin( dfSimDataStage2$TimeOfPFSEvent,
                                             dfSimDataStage2$TimeOfDropout,
                                             dStage2AnalysisTiming ) - dfSimDataStage2$ArrivalTime

    nStage2NumPatients <- nrow( dfSimDataStage2 )

    # Order the data by observed time
    dfSimDataStage2 <- dfSimDataStage2[ order( dfSimDataStage2$PFSObservedTime ), ]

    # Compute Observed HR
    coxModel <- survival::coxph( survival::Surv( PFSObservedTime, PFSEvent ) ~ Trt, data = dfSimDataStage2 )
    dTrueHR  <- exp( coxModel$coefficients )

    dfSimDataStage2$EventOnTreatment <- ifelse( dfSimDataStage2$Trt == 1, dfSimDataStage2$PFSEvent, 0 )
    dfSimDataStage2$EventOnControl   <- ifelse( dfSimDataStage2$Trt == 0, dfSimDataStage2$PFSEvent, 0 )

    nSubjectsAtRiskTreatment <- nrow( dfSimDataStage2[ dfSimDataStage2$Trt == 1, ] )
    nSubjectsAtRiskControl   <- nrow( dfSimDataStage2[ dfSimDataStage2$Trt == 0, ] )

    # Initialize intermediate quantities required for test statistic computation
    dNum <- 0
    dDen <- 0

    # Iterate over subjects to calculate dNum and dDen required for test statistic computation
    for( nSubject in seq_len( nrow( dfSimDataStage2 ) ) )
    {   # Update the count of subjects at risk for each arm for non event times
        if( dfSimDataStage2$PFSEvent[ nSubject ] == 0 )
        {
            if( dfSimDataStage2$Trt[ nSubject ] == 1 )
            {
                nSubjectsAtRiskTreatment <- nSubjectsAtRiskTreatment - 1
            }
            if( dfSimDataStage2$Trt[ nSubject ] == 0 )
            {
                nSubjectsAtRiskControl   <- nSubjectsAtRiskControl - 1
            }
        }
            # For subjects with events, compute dNum and dDen
            if( dfSimDataStage2$PFSEvent[ nSubject ] == 1 )
            {
                nEventsOnTreatment <- dfSimDataStage2$EventOnTreatment[ nSubject ]
                nEventsOnControl   <- dfSimDataStage2$EventOnControl[ nSubject ]
                nEvents            <- nEventsOnTreatment + nEventsOnControl
                nSubjectsAtRisk    <- nSubjectsAtRiskTreatment + nSubjectsAtRiskControl

                # Equation Q.242 in East Manual
                dNum <- dNum + nEventsOnTreatment - nSubjectsAtRiskTreatment * nEvents / nSubjectsAtRisk

                # Generate dDen based on number of subjects at risk
                if( nSubjectsAtRisk != 1 )
                {
                    # Equation Q.243 in East Manual
                    dDen <- dDen + nSubjectsAtRiskTreatment * nSubjectsAtRiskControl * ( nSubjectsAtRisk - nEvents ) * nEvents / ( ( nSubjectsAtRisk - 1 ) * nSubjectsAtRisk ^ 2 )
                }
                # Update the count of subjects at risk before the next iteration
                nSubjectsAtRiskTreatment <- nSubjectsAtRiskTreatment - nEventsOnTreatment
                nSubjectsAtRiskControl   <- nSubjectsAtRiskControl - nEventsOnControl
            }
    }

    # Check that dDen is not zero
    if( dDen == 0 )
    {
        return( ReturnResult( nTrtArms = nTrtArms, nErrorCode = -6 ) )
    }

    # Compute the log-rank test statistic
    dTS <- dNum / sqrt( dDen )

    strDecision <- CyneRgy::GetDecisionString( LookInfo, nLookIndex, nQtyOfLooks,
                                               bIAEfficacyCondition = dTS < dCriticalPoint,
                                               bFAEfficacyCondition = dTS < dCriticalPoint )

    nDecision <- CyneRgy::GetDecision( strDecision, DesignParam, LookInfo )

    # Calculate the number of patients that were recruited (and their PFS data was used for analysis) at Stage 2
    dfStage2PatientPFSData <- dfSimDataStage2[ dfSimDataStage2$Stage == 2, ]

    vStage2RecruitedPatientsPerArm <- as.vector( table( dfStage2PatientPFSData$TreatmentID ) )

    nCompleters <- nrow( dfSimDataStage1 ) + nrow( dfStage2PatientPFSData )

    lRet <- ReturnResult( nTrtArms,
                          nErrorCode,
                          nBestArm,
                          dCriticalPoint,
                          dTS,
                          nDecision,
                          nStage1NumCompleters,
                          nStage2NumPatients,
                          dStage2AnalysisTiming,
                          nCompleters,
                          dTrueHR,
                          vStage1RecruitedPatientsPerArm,
                          vStage2RecruitedPatientsPerArm )

    return( lRet )

}

##################
ReturnResult <- function( nTrtArms,
                          nErrorCode,
                          nBestArm = NA_integer_,
                          dCriticalPoint = NA_real_,
                          dTS = NA_real_,
                          nDecision = NA_integer_,
                          nStage1NumCompleters = NA_integer_,
                          nStage2NumPatients = NA_integer_,
                          dStage2AnalysisTiming = NA_real_,
                          nCompleters = NA_integer_,
                          dTrueHR = NA_real_,
                          vStage1RecruitedPatientsPerArm = rep( NA_integer_, nTrtArms + 1 ),
                          vStage2RecruitedPatientsPerArm = rep( NA_integer_, 2 ) )
{
    vTestStat <- rep( 0, nTrtArms )
    vTestStat[ nBestArm ] <- dTS

    vDecision <- rep( 0, nTrtArms )
    vDecision[ nBestArm ] <- nDecision

    lRet <- list( TestStat  = as.double( vTestStat ),
                  Decision  = as.integer( vDecision ),
                  ErrorCode = as.integer( nErrorCode ) )

    for( i in 1:nTrtArms )
    {
        lRet[[ paste0( "NumPatientInStage_", i ) ] ] <- as.integer( ifelse( i == 1, nStage1NumCompleters,
                                           ifelse( i == 2, nStage2NumPatients, 0 ) ) )
        lRet[[ paste0( "NumCompleters_", i ) ] ]        <- as.integer( ifelse( i == nBestArm, nCompleters, 0 ) )
        lRet[[ paste0( "ChosenArm_", i ) ] ]            <- as.integer( ifelse( i == nBestArm, nBestArm, 0 ) )
        lRet[[ paste0( "Stage2AnalysisTiming_", i ) ] ] <- as.double( ifelse( i == nBestArm, dStage2AnalysisTiming, 0 ) )
        lRet[[ paste0( "CriticalPoint_", i ) ] ]        <- as.double( ifelse( i == nBestArm, dCriticalPoint, 0 ) )
        lRet[[ paste0( "HazardRatio_", i ) ] ]          <- as.double( ifelse( i == nBestArm, dTrueHR, 0 ) )
        lRet[[ paste0( "Control_Stage_", i, "_Patients" ) ] ] <- as.double( ifelse( i == 1, vStage1RecruitedPatientsPerArm[ 1 ],
                                                ifelse( i == 2, vStage2RecruitedPatientsPerArm[ 1 ], 0 ) ) )
        lRet[[ paste0( "Stage1Patients_Arm_", i ) ] ] <- vStage1RecruitedPatientsPerArm[ i + 1 ]
        lRet[[ paste0( "Stage2Patients_Arm_", i ) ] ] <- as.double( ifelse( i == nBestArm, vStage2RecruitedPatientsPerArm[ 2 ], 0 ) )

    }

    return( lRet )
}
