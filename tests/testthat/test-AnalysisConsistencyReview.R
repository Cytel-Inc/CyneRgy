######################################################################################################################## .
# Description: Regression checks for corrected example analysis data and boundary handling.
######################################################################################################################## .

test_that( "ordinary analyses exclude simulated outcomes for subjects who dropped out", {
    lCases <- list(
        list( "2ArmBinaryOutcomeAnalysis", "AnalyzeUsingPropTest", NULL ),
        list( "2ArmBinaryOutcomeAnalysis", "AnalyzeUsingEastManualFormula", NULL ),
        list( "2ArmBinaryOutcomeAnalysis", "AnalyzeUsingPropLimitsOfCI",
            list( dConfLevel = 0.9, dLowerLimit = 0.1, dUpperLimit = 0 ) ),
        list( "2ArmBinaryOutcomeAnalysis", "AnalyzeUsingBetaBinomial",
            list( dAlphaCtrl = 1, dBetaCtrl = 1, dAlphaExp = 1, dBetaExp = 1,
                dUpperCutoffEfficacy = 0.95, dLowerCutoffForFutility = 0.1 ) ),
        list( "2ArmNormalOutcomeAnalysis", "AnalyzeUsingTTestNormal", NULL ),
        list( "2ArmNormalOutcomeAnalysis", "AnalyzeUsingEastManualFormulaNormal", NULL ),
        list( "2ArmNormalOutcomeAnalysis", "AnalyzeUsingMeanLimitsOfCI",
            list( dConfLevel = 0.9, dMAV = 0.1, dTV = 0 ) )
    )
    for ( lCase in lCases ) {
        fnAnalyze <- .GetCommonExampleFunction( lCase[[ 1 ]], paste0( lCase[[ 2 ]], ".R" ), lCase[[ 2 ]] )
        dfData <- data.frame( TreatmentID = rep( 0:1, each = 30 ) )
        if ( lCase[[ 1 ]] == "2ArmBinaryOutcomeAnalysis" ) {
            dfData$Response <- c( rep( 1, 10 ), rep( 0, 20 ), rep( 1, 25 ), rep( 0, 5 ) )
        } else {
            dfData$Response <- c( seq( -1, 1, length.out = 30 ), seq( 1, 3, length.out = 30 ) )
        }
        lDesign <- list( TailType = 1, CriticalPoint = 1.96, MaxCompleters = 60 )
        set.seed( 389 )
        lExpected <- fnAnalyze( dfData, lDesign, UserParam = lCase[[ 3 ]] )
        for ( strIndicator in c( "CensorInd", "CensorIndOrg" ) ) {
            dfWithDropouts <- rbind( dfData,
                data.frame( TreatmentID = rep( 0:1, each = 30 ), Response = rep( c( 1, 0 ), each = 30 ) ) )
            dfWithDropouts[[ strIndicator ]] <- rep( c( 1, 0 ), each = 60 )
            set.seed( 389 )
            lObserved <- fnAnalyze( dfWithDropouts, lDesign, UserParam = lCase[[ 3 ]] )
            expect_equal( lObserved, lExpected, info = lCase[[ 2 ]] )
        }
        if ( lCase[[ 2 ]] == "AnalyzeUsingBetaBinomial" ) {
            expect_length( lExpected$Delta, 1 )
            expect_equal( lExpected$Delta, 15 / 32 )
        }
    }
} )

test_that( "multi-arm binary and continuous analyses preserve NA for absent arms", {
    lDesign <- list( TailType = 1, Alpha = 0.025, NumTreatments = 2, IsArmPresent = c( 0, 1 ) )
    for ( strFunction in c( "AnalyzeMultiArmUsingPropTestBonferroni", "AnalyzeMultiArmUsingTTestBonferroni" ) ) {
        fnAnalyze <- .GetCommonExampleFunction( "MultiArmAnalysis", paste0( strFunction, ".R" ), strFunction )
        dfData <- data.frame( TreatmentID = rep( c( 0, 2 ), each = 40 ) )
        if ( strFunction == "AnalyzeMultiArmUsingPropTestBonferroni" ) {
            dfData$Response <- c( rep( 1, 10 ), rep( 0, 30 ), rep( 1, 35 ), rep( 0, 5 ) )
        } else {
            dfData$Response <- c( seq( -1, 1, length.out = 40 ), seq( 1, 3, length.out = 40 ) )
        }
        lResult <- fnAnalyze( dfData, lDesign )
        expect_identical( lResult$Decision, c( NA_integer_, 2L ), info = strFunction )
    }
} )

test_that( "multi-arm survival analyses use native p-value-scale efficacy boundaries", {
    skip_if_not_installed( "survival" )
    set.seed( 390 )
    fnAnalyze <- .GetCommonExampleFunction( "MultiArmAnalysis", "AnalyzeMultiArmUsingLogrankTestBonferroni.R",
        "AnalyzeMultiArmUsingLogrankTestBonferroni" )
    dfData <- data.frame( ArrivalTime = 0, TreatmentID = rep( 0:1, each = 100 ),
        SurvivalTime = c( stats::rexp( 100, 1 ), stats::rexp( 100, 0.25 ) ) )
    lDesign <- list( NumTreatments = 1, IsArmPresent = 1, TailType = 0, Alpha = 0.025, MaxEvents = 100 )
    lLook <- list( NumLooks = 2, CurrLookIndex = 1, InfoFrac = c( 0.5, 1 ),
        EffBdryScale = 1, EffBdry = c( 1e-50, 0.025 ), RejType = 2 )
    lResult <- fnAnalyze( dfData, lDesign, lLook )
    expect_gt( lResult$AdjPVal, lLook$EffBdry[ 1 ] )
    expect_lt( lResult$AdjPVal, 0.5 )
    expect_identical( lResult$Decision, 0L )
} )

test_that( "MMRM sequential analyses compare p-values with stage rejection levels", {
    skip_if_not_installed( "nlme" )
    skip_if_not_installed( "rpact" )
    skip_if_not_installed( "dplyr" )
    skip_if_not_installed( "tidyr" )
    set.seed( 391 )
    dfData <- data.frame( ArrivalTime = 0, TreatmentID = rep( 0:1, each = 60 ),
        Response1 = stats::rnorm( 120 ), ArrTimeVisit1 = 0, ArrTimeVisit2 = 1 )
    vNoise <- stats::residuals( stats::lm( stats::rnorm( 120 ) ~ Response1 + TreatmentID, data = dfData ) )
    cNoiseModel <- stats::lm( vNoise ~ Response1 + TreatmentID, data = dfData )
    cDesign <- rpact::getDesignGroupSequential( kMax = 3, informationRates = c( 0.25, 0.6, 1 ),
        alpha = 0.025, sided = 1, typeOfDesign = "OF" )
    dTargetP <- mean( c( cDesign$stageLevels[ 3 ], 0.025 ) )
    dEffect <- stats::qt( 1 - dTargetP, stats::df.residual( cNoiseModel ) ) *
        summary( cNoiseModel )$coefficients[ "TreatmentID", "Std. Error" ]
    dfData$Response2 <- 0.7 * dfData$Response1 + dEffect * dfData$TreatmentID + vNoise
    lLook <- list( NumLooks = 3, CurrLookIndex = 3, CumCompleters = c( 30, 72, 120 ),
        InfoFrac = c( 0.25, 0.6, 1 ), InterimVisit = 2, IncludePipeline = 0, RejType = 0 )
    for ( strFunction in c( "AnalyzeUsingMMRM", "AnalyzeUsingMMRMWithGLS" ) ) {
        fnAnalyze <- .GetCommonExampleFunction( "SchizophreniaTrial", paste0( strFunction, ".R" ), strFunction )
        lResult <- fnAnalyze( dfData, list( TailType = 1, Alpha = 0.025 ), lLook )
        expect_identical( lResult$ErrorCode, 0L, info = strFunction )
        expect_equal( lResult$p.value, dTargetP, tolerance = 1e-10, info = strFunction )
        expect_lt( lResult$p.value, 0.025 )
        expect_gt( lResult$p.value, cDesign$stageLevels[ 3 ] )
        expect_identical( lResult$Decision, 0L, info = strFunction )
    }
} )

test_that( "survival analyses censor dropout and fixed follow-up before positioning the event look", {
    skip_if_not_installed( "survival" )
    set.seed( 392 )
    dfData <- data.frame( ArrivalTime = seq_len( 200 ) / 100, TreatmentID = rep( 0:1, each = 100 ),
        SurvivalTime = c( stats::rexp( 100, 1 ), stats::rexp( 100, 0.25 ) ), DropOutTime = Inf, TrueHR = 0.25 )
    vDropouts <- seq( 1, 100, by = 2 )
    dfData$DropOutTime[ vDropouts ] <- dfData$SurvivalTime[ vDropouts ] / 10
    lDesign <- list( TailType = 0, Alpha = 0.025, CriticalPoint = -1.96, MaxEvents = 50,
        FollowUpType = 1, FollowUpDur = 2, NumTreatments = 1, IsArmPresent = 1 )
    vEventTimes <- sort( ( dfData$ArrivalTime + dfData$SurvivalTime )[
        dfData$SurvivalTime <= pmin( dfData$DropOutTime, lDesign$FollowUpDur ) ] )
    dAnalysisTime <- vEventTimes[ lDesign$MaxEvents ]
    dfExpected <- dfData[ dfData$ArrivalTime <= dAnalysisTime, ]
    dfExpected$Event <- dfExpected$SurvivalTime <= dfExpected$DropOutTime &
        dfExpected$SurvivalTime <= lDesign$FollowUpDur &
        dfExpected$ArrivalTime + dfExpected$SurvivalTime <= dAnalysisTime
    dfExpected$Time <- pmin( dfExpected$SurvivalTime, dfExpected$DropOutTime,
        lDesign$FollowUpDur, dAnalysisTime - dfExpected$ArrivalTime )
    cCox <- survival::coxph( survival::Surv( Time, Event ) ~ factor( TreatmentID ), data = dfExpected )
    cLogrank <- survival::survdiff( survival::Surv( Time, Event ) ~ TreatmentID, data = dfExpected )
    dLogrankStatistic <- sqrt( cLogrank$chisq ) * sign( cLogrank$obs[ 2 ] - cLogrank$exp[ 2 ] )
    lCases <- list(
        list( "BayesianAssuranceTimeToEvent", "AnalyzeSurvivalDataUsingCoxPH", "cox" ),
        list( "ConsecutiveStudiesContinuousTimeToEvent", "AnalyzeSurvivalDataUsingCoxPH", "cox" ),
        list( "2ArmTimeToEventOutcomeAnalysis", "AnalyzeTTESSR", "logrank" ),
        list( "2ArmTimeToEventOutcomeAnalysis", "AnalyzeUsingSurvivalPackage", "logrank" ),
        list( "MultiArmAnalysis", "AnalyzeMultiArmUsingLogrankTestBonferroni", "multiarm" )
    )
    for ( lCase in lCases ) {
        fnAnalyze <- .GetCommonExampleFunction( lCase[[ 1 ]], paste0( lCase[[ 2 ]], ".R" ), lCase[[ 2 ]] )
        lResult <- fnAnalyze( dfData, lDesign )
        expect_identical( lResult$ErrorCode, 0L, info = lCase[[ 1 ]] )
        if ( lCase[[ 3 ]] == "cox" ) {
            expect_equal( lResult$TestStat, unname( summary( cCox )$coefficients[ , "z" ] ) )
        } else if ( lCase[[ 3 ]] == "logrank" ) {
            expect_equal( lResult$TestStat, dLogrankStatistic )
        } else {
            expect_equal( lResult$HR, unname( exp( stats::coef( cCox ) ) ) )
            expect_equal( lResult$RawPVal, stats::pnorm( dLogrankStatistic ) )
        }
        if ( !is.null( lResult$AnalysisTime ) ) {
            expect_equal( lResult$AnalysisTime, dAnalysisTime )
        }
        dfNoEvents <- dfData
        dfNoEvents$DropOutTime <- 0
        expect_identical( fnAnalyze( dfNoEvents, lDesign )$ErrorCode, 1L )
    }

    fnProbabilitySuccess <- .GetCommonExampleFunction( "ProbabilitySuccessDualEndpoints", "AnalyzePFSAndOS.R",
        "AnalyzePFSAndOS" )
    dfData$OS <- dfData$SurvivalTime + stats::rexp( 200, 1 )
    dfExpected$OS <- dfData$OS[ dfData$ArrivalTime <= dAnalysisTime ]
    dfExpected$OSEvent <- dfExpected$OS <= dfExpected$DropOutTime &
        dfExpected$OS <= lDesign$FollowUpDur & dfExpected$ArrivalTime + dfExpected$OS <= dAnalysisTime
    dfExpected$OSTime <- pmin( dfExpected$OS, dfExpected$DropOutTime,
        lDesign$FollowUpDur, dAnalysisTime - dfExpected$ArrivalTime )
    cOSCox <- survival::coxph( survival::Surv( OSTime, OSEvent ) ~ factor( TreatmentID ), data = dfExpected )
    lProbabilitySuccess <- fnProbabilitySuccess( dfData, lDesign,
        UserParam = list( HazardRatioCutoffIA = 1, HazardRatioCutoffFA = 1 ) )
    expect_identical( lProbabilitySuccess$ErrorCode, 0L )
    expect_equal( lProbabilitySuccess$dHazardRatioPFS, unname( exp( stats::coef( cCox ) ) ) )
    expect_equal( lProbabilitySuccess$dHazardRatioOS, unname( exp( stats::coef( cOSCox ) ) ) )
} )

test_that( "standard two-arm analyses accept the current-look event count as a scalar", {
    skip_if_not_installed( "survival" )
    set.seed( 393 )
    dfData <- data.frame( ArrivalTime = 0, TreatmentID = rep( 0:1, each = 100 ),
        SurvivalTime = c( stats::rexp( 100, 1 ), stats::rexp( 100, 0.25 ) ), TrueHR = 0.25,
        Region = rep( 1:2, 100 ) )
    lDesign <- list( TailType = 0, Alpha = 0.025, CriticalPoint = -1.96, MaxEvents = 100,
        TestStratFactors = "Region" )
    lScalar <- list( NumLooks = 2, CurrLookIndex = 2, CumEvents = 100,
        EffBdry = c( -3, -1.96 ), RejType = 2 )
    lVector <- lScalar
    lVector$CumEvents <- c( 50, 100 )
    for ( lCase in list( list( "2ArmTimeToEventOutcomeAnalysis", "AnalyzeStratification" ),
        list( "BayesianAssuranceTimeToEvent", "AnalyzeSurvivalDataUsingCoxPH" ),
        list( "ConsecutiveStudiesContinuousTimeToEvent", "AnalyzeSurvivalDataUsingCoxPH" ) ) ) {
        fnAnalyze <- .GetCommonExampleFunction( lCase[[ 1 ]], paste0( lCase[[ 2 ]], ".R" ), lCase[[ 2 ]] )
        lExpected <- fnAnalyze( dfData, lDesign, lVector )
        expect_identical( fnAnalyze( dfData, lDesign, lScalar ), lExpected, info = lCase[[ 1 ]] )
        expect_identical( lExpected$ErrorCode, 0L )
    }
} )
