######################################################################################################################## .
# Description: Regression checks for corrected example analysis data and boundary handling.
######################################################################################################################## .

test_that( "ordinary analyses preserve engine-supplied rows regardless of censor metadata", {
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
        dfData$Response <- if ( lCase[[ 1 ]] == "2ArmBinaryOutcomeAnalysis" ) {
            c( rep( 1, 10 ), rep( 0, 20 ), rep( 1, 25 ), rep( 0, 5 ) )
        } else {
            c( seq( -1, 1, length.out = 30 ), seq( 1, 3, length.out = 30 ) )
        }
        lDesign <- list( TailType = 1, CriticalPoint = 1.96, MaxCompleters = 60 )
        set.seed( 389 )
        lExpected <- fnAnalyze( dfData, lDesign, UserParam = lCase[[ 3 ]] )
        for ( strColumn in c( "CensorInd", "CensorIndOrg" ) ) {
            dfEngineData <- dfData
            dfEngineData[[ strColumn ]] <- rep( 0:1, 30 )
            set.seed( 389 )
            expect_equal( fnAnalyze( dfEngineData, lDesign, UserParam = lCase[[ 3 ]] ), lExpected,
                info = paste( lCase[[ 2 ]], strColumn ) )
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
        Response1 = stats::rnorm( 120 ), ArrTimeVisit1 = 0, ArrTimeVisit2 = 1, ArrTimeVisit3 = 2 )
    vNoise2 <- stats::rnorm( 120 )
    vNoise3 <- stats::rnorm( 120 )
    cDesign <- rpact::getDesignGroupSequential( kMax = 3, informationRates = c( 0.25, 0.6, 1 ),
        alpha = 0.025, sided = 1, typeOfDesign = "OF" )
    dTargetP <- mean( c( cDesign$stageLevels[ 3 ], 0.025 ) )
    lLook <- list( NumLooks = 3, CurrLookIndex = 3, CumCompleters = c( 30, 72, 120 ),
        InfoFrac = c( 0.25, 0.6, 1 ), InterimVisit = 3, IncludePipeline = 0, RejType = 0 )
    for ( strFunction in c( "AnalyzeUsingMMRM", "AnalyzeUsingMMRMWithGLS" ) ) {
        fnAnalyze <- .GetCommonExampleFunction( "SchizophreniaTrial", paste0( strFunction, ".R" ), strFunction )
        fnAtEffect <- function( dEffect ) {
            dfData$Response2 <- 0.7 * dfData$Response1 + dEffect * dfData$TreatmentID + vNoise2
            dfData$Response3 <- 0.7 * dfData$Response1 + dEffect * dfData$TreatmentID + vNoise3
            fnAnalyze( dfData, list( TailType = 1, Alpha = 0.025 ), lLook )
        }
        dEffect <- stats::uniroot( function( dEffect ) fnAtEffect( dEffect )$p.value - dTargetP,
            c( 0, 2 ), tol = 1e-9 )$root
        lResult <- fnAtEffect( dEffect )
        expect_identical( lResult$ErrorCode, 0L, info = strFunction )
        expect_equal( lResult$p.value, dTargetP, tolerance = 1e-7, info = strFunction )
        expect_lt( lResult$p.value, 0.025 )
        expect_gt( lResult$p.value, cDesign$stageLevels[ 3 ] )
        expect_identical( lResult$Decision, 0L, info = strFunction )
    }
} )

test_that( "survival analyses preserve engine-supplied outcomes without recensoring", {
    skip_if_not_installed( "survival" )
    set.seed( 392 )
    dfData <- data.frame( ArrivalTime = rep( 0, 200 ), TreatmentID = rep( 0:1, each = 100 ),
        SurvivalTime = c( stats::rexp( 100, 1 ), stats::rexp( 100, 0.25 ) ), TrueHR = 0.25 )
    lDesign <- list( TailType = 0, Alpha = 0.025, CriticalPoint = -1.96, MaxEvents = 100,
        NumTreatments = 1, IsArmPresent = 1 )
    lCases <- list(
        list( "BayesianAssuranceTimeToEvent", "AnalyzeSurvivalDataUsingCoxPH" ),
        list( "ConsecutiveStudiesContinuousTimeToEvent", "AnalyzeSurvivalDataUsingCoxPH" ),
        list( "2ArmTimeToEventOutcomeAnalysis", "AnalyzeTTESSR" ),
        list( "2ArmTimeToEventOutcomeAnalysis", "AnalyzeUsingSurvivalPackage" ),
        list( "MultiArmAnalysis", "AnalyzeMultiArmUsingLogrankTestBonferroni" )
    )
    dfEngineData <- dfData
    dfEngineData$DropOutTime <- 0
    lEngineDesign <- lDesign
    lEngineDesign$FollowUpType <- 1
    lEngineDesign$FollowUpDur <- 0.01
    for ( lCase in lCases ) {
        fnAnalyze <- .GetCommonExampleFunction( lCase[[ 1 ]], paste0( lCase[[ 2 ]], ".R" ), lCase[[ 2 ]] )
        lExpected <- fnAnalyze( dfData, lDesign )
        expect_identical( lExpected$ErrorCode, 0L )
        expect_equal( fnAnalyze( dfEngineData, lEngineDesign ), lExpected, info = lCase[[ 2 ]] )
    }
    fnAnalyze <- .GetCommonExampleFunction( "ProbabilitySuccessDualEndpoints", "AnalyzePFSAndOS.R",
        "AnalyzePFSAndOS" )
    dfData$OS <- dfData$SurvivalTime + stats::rexp( 200, rate = 5 )
    dfEngineData$OS <- dfData$OS
    lUser <- list( HazardRatioCutoffIA = 1, HazardRatioCutoffFA = 1 )
    expect_equal( fnAnalyze( dfEngineData, lEngineDesign, UserParam = lUser ),
        fnAnalyze( dfData, lDesign, UserParam = lUser ) )
} )
