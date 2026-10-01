#################################################################################################### .
# Description: Regression checks for analysis integration-point contracts and interim data limits.
#################################################################################################### .

test_that( "mean confidence-interval analyses can stop for interim futility", {
    fnAnalyze <- .GetCommonExampleFunction( "2ArmNormalOutcomeAnalysis", "AnalyzeUsingMeanLimitsOfCI.R",
        "AnalyzeUsingMeanLimitsOfCI" )
    dfData <- data.frame( TreatmentID = rep( 0:1, each = 30 ),
        Response = c( seq( -1, 1, length.out = 30 ), seq( -2, 0, length.out = 30 ) ) )
    lDesign <- list( TailType = 1 )
    lLook <- list( NumLooks = 2, CurrLookIndex = 1, CumCompleters = c( 60, 120 ), RejType = 4 )
    lUser <- list( dMAV = 0, dTV = 0.5, dConfLevel = 0.9 )
    lExpected <- stats::t.test( dfData$Response[ dfData$TreatmentID == 1 ],
        dfData$Response[ dfData$TreatmentID == 0 ], alternative = "greater", var.equal = TRUE, conf.level = 0.9 )
    lResult <- fnAnalyze( dfData, lDesign, lLook, lUser )
    expect_identical( lResult$Decision, 3L )
    expect_equal( lResult$TestStat, unname( lExpected$conf.int[ 1 ] ) )
    lTwoSided <- stats::t.test( dfData$Response[ dfData$TreatmentID == 1 ],
        dfData$Response[ dfData$TreatmentID == 0 ], var.equal = TRUE, conf.level = 0.9 )
    lUser$dMAV <- mean( c( lExpected$conf.int[ 1 ], lTwoSided$conf.int[ 1 ] ) )
    lEfficacy <- fnAnalyze( dfData, lDesign, lLook, lUser )
    expect_identical( lEfficacy$Decision, 2L )
} )

test_that( "hazard-ratio confidence limits compare thresholds on the same scale", {
    skip_if_not_installed( "survival" )
    set.seed( 389 )
    fnAnalyze <- .GetCommonExampleFunction( "2ArmTimeToEventOutcomeAnalysis",
        "AnalyzeUsingHazardRatioLimitsOfCI.R", "AnalyzeUsingHazardRatioLimitsOfCI" )
    dfData <- data.frame( ArrivalTime = 0, TreatmentID = rep( 0:1, each = 100 ),
        SurvivalTime = stats::rexp( 200 ) )
    lDesign <- list( MaxEvents = 100, TailType = 0 )
    lLook <- list( NumLooks = 2, CurrLookIndex = 1, InfoFrac = c( 0.8, 1 ), RejType = 5 )
    lResult <- fnAnalyze( dfData, lDesign, lLook,
        list( dMAV = 0.001, dTV = 0.01, dConfLevel = 0.9 ) )
    expect_identical( lResult$Decision, 3L )
    expect_identical( lResult$ErrorCode, 0L )
    expect_identical( fnAnalyze( dfData, lDesign )$ErrorCode, -1L )
} )

test_that( "dose-finding analyses use native response lag and current-look proof of concept", {
    fnAnalyze <- .GetCommonExampleFunction( "DoseFindingAnalysis",
        "AnalyzeDoseFindingContinuousUsingPairwiseTTest.R", "AnalyzeDoseFindingContinuousUsingPairwiseTTest" )
    dfData <- data.frame( ArrivalTime = seq_len( 120 ), TreatmentID = rep( 0:2, 40 ),
        CensorIndOrg = c( rep( 0, 6 ), rep( 1, 114 ) ) )
    dfData$ClndrRespTime <- dfData$ArrivalTime + 10
    dfData$Response <- dfData$TreatmentID * 1.5 + rep( seq( -0.2, 0.2, length.out = 40 ), each = 3 )
    lDesign <- list( NumTreatments = 2, Alpha = 0.025, TailType = 1, VarType = 4,
        IsArmPresent = c( 1, 1 ), RespLag = 10, MaxCompleters = 114 )
    lLook <- list( NumLooks = 2, CurrLookIndex = 1, CumCompleters = c( 90, 114 ),
        FutBdry = c( NA_real_, NA_real_ ), PoCThreshold = c( 0.1, 0.2 ) )
    lResult <- fnAnalyze( dfData, lDesign, lLook )
    expect_identical( lResult$ErrorCode, 0L )
    expect_equal( lResult$AnalysisTime, 106 )
    expect_identical( lResult$POCStatusArm, c( 1L, 1L ) )
    lFixed <- fnAnalyze( dfData, lDesign )
    expect_identical( lFixed$ErrorCode, 0L )
    expect_equal( lFixed$AnalysisTime, 130 )
    expect_identical( lFixed$Decision, c( 2L, 2L ) )
} )

test_that( "dual-endpoint Fisher analyses exclude pending responses and retain outcome levels", {
    fnAnalyze <- .GetCommonExampleFunction( "DEPAnalysis", "AnalyzeDEPUsingFisherExact.R",
        "AnalyzeDEPUsingFisherExact" )
    dfData <- data.frame( ArrivalTime = 0, TreatmentID = rep( 0:1, each = 20 ),
        Response1 = seq_len( 40 ), Response2 = c( rep( 1, 10 ), rep( 0, 10 ), rep( 1, 2 ), rep( 0, 18 ) ),
        ClndrRespTime = seq_len( 40 ), ClndrRespTime2 = c( seq_len( 10 ), rep( 1000, 10 ), seq_len( 20 ) ),
        CensorIndOrg = 1, CensorIndOrg2 = 1 )
    lDesign <- list( EndpointName = c( "TTE", "Binary" ), EndpointType = c( 2, 1 ),
        TailType = c( 0, 0 ), PlanEndTrial = 2, SampleSize = 40, MaxEvents = list( TTE = 20, Binary = NA ), MaxCompleters = list( TTE = NA, Binary = 20 ) )
    lResult <- fnAnalyze( dfData, lDesign )
    lExpected <- stats::fisher.test( matrix( c( 0, 10, 18, 2 ), nrow = 2, byrow = TRUE ),
        alternative = "less" )
    expect_equal( lResult$Delta, -0.9 )
    expect_equal( lResult$TestStat, stats::qnorm( lExpected$p.value ) )
    expect_lt( lResult$TestStat, 0 )
    dfData$Response2 <- 1
    lConstant <- fnAnalyze( dfData, lDesign )
    expect_identical( lConstant$ErrorCode, 0L )
    expect_equal( lConstant$Delta, 0 )
} )

test_that( "Schizophrenia interim datasets contain only observed visit values", {
    skip_if_not_installed( "dplyr" )
    skip_if_not_installed( "tidyr" )
    dfData <- data.frame( ArrivalTime = c( 0, 1, 2, 3 ), TreatmentID = rep( 0:1, 2 ) )
    for ( nVisit in seq_len( 5 ) ) {
        dfData[[ paste0( "ArrTimeVisit", nVisit ) ]] <- nVisit - 1
        dfData[[ paste0( "Response", nVisit ) ]] <- seq_len( 4 ) + nVisit
    }
    lLook <- list( NumLooks = 2, CurrLookIndex = 1, CumCompleters = c( 2, 4 ),
        InterimVisit = 2, IncludePipeline = 0 )
    for ( strFile in c( "AnalyzeUsingMMRM.R", "AnalyzeUsingMMRMWithGLS.R" ) ) {
        fnDataset <- .GetCommonExampleFunction( "SchizophreniaTrial", strFile, "CreateAnalysisDataset" )
        dfInterim <- fnDataset( dfData, lLook )
        expect_true( all( dfInterim$CalendarVisitTime <= 2 ) )
        expect_equal( nrow( dfInterim ), 3 )
        lLook$CurrLookIndex <- 2
        dfFinal <- fnDataset( dfData, lLook )
        expect_equal( nrow( dfFinal ), 16 )
        lLook$CurrLookIndex <- 1
    }
} )

test_that( "multi-arm survival comparisons use one-sided p-values and the actual arm contrast", {
    skip_if_not_installed( "survival" )
    set.seed( 390 )
    fnAnalyze <- .GetCommonExampleFunction( "MultiArmAnalysis", "AnalyzeMultiArmUsingLogrankTestBonferroni.R",
        "AnalyzeMultiArmUsingLogrankTestBonferroni" )
    dfData <- data.frame( ArrivalTime = 0, TreatmentID = rep( c( 0, 2 ), each = 100 ),
        SurvivalTime = c( stats::rexp( 100, 1 ), stats::rexp( 100, 0.25 ) ) )
    lDesign <- list( NumTreatments = 2, IsArmPresent = c( 0, 1 ), TailType = 0, Alpha = 0.025,
        MaxEvents = 100 )
    lResult <- fnAnalyze( dfData, lDesign )
    dfData$Time <- pmin( dfData$SurvivalTime, lResult$AnalysisTime )
    dfData$Event <- dfData$SurvivalTime <= lResult$AnalysisTime
    cCox <- survival::coxph( survival::Surv( Time, Event ) ~ factor( TreatmentID ), data = dfData )
    cLogrank <- survival::survdiff( survival::Surv( Time, Event ) ~ TreatmentID, data = dfData )
    expect_equal( lResult$HR[ 2 ], unname( exp( stats::coef( cCox ) ) ) )
    expect_equal( lResult$RawPVal[ 2 ], stats::pnorm( -sqrt( cLogrank$chisq ) ) )
    expect_identical( lResult$Decision[ 2 ], 1L )
    expect_true( is.na( lResult$Decision[ 1 ] ) )
} )

test_that( "isotonic dose effects weight previously pooled blocks by their sizes", {
    fnIsotonic <- .GetCommonExampleFunction( "DoseFindingAnalysis",
        "AnalyzeDoseFindingContinuousUsingPairwiseTTest.R", "ComputeIsotonicDeltas" )
    vEffects <- c( 3, 2, 1, 5, 4 )
    expect_equal( fnIsotonic( vEffects, 1L ), stats::isoreg( vEffects )$yf )
    expect_equal( fnIsotonic( -vEffects, 0L ), -stats::isoreg( vEffects )$yf )
} )

test_that( "MMRM analyses apply the configured one-sided direction", {
    skip_if_not_installed( "nlme" )
    skip_if_not_installed( "dplyr" )
    skip_if_not_installed( "tidyr" )
    set.seed( 391 )
    dfData <- data.frame( ArrivalTime = 0, TreatmentID = rep( 0:1, each = 60 ),
        Response1 = stats::rnorm( 120 ) )
    for ( nVisit in 2:5 ) {
        dfData[[ paste0( "Response", nVisit ) ]] <- 0.7 * dfData$Response1 +
            3 * dfData$TreatmentID + stats::rnorm( 120 )
    }
    for ( nVisit in seq_len( 5 ) ) {
        dfData[[ paste0( "ArrTimeVisit", nVisit ) ]] <- nVisit - 1
    }
    for ( strFunction in c( "AnalyzeUsingMMRM", "AnalyzeUsingMMRMWithGLS" ) ) {
        fnAnalyze <- .GetCommonExampleFunction( "SchizophreniaTrial", paste0( strFunction, ".R" ),
            strFunction )
        lLeft <- fnAnalyze( dfData, list( TailType = 0, Alpha = 0.025 ) )
        lRight <- fnAnalyze( dfData, list( TailType = 1, Alpha = 0.025 ) )
        expect_identical( lLeft$ErrorCode, 0L )
        expect_identical( lRight$ErrorCode, 0L )
        expect_identical( lLeft$Decision, 0L )
        expect_identical( lRight$Decision, 2L )
        expect_gt( lLeft$p.value, 0.99 )
        expect_lt( lRight$p.value, 0.01 )
        expect_equal( lLeft$p.value + lRight$p.value, 1 )
    }
} )

test_that( "conditional-power analysis honors the specified final efficacy boundary", {
    skip_if_not_installed( "survival" )
    set.seed( 392 )
    fnAnalyze <- .GetCommonExampleFunction( "TimeToEventConditionalPowerFutilityAnalysis",
        "AnalyzeTTEWithConditionalPowerFutility.R", "AnalyzeTTEWithConditionalPowerFutility" )
    dfData <- data.frame( ArrivalTime = 0, TreatmentID = rep( 0:1, each = 100 ),
        SurvivalTime = c( stats::rexp( 100, 1 ), stats::rexp( 100, 0.25 ) ) )
    lDesign <- list( TailType = 0, Alpha = 0.025, AllocInfo = 1, MaxEvents = 100,
        CriticalPoint = -100 )
    lStrict <- fnAnalyze( dfData, lDesign )
    expect_identical( lStrict$Decision, 0L )
    lDesign$CriticalPoint <- 0
    lPermissive <- fnAnalyze( dfData, lDesign )
    expect_identical( lPermissive$Decision, 1L )
    lLook <- list( NumLooks = 2, CurrLookIndex = 2, InfoFrac = c( 0.5, 1 ),
        EffBdry = c( -3, -100 ), RejType = 2 )
    lSequential <- fnAnalyze( dfData, lDesign, lLook )
    expect_identical( lSequential$Decision, 0L )
    expect_equal( lSequential$dConditionalPower, -1 )
} )

test_that( "MMRM interim analyses reduce to ANCOVA with one observed follow-up", {
    skip_if_not_installed( "nlme" )
    skip_if_not_installed( "dplyr" )
    skip_if_not_installed( "tidyr" )
    skip_if_not_installed( "rpact" )
    set.seed( 393 )
    dfData <- data.frame( ArrivalTime = 0, TreatmentID = rep( 0:1, each = 60 ),
        Response1 = stats::rnorm( 120 ) )
    dfData$Response2 <- 0.7 * dfData$Response1 + 3 * dfData$TreatmentID + stats::rnorm( 120 )
    for ( nVisit in 3:5 ) {
        # Future visits have a large effect in the opposite direction and must be excluded.
        dfData[[ paste0( "Response", nVisit ) ]] <- -1000 * dfData$TreatmentID + stats::rnorm( 120 )
    }
    for ( nVisit in seq_len( 5 ) ) {
        dfData[[ paste0( "ArrTimeVisit", nVisit ) ]] <- nVisit - 1
    }
    lLook <- list( NumLooks = 2, CurrLookIndex = 1, CumCompleters = c( 60, 120 ),
        InterimVisit = 2, IncludePipeline = 0, RejType = 0 )
    cExpected <- stats::lm( Response2 ~ Response1 + TreatmentID, data = dfData )
    mExpected <- summary( cExpected )$coefficients
    dExpectedRight <- stats::pt( mExpected[ "TreatmentID", "t value" ], stats::df.residual( cExpected ),
        lower.tail = FALSE )
    for ( strFunction in c( "AnalyzeUsingMMRM", "AnalyzeUsingMMRMWithGLS" ) ) {
        fnAnalyze <- .GetCommonExampleFunction( "SchizophreniaTrial", paste0( strFunction, ".R" ),
            strFunction )
        lRight <- fnAnalyze( dfData, list( TailType = 1, Alpha = 0.025 ), lLook )
        expect_identical( lRight$ErrorCode, 0L )
        expect_identical( lRight$Decision, 2L )
        expect_equal( lRight$PrimDelta, unname( stats::coef( cExpected )[ "TreatmentID" ] ) )
        expect_equal( lRight$p.value, dExpectedRight, tolerance = 1e-10 )
        lLook$RejType <- 2
        lLeft <- fnAnalyze( dfData, list( TailType = 0, Alpha = 0.025 ), lLook )
        expect_identical( lLeft$ErrorCode, 0L )
        expect_identical( lLeft$Decision, 0L )
        expect_equal( lLeft$p.value, 1 - dExpectedRight )
        lLook$RejType <- 0
    }
} )

test_that( "MMRM reports a failed model fit as a nonfatal simulation error", {
    skip_if_not_installed( "nlme" )
    skip_if_not_installed( "dplyr" )
    skip_if_not_installed( "tidyr" )
    fnAnalyze <- .GetCommonExampleFunction( "SchizophreniaTrial", "AnalyzeUsingMMRM.R",
        "AnalyzeUsingMMRM" )
    dfData <- data.frame( ArrivalTime = 0, TreatmentID = 0:1,
        Response1 = c( 1, 2 ), Response2 = c( 2, 4 ), ArrTimeVisit1 = 0, ArrTimeVisit2 = 1 )
    lResult <- suppressWarnings( fnAnalyze( dfData, list( TailType = 1, Alpha = 0.025 ) ) )
    expect_identical( lResult$ErrorCode, 1L )
    expect_true( is.na( lResult$PrimDelta ) )
    expect_equal( lResult$p.value, 1 )
} )
