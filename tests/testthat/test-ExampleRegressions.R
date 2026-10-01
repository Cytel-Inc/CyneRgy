#################################################################################################### .
#   Description: Regression checks for example calculations and integration-point outputs.
#################################################################################################### .

test_that( "binary analyses compare experimental and control proportions with unequal arm sizes", {
    dfData <- data.frame(
        TreatmentID = c( rep( 0, 20 ), rep( 1, 40 ) ),
        Response = c( rep( 1, 8 ), rep( 0, 12 ), rep( 1, 32 ), rep( 0, 8 ) )
    )
    lDesign <- list( TailType = 1, CriticalPoint = 1.96, MaxCompleters = 60 )
    lExpected <- stats::prop.test( c( 32, 8 ), c( 40, 20 ),
        alternative = "greater", correct = FALSE )
    lResult <- AnalyzeUsingPropTest( dfData, lDesign )
    expect_equal( lResult$TestStat, stats::qnorm( 1 - lExpected$p.value ) )
    expect_identical( lResult$Decision, 2L )

    lExpectedCI <- stats::prop.test( c( 32, 8 ), c( 40, 20 ), correct = FALSE,
        conf.level = 0.9 )
    lResultCI <- AnalyzeUsingPropLimitsOfCI( dfData, lDesign,
        UserParam = list( dConfLevel = 0.9, dLowerLimit = 0.1, dUpperLimit = 0 ) )
    expect_equal( lResultCI$Delta, 0.4 )
    expect_equal( lResultCI$TestStat, unname( lExpectedCI$conf.int[ 1 ] ) )
} )

test_that( "treatment selection supports one experimental arm and constant binary outcomes", {
    vFunctions <- c( "SelectExpThatAreBetterThanCtrl", "SelectExpUsingBayesianRule",
        "SelectExpWithPValueLessThanSpecified", "SelectSpecifiedNumberOfExpWithHighestResponses" )
    for ( strFunction in vFunctions ) {
        fnSelect <- .GetCommonExampleFunction( "TreatmentSelection", paste0( strFunction, ".R" ),
            strFunction )
        for ( nResponse in 0:1 ) {
            dfData <- data.frame( TreatmentID = rep( 0:1, each = 20 ), Response = nResponse )
            # A constant outcome has no chi-squared information; the example falls back to an arm.
            lResult <- suppressWarnings( fnSelect( dfData, list(), list() ) )
            expect_identical( lResult$ErrorCode, 0L, info = strFunction )
            expect_identical( lResult$TreatmentID, 1L, info = strFunction )
            expect_length( lResult$AllocRatio, 1 )
        }
    }
} )

test_that( "stratified survival responses retain the original subject and arm order", {
    set.seed( 94 )
    dfSubjects <- expand.grid( TreatmentID = 0:1, StratumID = 2:1, Replicate = seq_len( 2000 ) )
    mHazards <- matrix( c( 0.2, 2, 2, 0.2 ), nrow = 2, byrow = TRUE )
    fnGenerate <- .GetCommonExampleFunction( "2ArmTimeToEventOutcomePatientSimulation",
        "SimulatePatientOutcomeStratification.R", "SimulatePatientOutcomeStratification" )
    lResult <- fnGenerate( nrow( dfSubjects ), 2, rep( 0, nrow( dfSubjects ) ),
        dfSubjects$TreatmentID, dfSubjects$StratumID, 1, 1, 0, mHazards )
    expect_identical( lResult$ErrorCode, 0L )
    for ( nStratum in 1:2 ) {
        for ( nArm in 0:1 ) {
            vResponses <- lResult$SurvivalTime[ dfSubjects$StratumID == nStratum &
                dfSubjects$TreatmentID == nArm ]
            expect_equal( mean( vResponses ) * mHazards[ nStratum, nArm + 1 ], 1, tolerance = 0.06 )
        }
    }
} )

test_that( "survival assurance accepts the same mixture sampling weights as continuous assurance", {
    set.seed( 95 )
    fnGenerate <- .GetCommonExampleFunction( "BayesianAssuranceTimeToEvent",
        "SimulatePatientSurvivalAssurance.R", "SimulatePatientSurvivalAssurance" )
    lUser <- list( dWeight1 = 2, dWeight2 = 0, dPriorMean = -0.7, dPriorSD = 0,
        dAlpha = 2, dBeta = 2, dUpper = 0, dLower = -1, dMeanTTECtrl = 1 )
    lResult <- fnGenerate( 20, 2, rep( 0, 20 ), rep( 0:1, 10 ), 1, 1, 0,
        matrix( c( 1, 1 ), nrow = 1 ), lUser )
    expect_identical( lResult$ErrorCode, 0L )
    expect_equal( lResult$TrueHR, rep( exp( -0.7 ), 20 ) )
} )

test_that( "assurance generators report treatment effects when the control mean is nonzero", {
    vExamples <- c( "BayesianAssuranceContinuous", "ConsecutiveStudiesContinuous",
        "ConsecutiveStudiesContinuousTimeToEvent" )
    lUser <- list( dWeight1 = 1, dWeight2 = 0, dMean1 = 0.5, dMean2 = 1,
        dSD1 = 0, dSD2 = 0, dMeanCtrl = 10, dSDCtrl = 0, dSDExp = 0 )
    for ( strExample in vExamples ) {
        fnGenerate <- .GetCommonExampleFunction( strExample, "SimulatePatientOutcomeNormalAssurance.R",
            "SimulatePatientOutcomeNormalAssurance" )
        lResult <- fnGenerate( 4, rep( 0, 4 ), c( 0, 1, 0, 1 ), c( 10, 11 ), c( 0, 0 ), lUser )
        expect_identical( lResult$ErrorCode, 0L, info = strExample )
        expect_equal( lResult$Response, c( 10, 10.5, 10, 10.5 ), info = strExample )
        expect_equal( lResult$vTrueDelta, rep( 0.5, 4 ), info = strExample )
        expect_identical( lResult$Delta, lResult$vTrueDelta, info = strExample )
        expect_equal( lResult$dSimMeanCtrl, rep( 10, 4 ), info = strExample )
        expect_equal( lResult$dSimMeanExp, rep( 10.5, 4 ), info = strExample )

        fnAnalyze <- .GetCommonExampleFunction( strExample, "AnalyzeUsingBayesianNormals.R",
            "AnalyzeUsingBayesianNormals" )
        dfGenerated <- data.frame( lResult[ setdiff( names( lResult ), "ErrorCode" ) ],
            TreatmentID = c( 0, 1, 0, 1 ) )
        lAnalysisUser <- list( dPriorMeanCtrl = 10, dPriorStdDevCtrl = 1,
            dPriorMeanExp = 10.5, dPriorStdDevExp = 1, dSigma = 1,
            dMAV = 0, dPU = 0.8, dPUFutility = 0.8 )
        lAnalyzed <- fnAnalyze( dfGenerated, list( TailType = 1 ), UserParam = lAnalysisUser )
        expect_identical( lAnalyzed$ErrorCode, 0L, info = strExample )
        expect_equal( lAnalyzed$dSimMeanCtrl, 10, info = strExample )
        expect_equal( lAnalyzed$dSimMeanExp, 10.5, info = strExample )

        fnFromPrior <- .GetCommonExampleFunction( strExample, "SimulatePatientOutcomeNormalAssurance.R",
            "SimulatePatientOutcomeNormalAssuranceUsingPriorInput" )
        envPrior <- environment( fnFromPrior )
        envPrior$vPrior <- c( 10.75, 11 )
        envPrior$nSimIndex <- 1
        lFromPrior <- fnFromPrior( 4, rep( 0, 4 ), c( 0, 1, 0, 1 ), c( 10, 11 ), c( 0, 0 ), lUser )
        expect_equal( lFromPrior$Delta, rep( 0.75, 4 ), info = strExample )
        expect_equal( lFromPrior$dSimMeanCtrl, rep( 10, 4 ), info = strExample )
        expect_equal( lFromPrior$dSimMeanExp, rep( 10.75, 4 ), info = strExample )
        expect_equal( envPrior$nSimIndex, 2, info = strExample )
    }
} )

test_that( "binary and normal SSR analyses honor lower efficacy boundaries without adaptation", {
    fnBinary <- .GetCommonExampleFunction( "2ArmBinaryOutcomeAnalysis", "AnalyzeBinarySSR.R",
        "AnalyzeBinarySSR" )
    fnNormal <- .GetCommonExampleFunction( "2ArmNormalOutcomeAnalysis", "AnalyzeNormalSSR.R",
        "AnalyzeNormalSSR" )
    dfData <- data.frame( TreatmentID = rep( 0:1, each = 50 ), ArrivalTime = seq_len( 100 ) / 10 )
    dfBinary <- dfData
    dfBinary$Response <- c( rep( 1, 45 ), rep( 0, 5 ), rep( 1, 5 ), rep( 0, 45 ) )
    dfNormal <- dfData
    dfNormal$Response <- c( seq( 2, 4, length.out = 50 ), seq( -1, 1, length.out = 50 ) )
    lDesign <- list( TailType = 0, CriticalPoint = -1.96, MaxCompleters = 100, RespLag = 2 )
    for ( lCase in list( list( fnBinary, dfBinary ), list( fnNormal, dfNormal ) ) ) {
        lResult <- lCase[[ 1 ]]( lCase[[ 2 ]], lDesign )
        expect_identical( lResult$ErrorCode, 0L )
        expect_lt( lResult$TestStat, -1.96 )
        expect_identical( lResult$Decision, 1L )
        expect_identical( lResult$ReEstCompleters, 100L )
    }
} )

test_that( "SSR completer analyses exclude pending responses and count actual completers", {
    fnAnalyze <- .GetCommonExampleFunction( "2ArmNormalOutcomeAnalysis", "AnalyzeNormalSSR.R",
        "AnalyzeNormalSSR" )
    dfData <- data.frame( TreatmentID = rep( 0:1, 12 ), ArrivalTime = seq_len( 24 ),
        Response = c( seq_len( 12 ), rep( 1000, 12 ) ), CensorInd = c( 0, rep( 1, 23 ) ) )
    lDesign <- list( TailType = 1, CriticalPoint = 1.96, MaxCompleters = 20, RespLag = 100 )
    lLook <- list( NumLooks = 2, CurrLookIndex = 1, InfoFrac = c( 0.5, 1 ),
        EffBdry = c( 2.5, 1.96 ), RejType = 0 )
    lResult <- fnAnalyze( dfData, lDesign, LookInfo = lLook )
    expect_equal( lResult$AnalysisTime, 111 )
    expect_equal( lResult$Delta, -1 )
    expect_identical( lResult$ReEstCompleters, 20L )

    lAdapt <- list( SSRFuncScale = 0, PromZoneMin = 0, PromZoneMax = 1,
        MaxSSMultInp = list( MaxSSMult = 1.5 ) )
    lAdapted <- fnAnalyze( dfData, lDesign, LookInfo = lLook, AdaptInfo = lAdapt )
    expect_identical( lAdapted$ReEstCompleters, 30L )
} )

test_that( "survival SSR uses a signed logrank statistic and the native sample size multiplier", {
    skip_if_not_installed( "survival" )
    set.seed( 91 )
    dfData <- data.frame( TreatmentID = rep( 0:1, each = 100 ), ArrivalTime = 0,
        SurvivalTime = c( stats::rexp( 100, 1 ), stats::rexp( 100, 0.25 ) ) )
    fnAnalyze <- .GetCommonExampleFunction( "2ArmTimeToEventOutcomeAnalysis", "AnalyzeTTESSR.R",
        "AnalyzeTTESSR" )
    lDesign <- list( TailType = 0, CriticalPoint = -1.96, MaxEvents = 100 )
    lResult <- fnAnalyze( dfData, lDesign )
    dfObserved <- dfData
    dfObserved$Event <- dfObserved$SurvivalTime <= lResult$AnalysisTime
    dfObserved$Time <- pmin( dfObserved$SurvivalTime, lResult$AnalysisTime )
    cExpected <- survival::survdiff( survival::Surv( Time, Event ) ~ TreatmentID, data = dfObserved )
    expect_equal( lResult$TestStat, -sqrt( cExpected$chisq ) )
    expect_identical( lResult$Decision, 1L )
    expect_identical( lResult$ReEstEvents, 100L )

    lLook <- list( NumLooks = 2, CurrLookIndex = 1, InfoFrac = c( 0.5, 1 ),
        EffBdry = c( -3, -1.96 ), RejType = 2 )
    lAdapt <- list( SSRFuncScale = 0, PromZoneMin = 0, PromZoneMax = 1,
        MaxSSMultInp = list( MaxSSMult = 1.5 ) )
    lAdapted <- fnAnalyze( dfData, lDesign, LookInfo = lLook, AdaptInfo = lAdapt )
    expect_identical( lAdapted$ReEstEvents, 150L )

    fnStratified <- .GetCommonExampleFunction( "2ArmTimeToEventOutcomeAnalysis", "AnalyzeStratification.R",
        "AnalyzeStratification" )
    dfData$Region <- rep( 1:2, 100 )
    lDesign$TestStratFactors <- "Region"
    lStratified <- fnStratified( dfData, lDesign )
    expect_lt( lStratified$TestStat, -1.96 )
    expect_identical( lStratified$Decision, 1L )
} )

test_that( "Cox assurance analyses support partial optional parameters and sequential boundaries", {
    skip_if_not_installed( "survival" )
    set.seed( 92 )
    dfData <- data.frame( TreatmentID = rep( 0:1, each = 100 ), ArrivalTime = 0,
        SurvivalTime = c( stats::rexp( 100, 1 ), stats::rexp( 100, 0.25 ) ), TrueHR = 0.25 )
    lDesign <- list( TailType = 0, Alpha = 0.025, MaxEvents = 100 )
    lLook <- list( NumLooks = 2, CurrLookIndex = 1, CumEvents = c( 100, 150 ),
        EffBdry = c( -20, -1.96 ), RejType = 2 )
    for ( strExample in c( "BayesianAssuranceTimeToEvent", "ConsecutiveStudiesContinuousTimeToEvent" ) ) {
        fnAnalyze <- .GetCommonExampleFunction( strExample, "AnalyzeSurvivalDataUsingCoxPH.R",
            "AnalyzeSurvivalDataUsingCoxPH" )
        lResult <- fnAnalyze( dfData, lDesign, UserParam = list() )
        expect_identical( lResult$Decision, 1L, info = strExample )
        expect_equal( lResult$TrueHR, 0.25, info = strExample )
        lInterim <- fnAnalyze( dfData, lDesign, LookInfo = lLook,
            UserParam = list( bReturnNAForNoGoTrials = TRUE ) )
        expect_identical( lInterim$Decision, 0L, info = strExample )
        expect_true( is.na( lInterim$TrueHR ), info = strExample )
        lPValueLook <- lLook
        lPValueLook$EffBdryScale <- 1
        lPValueLook$EffBdry <- c( 1e-50, 0.025 )
        lPValueInterim <- fnAnalyze( dfData, lDesign, LookInfo = lPValueLook )
        expect_identical( lPValueInterim$Decision, 0L, info = strExample )
    }
} )

test_that( "repeated-measures analysis preserves subject baselines with two, three, or five visits", {
    skip_if_not_installed( "nlme" )
    set.seed( 93 )
    fnAnalyze <- .GetCommonExampleFunction( "2ArmNormalRepeatedMeasuresResponseGeneration",
        "RepeatedMeasuresAnalysisUsingRPackage.R", "MMRMAna" )
    for ( nVisits in c( 2, 3, 5 ) ) {
        dfData <- data.frame( TreatmentID = rep( 0:1, each = 50 ), Response1 = stats::rnorm( 100 ) )
        for ( nVisit in 2:nVisits ) {
            dfData[[ paste0( "Response", nVisit ) ]] <- 0.8 * dfData$Response1 +
                2 * dfData$TreatmentID + stats::rnorm( 100 )
        }
        lResult <- fnAnalyze( dfData, list( NumVisit = nVisits ) )
        lShuffled <- fnAnalyze( dfData[ sample( seq_len( 100 ) ), ], list( NumVisit = nVisits ) )
        expect_identical( lResult$ErrorCode, 0L )
        expect_gt( lResult$TestStat, 5 )
        expect_equal( lResult$TestStat, lShuffled$TestStat, tolerance = 1e-4 )
        if ( nVisits == 2 ) {
            cExpected <- stats::lm( Response2 ~ Response1 + TreatmentID, data = dfData )
            dOneSidedP <- summary( cExpected )$coefficients[ "TreatmentID", "Pr(>|t|)" ] / 2
            expect_equal( stats::pnorm( lResult$TestStat, lower.tail = FALSE ), dOneSidedP,
                tolerance = 1e-8 )
        }
        expect_false( any( c( "PrimDelta", "SecDelta" ) %in% names( lResult ) ) )
        expect_identical( fnAnalyze( dfData, list( NumVisit = nVisits ), LookInfo = list() )$ErrorCode, 1L )
    }
} )
