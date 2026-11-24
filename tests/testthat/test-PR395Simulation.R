test_that( "binary assurance treats missing prior configuration as fatal", {
    fnGenerate <- .GetCommonExampleFunction( "ConsecutiveStudiesBinary",
        "SimulatePatientOutcomeBinaryWithAssurance.R", "SimulatePatientOutcomeBinaryWithAssurance" )
    lInputs <- list( NumSub = 4L, NumArm = 2L,
        TreatmentID = c( 1L, 0L, 1L, 0L ), PropResp = c( 0.1, 0.9 ) )
    lMissing <- do.call( fnGenerate, lInputs )
    expect_identical( lMissing$ErrorCode, -1L )
    expect_identical( lMissing$Response, rep( NA_real_, 4 ) )
    lPrior <- list( dParameter1Ctrl = 2, dParameter2Ctrl = 3,
        dParameter1Exp = 4, dParameter2Exp = 5 )
    for ( strName in names( lPrior ) ) {
        lResult <- do.call( fnGenerate, c( lInputs, list( UserParam = lPrior[ names( lPrior ) != strName ] ) ) )
        expect_identical( lResult$ErrorCode, -2L )
        expect_identical( lResult$Response, rep( NA_real_, 4 ) )
    }
    set.seed( 395 )
    dControl <- stats::rbeta( 1, 2, 3 )
    dExperimental <- stats::rbeta( 1, 4, 5 )
    vExpected <- vapply( lInputs$TreatmentID, function( nArm ) {
        stats::rbinom( 1, 1, c( dControl, dExperimental )[ nArm + 1 ] )
    }, numeric( 1 ) )
    set.seed( 395 )
    lResult <- do.call( fnGenerate, c( lInputs, list( UserParam = lPrior ) ) )
    expect_identical( lResult$ErrorCode, 0L )
    expect_identical( lResult$Response, vExpected )
    expect_equal( lResult$TrueProbabilityControl, rep( dControl, 4 ) )
    expect_equal( lResult$TrueProbabilityExperimental, rep( dExperimental, 4 ) )
} )

test_that( "stratified survival uses the engine's per-stratum assessment time vector", {
    fnGenerate <- .GetCommonExampleFunction( "2ArmTimeToEventOutcomePatientSimulation",
        "SimulatePatientOutcomeStratification.R", "SimulatePatientOutcomeStratification" )
    lInputs <- list( NumSub = 4L, NumArm = 2L, ArrivalTime = rep( 0, 4 ),
        TreatmentID = c( 1L, 0L, 0L, 1L ), StratumID = c( 2L, 1L, 2L, 1L ),
        SurvMethod = 2L, NumPrd = 1L, PrdTime = c( 2, 20 ),
        SurvParam = matrix( c( 50, 25, 50, 25 ), nrow = 2, byrow = TRUE ) )
    set.seed( 395 )
    vExpected <- numeric( 4 )
    for ( nStratum in unique( lInputs$StratumID ) ) {
        for ( nSubject in which( lInputs$StratumID == nStratum ) ) {
            dRate <- -log( lInputs$SurvParam[ nStratum, lInputs$TreatmentID[ nSubject ] + 1 ] / 100 ) /
                lInputs$PrdTime[ nStratum ]
            vExpected[ nSubject ] <- stats::rexp( 1, dRate )
        }
    }
    set.seed( 395 )
    lResult <- do.call( fnGenerate, lInputs )
    expect_identical( lResult$ErrorCode, 0L )
    expect_equal( lResult$SurvivalTime, vExpected )
    lInputs$PrdTime <- cbind( c( 2, 20 ), c( 2, 20 ) )
    set.seed( 395 )
    expect_identical( do.call( fnGenerate, lInputs ), lResult )
    for ( dInvalidTime in c( NA_real_, Inf, 0 ) ) {
        lInputs$PrdTime[ 2, 1 ] <- dInvalidTime
        lFailed <- do.call( fnGenerate, lInputs )
        expect_identical( lFailed$ErrorCode, -100L )
        expect_true( is.na( lFailed$SurvivalTime[ 3 ] ) )
    }
} )

test_that( "randomization accepts the engine's NumArm partial name without changing public formals", {
    vFunctions <- c( "RandomizationSubjectsUsingUniformDistribution",
        "RandomizationSubjectsUsingSampleFunctionInR", "RandomizeSubjectsAcrossMultipleArms" )
    for ( strFunction in vFunctions ) {
        fnRandomize <- .GetCommonExampleFunction( "RandomizeSubjects", paste0( strFunction, ".R" ), strFunction )
        nArms <- if ( strFunction == "RandomizeSubjectsAcrossMultipleArms" ) 3L else 2L
        vRatio <- if ( nArms == 3L ) c( 1, 2 ) else 2
        set.seed( 395 )
        lEngine <- fnRandomize( NumSub = 12L, NumArm = nArms, AllocRatio = vRatio )
        set.seed( 395 )
        lPublic <- fnRandomize( NumSub = 12L, NumArms = nArms, AllocRatio = vRatio )
        expect_identical( lEngine, lPublic )
        expect_identical( lEngine$ErrorCode, 0L )
        expect_length( lEngine$TreatmentID, 12 )
        expect_identical( as.integer( table( factor( lEngine$TreatmentID, levels = seq_len( nArms ) - 1L ) ) ),
            if ( nArms == 3L ) c( 3L, 3L, 6L ) else c( 4L, 8L ) )
    }
    fnBlock <- .GetCommonExampleFunction( "RandomizeSubjects", "BlockRandomizationSubjectsUsingRPackage.R",
        "BlockRandomizationSubjectsUsingRPackage" )
    expect_identical( fnBlock( NumSub = 12L, NumArm = 2L, AllocRatio = 2 )$ErrorCode, -2L )
} )

test_that( "repeated-measures generation accepts the actual named Explore arguments", {
    skip_if_not_installed( "MASS" )
    fnGenerate <- .GetCommonExampleFunction( "2ArmNormalRepeatedMeasuresResponseGeneration",
        "GenerateResponseDiffOfMeansRepeatedMeasures.R", "GenRespDiffOfMeansRepMeasures" )
    lResult <- fnGenerate( NumSub = 3L, NumVisit = 2L, TreatmentID = c( 1L, 0L, 1L ),
        Inputmethod = 0L, VisitTime = c( 0, 2 ), MeanControl = c( 10, 11 ), MeanTrt = c( 20, 22 ),
        StdDevControl = c( 0, 0 ), StdDevTrt = c( 0, 0 ), CorrMat = diag( 2 ) )
    expect_identical( lResult$ErrorCode, 0L )
    expect_equal( lResult$Response1, c( 20, 10, 20 ) )
    expect_equal( lResult$Response2, c( 22, 11, 22 ) )
    fnDrop <- .GetCommonExampleFunction( "2ArmPatientDropout", "GenerateDropoutTimeForRM.R",
        "GenerateDropoutTimeForRM" )
    lDrop <- fnDrop( NumSub = 3L, NumArm = 2L, NumVisit = 2L, VisitTime = c( 0, 2 ),
        TreatmentID = c( 1L, 0L, 1L ), DropMethod = 2L, ByTime = 2,
        DropParamControl = 0, DropParamTrt = 0 )
    expect_identical( lDrop$DropOutTime, rep( Inf, 3 ) )
    expect_identical( lDrop$ErrorCode, 0L )
} )

test_that( "normal assurance copies accept Design no-arrival calls and advance supplied prior means", {
    for ( strExample in c( "BayesianAssuranceContinuous", "ConsecutiveStudiesContinuous",
        "ConsecutiveStudiesContinuousTimeToEvent" ) ) {
        fnGenerate <- .GetCommonExampleFunction( strExample, "SimulatePatientOutcomeNormalAssurance.R",
            "SimulatePatientOutcomeNormalAssurance" )
        lInputs <- list( NumSub = 3L, TreatmentID = c( 1L, 0L, 1L ),
            Mean = c( 10, 100 ), StdDev = c( 100, 100 ) )
        lUser <- list( dWeight1 = 0, dWeight2 = 7, dMean1 = 0.5, dMean2 = -2,
            dSD1 = 0, dSD2 = 0, dMeanCtrl = 10, dSDCtrl = 0, dSDExp = 0 )
        lResult <- do.call( fnGenerate, c( lInputs, list( UserParam = lUser ) ) )
        expect_identical( lResult$ErrorCode, 0L, info = strExample )
        expect_equal( lResult$Response, c( 8, 10, 8 ), info = strExample )
        expect_equal( lResult$vTrueDelta, rep( -2, 3 ), info = strExample )
        expect_identical( lResult$Delta, lResult$vTrueDelta, info = strExample )

        fnFromPrior <- .GetCommonExampleFunction( strExample, "SimulatePatientOutcomeNormalAssurance.R",
            "SimulatePatientOutcomeNormalAssuranceUsingPriorInput" )
        envPrior <- environment( fnFromPrior )
        envPrior$vPrior <- c( 12, 14 )
        envPrior$nSimIndex <- 1
        lPriorUser <- list( dSDCtrl = 0, dSDExp = 0 )
        for ( dMean in c( 12, 14 ) ) {
            lPrior <- do.call( fnFromPrior, c( lInputs, list( UserParam = lPriorUser ) ) )
            expect_identical( lPrior$ErrorCode, 0L, info = strExample )
            expect_equal( lPrior$Response, c( dMean, 10, dMean ), info = strExample )
            expect_equal( lPrior$Delta, rep( dMean - 10, 3 ), info = strExample )
        }
        lExhausted <- do.call( fnFromPrior, c( lInputs, list( UserParam = lPriorUser ) ) )
        expect_identical( lExhausted$ErrorCode, -100L, info = strExample )
        expect_equal( envPrior$nSimIndex, 3, info = strExample )
        rm( list = c( "vPrior", "nSimIndex" ), envir = envPrior )
        expect_identical( do.call( fnFromPrior, c( lInputs, list( UserParam = lPriorUser ) ) )$ErrorCode,
            -100L, info = strExample )
    }
} )

test_that( "standard outcome generators accept Design arguments without unused ArrivalTime", {
    for ( strFunction in c( "SimulatePatientOutcomePercentAtZero.Binary",
        "SimulatePatientOutcomePercentAtZeroBetaDist.Binary" ) ) {
        fnGenerate <- .GetCommonExampleFunction( "2ArmBinaryOutcomePatientSimulation",
            paste0( strFunction, ".R" ), strFunction )
        lResult <- fnGenerate( NumSub = 3L, NumArm = 2L, TreatmentID = c( 1L, 0L, 1L ),
            PropResp = c( 0, 1 ) )
        expect_identical( lResult$ErrorCode, 0L )
        expect_identical( lResult$Response, c( 1, 0, 1 ) )
    }
    for ( strFunction in c( "SimulatePatientOutcomePercentAtZero",
        "SimulatePatientOutcomePercentAtZeroBetaDist" ) ) {
        fnGenerate <- .GetCommonExampleFunction( "2ArmNormalOutcomePatientSimulation",
            paste0( strFunction, ".R" ), strFunction )
        lResult <- fnGenerate( NumSub = 3L, TreatmentID = c( 1L, 0L, 1L ),
            Mean = c( 10, 20 ), StdDev = c( 0, 0 ) )
        expect_identical( lResult$ErrorCode, 0L )
        expect_identical( lResult$Response, c( 20, 10, 20 ) )
    }
} )
