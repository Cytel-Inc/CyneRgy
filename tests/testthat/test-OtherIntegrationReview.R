test_that( "DEP decisions use native endpoint names and the correct efficacy codes", {
    fnDecision <- .GetCommonExampleFunction( "DEPDecisionsUsingMCP", "GetDEPDecisionsFSD.R", "GetDEPDecisionsFSD" )
    lDesign <- list( EndpointName = c( "Survival", "Response" ), Alpha = 0.05,
        TailType = list( Response = 1L, Survival = 0L ) )
    lResult <- fnDecision( data.frame( ), lDesign, TestStat = list( Response = 3, Survival = -3 ) )
    expect_identical( lResult$Decision, list( Survival = 1L, Response = 2L ) )
    lPending <- fnDecision( data.frame( ), lDesign, TestStat = list( Response = NA_real_, Survival = -3 ) )
    expect_identical( lPending$Decision, list( Survival = 1L, Response = 0L ) )
} )

test_that( "treatment selection preserves IDs when an experimental arm has no observations", {
    dfData <- data.frame( TreatmentID = rep( c( 0, 2 ), each = 20 ),
        Response = c( rep( 0, 20 ), rep( 1, 20 ) ) )
    for ( strFunction in c( "SelectExpThatAreBetterThanCtrl", "SelectExpUsingBayesianRule",
        "SelectExpWithPValueLessThanSpecified", "SelectSpecifiedNumberOfExpWithHighestResponses" ) ) {
        fnSelect <- .GetCommonExampleFunction( "TreatmentSelection", paste0( strFunction, ".R" ), strFunction )
        lResult <- suppressWarnings( fnSelect( dfData, list( ), NULL ) )
        expect_identical( lResult$TreatmentID, 2L, info = strFunction )
        expect_length( lResult$AllocRatio, 1 )
    }
} )

test_that( "randomization retains exact sizes for empty and singleton allocations", {
    fnSample <- .GetCommonExampleFunction( "RandomizeSubjects", "RandomizationSubjectsUsingSampleFunctionInR.R",
        "RandomizationSubjectsUsingSampleFunctionInR" )
    fnUniform <- .GetCommonExampleFunction( "RandomizeSubjects", "RandomizationSubjectsUsingUniformDistribution.R",
        "RandomizationSubjectsUsingUniformDistribution" )
    fnMultiple <- .GetCommonExampleFunction( "RandomizeSubjects", "RandomizeSubjectsAcrossMultipleArms.R",
        "RandomizeSubjectsAcrossMultipleArms" )
    for ( nSubjects in 1:3 ) {
        for ( dRatio in c( 0, 1, 2, 100 ) ) {
            nExpectedExperimental <- nSubjects - round( nSubjects / ( 1 + dRatio ) )
            for ( nSeed in 1:5 ) {
                for ( fnRandomize in list( fnSample, fnUniform ) ) {
                    set.seed( nSeed )
                    lResult <- fnRandomize( nSubjects, 2, dRatio )
                    expect_length( lResult$TreatmentID, nSubjects )
                    expect_equal( sum( lResult$TreatmentID ), nExpectedExperimental )
                }
            }
        }
    }
    expect_identical( fnMultiple( 1, 3, c( 0, 100 ) )$TreatmentID, 2L )
} )

test_that( "dropout examples reject unsupported model configurations", {
    fnSurvival <- .GetCommonExampleFunction( "2ArmPatientDropout", "GenerateDropoutTimeForSurvival.R",
        "GenerateDropoutTimeForSurvival" )
    fnMultiple <- .GetCommonExampleFunction( "MultiArmPatientDropout", "GenerateDropoutTimeMultiArmForSurvival.R",
        "GenerateDropoutTimeMultiArmForSurvival" )
    for ( fnDropout in list( fnSurvival, fnMultiple ) ) {
        lResult <- fnDropout( 4, 2, c( 0, 1, 0, 1 ), 1, 2, c( 0, 2 ), matrix( 0.1, 2, 2 ) )
        expect_identical( lResult$ErrorCode, -1L )
        expect_identical( fnDropout( 4, 2, c( 0, 1, 0, 1 ), 1, 1, 0,
            matrix( 0, 1, 2 ) )$DropOutTime, rep( Inf, 4 ) )
    }
    fnRepeated <- .GetCommonExampleFunction( "2ArmPatientDropout", "GenerateDropoutTimeForRM.R",
        "GenerateDropoutTimeForRM" )
    lResult <- fnRepeated( 4, 2, 2, c( 1, 2 ), c( 0, 1, 0, 1 ), 1, c( 1, 2 ), c( 0.1, 0.2 ), c( 0.1, 0.2 ) )
    expect_identical( lResult$ErrorCode, -1L )
} )

test_that( "Poisson enrollment rejects a zero final rate and handles zero ramp-up rates", {
    for ( strFunction in c( "GeneratePoissonArrival", "GeneratePoissonArrivalMEP" ) ) {
        fnArrival <- .GetCommonExampleFunction( "GeneratePoissonArrival", paste0( strFunction, ".R" ), strFunction )
        expect_identical( fnArrival( 10, 1, 0, 0 )$ErrorCode, -1L )
        expect_identical( fnArrival( 10, 2, c( 0, 1 ), c( 1, 0 ) )$ErrorCode, -1L )
        expect_identical( fnArrival( 10, 1, 0, 1, UserParam = list( dRate2 = 1 ) )$ErrorCode, -1L )
        lResult <- fnArrival( 10, 2, c( 0, 1 ), c( 0, 5 ) )
        expect_length( lResult$ArrivalTime, 10 )
        expect_true( all( lResult$ArrivalTime >= 1 ) )
        expect_true( all( diff( lResult$ArrivalTime ) >= 0 ) )
    }
} )

test_that( "MEP decision handling accepts missing statistics at pending looks", {
    fnDecision <- .GetCommonExampleFunction( "MEPDesign", "GetMEPDecision.R", "GetMEPDecision" )
    lDesign <- list( EndpointName = "Response", EndpointType = 1L, TargetInformation = 100,
        EffFlg = matrix( 1L, 2, 1 ), FutFlg = matrix( 0L, 2, 1 ), FutThrsld = matrix( 0, 2, 1 ) )
    lLook <- list( LastLookDecision = 0L, EfficacyBoundaryPScale = NA_real_, LookNum = 1L,
        TestStatisticsOutputs = list( Response = list( data = list( TSPVal = NA_real_ ) ) ), EPStatus = 0L )
    lResult <- fnDecision( data.frame( ), data.frame( ), list( Response = list( Completers = 10 ) ), lLook, lDesign )
    expect_identical( lResult$Decision, 0L )
    expect_identical( lResult$ErrorCode, 0L )
} )

test_that( "block randomization rejects invalid ratios and non-positive blocks before generation", {
    fnRandomize <- .GetCommonExampleFunction( "RandomizeSubjects", "BlockRandomizationSubjectsUsingRPackage.R",
        "BlockRandomizationSubjectsUsingRPackage" )
    expect_identical( fnRandomize( 10, 3, c( 1, 1 ) )$ErrorCode, -1L )
    expect_identical( fnRandomize( 10, 2, 0, list( BlockSize1 = 10 ) )$ErrorCode, -7L )
    expect_identical( fnRandomize( 10, 2, 1, list( BlockSize1 = -2, BlockSize2 = 12 ) )$ErrorCode, -4L )
    expect_identical( fnRandomize( 10, 2, 1, list( BlockSize1 = NA_real_ ) )$ErrorCode, -4L )
} )

test_that( "ranked selection rejects unavailable ranks and missing allocation ratios", {
    fnSelect <- .GetCommonExampleFunction( "TreatmentSelection", "SelectSpecifiedNumberOfExpWithHighestResponses.R",
        "SelectSpecifiedNumberOfExpWithHighestResponses" )
    dfData <- data.frame( TreatmentID = rep( 0:2, each = 3 ), Response = rep( c( 0, 1, 1 ), each = 3 ) )
    expect_identical( fnSelect( dfData, list( ), NULL,
        list( QtyOfArmsToSelect = 3, Rank1AllocationRatio = 1 ) )$ErrorCode, -2L )
    expect_identical( fnSelect( dfData, list( ), NULL,
        list( QtyOfArmsToSelect = 2, Rank1AllocationRatio = 1 ) )$ErrorCode, -2L )
    lResult <- fnSelect( dfData, list( ), NULL,
        list( QtyOfArmsToSelect = 2, Rank1AllocationRatio = 2, Rank2AllocationRatio = 1 ) )
    expect_identical( lResult$TreatmentID, c( 1L, 2L ) )
    expect_identical( lResult$AllocRatio, c( 2, 1 ) )
} )

test_that( "Poisson enrollment counts are not capped in short accrual periods", {
    for ( strFunction in c( "GeneratePoissonArrival", "GeneratePoissonArrivalMEP" ) ) {
        fnArrival <- .GetCommonExampleFunction( "GeneratePoissonArrival", paste0( strFunction, ".R" ), strFunction )
        fnPeriod <- get( "SimulateAccrualTimesWithConstantRate", envir = environment( fnArrival ) )
        set.seed( 389 )
        vCounts <- replicate( 1000, length( fnPeriod( 100, 0, 0.1 ) ) )
        expect_equal( mean( vCounts ), 10, tolerance = 0.05 )
        expect_true( any( vCounts > 14 ) )
        lResult <- fnArrival( 200, 3, c( 0, 0.1, 1.1 ), c( 100, 0, 20 ) )
        expect_length( lResult$ArrivalTime, 200 )
        expect_true( all( diff( lResult$ArrivalTime ) >= 0 ) )
        expect_false( any( lResult$ArrivalTime >= 0.1 & lResult$ArrivalTime < 1.1 ) )
    }
} )

test_that( "MEP futility-only looks do not require an efficacy boundary", {
    fnDecision <- .GetCommonExampleFunction( "MEPDesign", "GetMEPDecision.R", "GetMEPDecision" )
    for ( nEndpointType in c( 1L, 2L ) ) {
        lDesign <- list( EndpointName = "Clinical response", EndpointType = nEndpointType,
            TargetInformation = 100, EffFlg = matrix( c( 0L, 1L ), 2, 1 ),
            FutFlg = matrix( c( 1L, 0L ), 2, 1 ), FutThrsld = matrix( 0.5, 2, 1 ) )
        lLook <- list( LastLookDecision = 0L, EfficacyBoundaryPScale = NaN, LookNum = 1L,
            TestStatisticsOutputs = list( "Clinical response" = list( data =
                list( TSPVal = 0.7, Delta = 0.1, HR = 0.9 ) ) ), EPStatus = 0L )
        lSummary <- list( "Clinical response" = list( Completers = 20, Events = 20, Delta = 0.1, HR = 0.9 ) )
        lResult <- fnDecision( data.frame( ), data.frame( ), lSummary, lLook, lDesign )
        expect_identical( lResult$Decision, 2L )
        expect_identical( lResult$ErrorCode, 0L )
    }
} )


test_that( "block randomization rejects ratios outside its integer representation", {
    fnRandomize <- .GetCommonExampleFunction( "RandomizeSubjects", "BlockRandomizationSubjectsUsingRPackage.R",
        "BlockRandomizationSubjectsUsingRPackage" )
    for ( dRatio in c( 0.005, 1e-9, 1e10 ) ) {
        expect_identical( fnRandomize( 100, 2, dRatio, list( BlockSize1 = 100 ) )$ErrorCode, -7L )
    }
    fnConvert <- get( "ConvertRatio", envir = environment( fnRandomize ) )
    expect_identical( fnConvert( 0.5 ), c( 2L, 1L ) )
    expect_identical( fnConvert( 1 ), c( 1L, 1L ) )
    expect_identical( fnConvert( 2 ), c( 1L, 2L ) )
} )
