test_that( "survival dropout preserves single-period numeric and matrix inputs", {
    lFunctions <- list(
        .GetCommonExampleFunction( "2ArmPatientDropout", "GenerateDropoutTimeForSurvival.R",
            "GenerateDropoutTimeForSurvival" ),
        .GetCommonExampleFunction( "MultiArmPatientDropout", "GenerateDropoutTimeMultiArmForSurvival.R",
            "GenerateDropoutTimeMultiArmForSurvival" )
    )
    for ( fnGenerate in lFunctions ) {
        for ( nMethod in 1:2 ) {
            vParameters <- c( 0.1, 0.2 )
            dTime <- if ( nMethod == 1 ) 0 else 12
            set.seed( 389 )
            lVector <- fnGenerate( 4, 2, c( 0, 1, 0, 1 ), nMethod, 1, dTime, vParameters )
            set.seed( 389 )
            lMatrix <- fnGenerate( 4, 2, c( 0, 1, 0, 1 ), nMethod, 1, dTime,
                matrix( vParameters, nrow = 1 ) )
            expect_identical( lVector, lMatrix )
            expect_identical( lVector$ErrorCode, 0L )
            expect_true( all( is.finite( lVector$DropOutTime ) ) )
        }
    }
} )

test_that( "survival assurance preserves the original single-probability interface", {
    fnGenerate <- .GetCommonExampleFunction( "BayesianAssuranceTimeToEvent",
        "SimulatePatientSurvivalAssurance.R", "SimulatePatientSurvivalAssurance" )
    lUser <- list( dWeight1 = 0.25, dPriorMean = 0, dPriorSD = 0.02, dAlpha = 2, dBeta = 2,
        dUpper = 0, dLower = -0.4, dMeanTTECtrl = 12 )
    set.seed( 389 )
    lResult <- fnGenerate( 4, 2, rep( 0, 4 ), c( 0, 1, 0, 1 ), 1, 1, 0,
        matrix( c( 0.1, 0.2 ), nrow = 1 ), lUser )
    expect_identical( lResult$ErrorCode, 0L )
    # Captured from Dev for this seed and the same complete input call.
    expect_equal( lResult$TrueHR, rep( 0.7610649, 4 ), tolerance = 1e-7 )
    expect_equal( lResult$SurvivalTime, c( 12.59216, 11.21997, 13.59015, 15.90388 ), tolerance = 1e-6 )
} )

test_that( "subpopulation analyses preserve the original custom hazard-ratio output", {
    skip_if_not_installed( "survival" )
    # Isolate the endpoint calculation from the optional graphical-testing package.
    envAnalyze <- new.env( parent = asNamespace( "CyneRgy" ) )
    sys.source( system.file( "Examples", "2ArmTimeToEventOutcomeAnalysis", "R",
        "AnalyzeSubpopulation.R", package = "CyneRgy" ), envir = envAnalyze )
    envAnalyze$ComputeGMCPDecisions <- function( vTestStats, ... ) {
        list( decisionFlag = rep( 0L, length( vTestStats ) ) )
    }
    set.seed( 389 )
    dfData <- data.frame( ArrivalTime = 0, TreatmentID = rep( 0:1, each = 50 ),
        SurvivalTime = stats::rexp( 100 ), Group = rep( c( "A", "B" ), 50 ) )
    lDesign <- list( MaxEvents = 75, NumSubPops = 1, SubpopName = "A",
        SubPops = list( A = list( Group = "A" ) ), PropAlpha = c( 0.5, 0.5 ),
        TransitionMatrix = matrix( 0, 2, 2 ), TailType = 0, Alpha = 0.025 )
    lResult <- envAnalyze$AnalyzeSubpopulation( dfData, lDesign )
    expect_identical( names( lResult$dHR ), c( "Full Population", "A" ) )
    expect_true( all( is.finite( unlist( lResult$dHR ) ) ) )
    expect_identical( lResult$dHR, lResult$HR )
} )

test_that( "dual-endpoint decisions accept positional statistics and tail directions", {
    fnDecide <- .GetCommonExampleFunction( "DEPDecisionsUsingMCP", "GetDEPDecisionsFSD.R",
        "GetDEPDecisionsFSD" )
    lDesign <- list( EndpointName = c( "First", "Second" ), TailType = c( First = 0, Second = 1 ),
        Alpha = 0.05 )
    lExpected <- fnDecide( NULL, lDesign, TestStat = list( Second = 3, First = -3 ) )
    lDesign$TailType <- unname( lDesign$TailType )
    expect_identical( fnDecide( NULL, lDesign, TestStat = list( -3, 3 ) ), lExpected )
    expect_identical( fnDecide( NULL, lDesign, TestStat = c( -3, 3 ) ), lExpected )
    expect_identical( lExpected$Decision, list( First = 1L, Second = 2L ) )
} )
