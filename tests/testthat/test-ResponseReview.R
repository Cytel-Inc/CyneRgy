######################################################################################################################## .
# Regression checks for response example contracts and subject ordering.
######################################################################################################################## .

test_that( "Phase 3 survival assurance reports scalar success and handles single-arm samples", {
    fnGenerate <- .GetCommonExampleFunction( "ConsecutiveStudiesContinuousTimeToEvent",
        "SimulatePatientSurvivalAssuranceUsingPh2Prior.R", "SimulatePatientSurvivalAssuranceUsingPh2Prior" )
    envPrior <- environment( fnGenerate )
    envPrior$gvPrior <- c( 0.5, 0.5 )
    envPrior$gnIndex <- 1
    lUser <- list( dIntercept = 0, dSlope = -1, dMeanTTECtrl = 1 )
    for ( nArm in 0:1 ) {
        lResult <- fnGenerate( 4, 2, rep( 0, 4 ), rep( nArm, 4 ), 1, 1, 0,
            matrix( c( 1, 1 ), nrow = 1 ), lUser )
        expect_identical( lResult$ErrorCode, 0L )
        expect_length( lResult$SurvivalTime, 4 )
        expect_true( all( is.finite( lResult$SurvivalTime ) ) )
        expect_equal( lResult$TrueHR, rep( exp( -0.5 ), 4 ) )
    }
    lExhausted <- fnGenerate( 4, 2, rep( 0, 4 ), rep( 0, 4 ), 1, 1, 0,
        matrix( c( 1, 1 ), nrow = 1 ), lUser )
    expect_identical( lExhausted$ErrorCode, -100L )
} )

test_that( "dual-endpoint responses use named inputs and ignore unused user parameters", {
    vExamples <- c( "SimulatePatientOutcomeDEPSurvSurvSingleHazardPiece",
        "SimulatePatientOutcomeDEPSurvBinSingleHazardPiece" )
    for ( strFunction in vExamples ) {
        fnGenerate <- .GetCommonExampleFunction( "DEPPatientSimulation", paste0( strFunction, ".R" ),
            strFunction )
        bBinary <- grepl( "SurvBin", strFunction )
        vNames <- c( "First", "Second" )
        lHazards <- list( Second = matrix( c( 0.4, 0.8 ), nrow = 1 ),
            First = matrix( c( 0.2, 0.3 ), nrow = 1 ) )
        lInputs <- list( NumSub = 10, NumArm = 2, ArrivalTime = rep( 0, 10 ),
            TreatmentID = rep( 0:1, 5 ), EndpointType = c( 2, ifelse( bBinary, 1, 2 ) ),
            EndpointName = vNames, Correlation = 0, SurvMethod = list( First = 1, Second = 1 ),
            NumPrd = list( First = 1, Second = 1 ), PrdTime = list( First = 0, Second = 0 ),
            SurvParam = lHazards, PropResp = list( First = NA, Second = c( 0, 1 ) ) )
        set.seed( 137 )
        lExpected <- do.call( fnGenerate, lInputs )
        set.seed( 137 )
        lActual <- do.call( fnGenerate, c( lInputs, list( UserParam = list( Unused = 1 ) ) ) )
        expect_identical( lActual, lExpected )
        expect_true( all( lActual$Response$First > 0 ) )
        expect_identical( lActual$ErrorCode, 0L )
        if ( bBinary ) {
            expect_equal( lActual$Response$Second, rep( 0:1, 5 ) )
        } else {
            set.seed( 137 )
            mLatent <- matrix( stats::rnorm( 20 ), ncol = 2 )
            expect_equal( lActual$Response$First,
                -log( stats::pnorm( mLatent[ , 1 ] ) ) / rep( c( 0.2, 0.3 ), 5 ) )
            expect_equal( lActual$Response$Second,
                -log( stats::pnorm( mLatent[ , 2 ] ) ) / rep( c( 0.4, 0.8 ), 5 ) )
        }
    }
} )

test_that( "CSV response generation samples singleton arm rows by index", {
    strOriginalDir <- getwd()
    bHadCache <- exists( "gdfPatients", envir = .GlobalEnv, inherits = FALSE )
    if ( bHadCache ) {
        dfOriginalCache <- get( "gdfPatients", envir = .GlobalEnv )
    }
    on.exit( {
        if ( bHadCache ) {
            assign( "gdfPatients", dfOriginalCache, envir = .GlobalEnv )
        } else if ( exists( "gdfPatients", envir = .GlobalEnv, inherits = FALSE ) ) {
            rm( "gdfPatients", envir = .GlobalEnv )
        }
    }, add = TRUE )
    if ( bHadCache ) {
        rm( "gdfPatients", envir = .GlobalEnv )
    }
    strTestDir <- tempfile( "response-csv-" )
    dir.create( file.path( strTestDir, "Inputs" ), recursive = TRUE )
    on.exit( setwd( strOriginalDir ), add = TRUE )
    on.exit( unlink( strTestDir, recursive = TRUE ), add = TRUE )
    utils::write.csv( data.frame( Treatment = c( 1, 0 ), "Visit 1" = c( 100, 200 ),
        check.names = FALSE ), file.path( strTestDir, "Inputs", "patients.csv" ), row.names = FALSE )
    vFunctions <- c( "GeneratePatientFromCSVGeneral", "GeneratePatientFromCSVSpecific" )
    lFunctions <- lapply( vFunctions, function( strFunction ) {
        return( .GetCommonExampleFunction( "PKPDResponseGeneration", paste0( strFunction, ".R" ), strFunction ) )
    } )
    # Source before changing directories so source lookup remains tied to the package.
    setwd( strTestDir )
    for ( fnGenerate in lFunctions ) {
        for ( nTrial in seq_len( 10 ) ) {
            lResult <- fnGenerate( 2, 1, c( 0, 0 ), c( 0, 1 ), 0, 1, 0, 0, 1, 1,
                matrix( 1 ), list( InputFileName = "patients.csv" ) )
            expect_identical( lResult$ErrorCode, 0L )
            expect_equal( lResult$Response1, c( 200, 100 ) )
        }
    }
} )

test_that( "normal repeated-measures generators handle an empty arm", {
    skip_if_not_installed( "MASS" )
    lFunctions <- list(
        .GetCommonExampleFunction( "2ArmNormalRepeatedMeasuresResponseGeneration",
            "GenerateResponseDiffOfMeansRepeatedMeasures.R", "GenRespDiffOfMeansRepMeasures" ),
        .GetCommonExampleFunction( "SchizophreniaTrial", "GenerateMMRMResponses.R", "GenerateMMRMResponses" )
    )
    for ( fnGenerate in lFunctions ) {
        for ( nArm in 0:1 ) {
            lResult <- fnGenerate( 3, 2, rep( 0, 3 ), rep( nArm, 3 ), 0, c( 1, 2 ),
                c( 1, 2 ), c( 10, 20 ), c( 0, 0 ), c( 0, 0 ), diag( 2 ) )
            expect_identical( lResult$ErrorCode, 0L )
            expect_equal( lResult$Response1, rep( ifelse( nArm == 0, 1, 10 ), 3 ) )
            expect_equal( lResult$Response2, rep( ifelse( nArm == 0, 2, 20 ), 3 ) )
        }
    }
} )


test_that( "multiple-outcome examples return their documented error when user inputs are missing", {
    vFunctions <- c( "SimulateMultipleOutcomes", "SimulateMultipleOutcomesCovariates",
        "SimulateMultipleOutcomesCovariatesStratRandomization" )
    for ( strFunction in vFunctions ) {
        fnGenerate <- .GetCommonExampleFunction( "MultipleEndpointsWithCovariates", paste0( strFunction, ".R" ),
            strFunction )
        lResult <- fnGenerate( 4, rep( 0, 4 ), c( 0, 1, 0, 1 ), c( 0, 1 ), c( 1, 1 ) )
        expect_identical( lResult$ErrorCode, 1L )
        expect_length( lResult$Response, 4 )
    }
} )

test_that( "PK concentration and Emax response generators coexist and integrate successive visit times", {
    skip_if_not_installed( "deSolve" )
    fnPK <- .GetCommonExampleFunction( "PKPDResponseGeneration", "GenerateDrugConcentration.R",
        "GenerateDrugConcentration" )
    fnEmax <- .GetCommonExampleFunction( "PKPDResponseGeneration", "GenerateResponseEmaxModel.R",
        "GenerateResponseEmaxModel" )
    # Loading the standalone integration function must not replace the Emax-specific helper.
    envEmax <- environment( fnEmax )
    envEmax$GenerateDrugConcentration <- fnPK
    on.exit( rm( "GenerateDrugConcentration", envir = envEmax ), add = TRUE )
    vVisits <- c( 0, 1, 2 )
    lInputs <- list( NumSub = 2, NumVisit = 3, ArrivalTime = c( 0, 0 ), TreatmentID = c( 0, 1 ),
        Inputmethod = 0, VisitTime = vVisits, MeanControl = rep( 0, 3 ), MeanTrt = rep( 0, 3 ),
        StdDevControl = rep( 0, 3 ), StdDevTrt = rep( 0, 3 ), CorrMat = diag( 3 ) )
    lPK <- do.call( fnPK, c( lInputs, list( UserParam = list( AbsorptionRate = 1,
        EliminationRate = 0.5, Dose = 10 ) ) ) )
    lEmax <- do.call( fnEmax, c( lInputs, list( UserParam = list( AbsorptionRate = 1,
        EliminationRate = 0.5, Dose = 10, E0 = 1, Emax = 2, EC50 = 5 ) ) ) )
    vExpectedPK <- 20 * ( exp( -0.5 * vVisits ) - exp( -vVisits ) )
    expect_identical( lPK$ErrorCode, 0L )
    expect_identical( lEmax$ErrorCode, 0L )
    for ( nVisitIndx in seq_along( vVisits ) ) {
        expect_equal( lPK[[ paste0( "Response", nVisitIndx ) ]], rep( vExpectedPK[ nVisitIndx ], 2 ),
            tolerance = 1e-5 )
        expect_equal( lEmax[[ paste0( "Response", nVisitIndx ) ]],
            c( 0, 1 + 2 * vExpectedPK[ nVisitIndx ] / ( 5 + vExpectedPK[ nVisitIndx ] ) ), tolerance = 1e-5 )
    }
} )

test_that( "multi-state response generation preserves absent arms and flags failed generation", {
    fnGenerate <- .GetCommonExampleFunction( "ProbabilitySuccessDualEndpoints",
        "Simulate2EndpointTTEWithMultiState.R", "Simulate2EndpointTTEWithMultiState" )
    envGenerate <- environment( fnGenerate )
    expect_equal( envGenerate$SimulateDualMultiStateTTE( 0, 6, 12, 0.1 ),
        data.frame( vPFS = numeric( 0 ), vOS = numeric( 0 ) ) )
    fnOriginal <- envGenerate$SimulateDualMultiStateTTE
    on.exit( assign( "SimulateDualMultiStateTTE", fnOriginal, envir = envGenerate ), add = TRUE )
    # Keep this boundary check independent of the numerical median-solving routine.
    envGenerate$SimulateDualMultiStateTTE <- function( nQtyOfPatients, ... ) {
        return( data.frame( vPFS = rep( 6, nQtyOfPatients ), vOS = rep( 12, nQtyOfPatients ) ) )
    }
    lUser <- list( dMedianPFS0 = 6, dMedianOS0 = 12, dProbOfDeathBeforeProgression0 = 0.1,
        dMedianPFS1 = 6, dMedianOS1 = 12, dProbOfDeathBeforeProgression1 = 0.1 )
    for ( nArm in 0:1 ) {
        lResult <- fnGenerate( 3, 2, rep( 0, 3 ), rep( nArm, 3 ), 1, 1, 0,
            matrix( c( 1, 1 ), nrow = 1 ), lUser )
        expect_identical( lResult$ErrorCode, 0L )
        expect_equal( lResult$SurvivalTime, rep( 6, 3 ) )
        expect_equal( lResult$OS, rep( 12, 3 ) )
    }
    envGenerate$SimulateDualMultiStateTTE <- function( nQtyOfPatients, ... ) {
        return( data.frame( vPFS = rep( NA_real_, nQtyOfPatients ), vOS = rep( NA_real_, nQtyOfPatients ) ) )
    }
    lFailed <- fnGenerate( 3, 2, rep( 0, 3 ), rep( 0, 3 ), 1, 1, 0,
        matrix( c( 1, 1 ), nrow = 1 ), lUser )
    expect_identical( lFailed$ErrorCode, 1L )
} )
