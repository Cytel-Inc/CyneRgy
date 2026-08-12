#################################################################################################### .
#   Description: Tests for the common simulation functions exported by CyneRgy.
#################################################################################################### .

test_that( "common functions run a complete binary simulation workflow", {
    set.seed( 42 )

    NumSub     <- 100
    NumArms    <- 2
    AllocRatio <- 1
    ProbDrop   <- 0.10
    PropResp   <- c( 0.30, 0.50 )

    lArrival <- GeneratePoissonArrival( NumSub = NumSub, NumPrd = 1, PrdStart = 0, AccrRate = 10 )
    expect_identical( lArrival$ErrorCode, 0L )
    expect_length( lArrival$ArrivalTime, NumSub )
    expect_true( all( diff( lArrival$ArrivalTime ) >= 0 ) )

    lRandomization <- RandomizationSubjectsUsingUniformDistribution( NumSub = NumSub, NumArms = NumArms,
                                                                      AllocRatio = AllocRatio )
    expect_identical( lRandomization$ErrorCode, 0L )
    expect_equal( as.integer( table( lRandomization$TreatmentID ) ), c( 50L, 50L ) )

    lCensoring <- GenerateCensoringUsingBinomialProportion( NumSub = NumSub, ProbDrop = ProbDrop )
    expect_identical( lCensoring$ErrorCode, 0L )
    expect_true( all( lCensoring$CensorInd %in% c( 0L, 1L ) ) )

    lOutcome <- SimulatePatientOutcomePercentAtZero.Binary( NumSub = NumSub, NumArm = NumArms,
                                                             ArrivalTime = lArrival$ArrivalTime,
                                                             TreatmentID = lRandomization$TreatmentID,
                                                             PropResp = PropResp )
    expect_identical( lOutcome$ErrorCode, 0L )
    expect_true( all( lOutcome$Response %in% c( 0, 1 ) ) )

    bCompleters <- lCensoring$CensorInd == 1
    SimData     <- data.frame( Response = lOutcome$Response[ bCompleters ],
                               TreatmentID = lRandomization$TreatmentID[ bCompleters ] )
    UserParam   <- list( dAlphaCtrl = 0.5, dBetaCtrl = 0.5,
                         dAlphaExp = 1, dBetaExp = 1,
                         dUpperCutoffEfficacy = 0.8,
                         dLowerCutoffForFutility = 0.1 )

    lAnalysis <- AnalyzeUsingBetaBinomial( SimData = SimData,
                                           DesignParam = list( TailType = 1, CriticalPoint = 1.96 ),
                                           UserParam = UserParam )
    expect_identical( lAnalysis$ErrorCode, 0L )
    expect_true( lAnalysis$TestStat >= 0 && lAnalysis$TestStat <= 1 )
    expect_true( lAnalysis$Decision %in% c( 0L, 2L ) )
    expect_type( lAnalysis$Delta, "double" )
} )


test_that( "common functions validate unsupported inputs", {
    expect_identical(
        RandomizationSubjectsUsingUniformDistribution( NumSub = 10, NumArms = 3, AllocRatio = 1 )$ErrorCode,
        -1L
    )
    expect_error( GenerateCensoringUsingBinomialProportion( NumSub = 10, ProbDrop = 2 ), "between 0 and 1" )

    SimData <- data.frame( Response = c( 0, 1 ), TreatmentID = c( 0, 1 ) )
    lResult <- AnalyzeUsingBetaBinomial( SimData, list( TailType = 1 ), UserParam = NULL )
    expect_identical( lResult$ErrorCode, -1L )
} )


test_that( "common endpoint wrappers expose and load their bundled implementations", {
    lFunctions <- list(
        SimulatePatientOutcomePercentAtZero = c( "2ArmNormalOutcomePatientSimulation", "SimulatePatientOutcomePercentAtZero.R" ),
        SimulatePatientOutcomePercentAtZeroBetaDist = c( "2ArmNormalOutcomePatientSimulation", "SimulatePatientOutcomePercentAtZeroBetaDist.R" ),
        AnalyzeUsingTTestNormal = c( "2ArmNormalOutcomeAnalysis", "AnalyzeUsingTTestNormal.R" ),
        AnalyzeUsingMeanLimitsOfCI = c( "2ArmNormalOutcomeAnalysis", "AnalyzeUsingMeanLimitsOfCI.R" ),
        AnalyzeUsingEastManualFormulaNormal = c( "2ArmNormalOutcomeAnalysis", "AnalyzeUsingEastManualFormulaNormal.R" ),
        SimulatePatientSurvivalWeibull = c( "2ArmTimeToEventOutcomePatientSimulation", "SimulatePatientSurvivalWeibull.R" ),
        SimulatePatientSurvivalMixtureExponentials = c( "2ArmTimeToEventOutcomePatientSimulation", "SimulatePatientSurvivalMixtureExponentials.R" ),
        AnalyzeUsingSurvivalPackage = c( "2ArmTimeToEventOutcomeAnalysis", "AnalyzeUsingSurvivalPackage.R" ),
        AnalyzeUsingHazardRatioLimitsOfCI = c( "2ArmTimeToEventOutcomeAnalysis", "AnalyzeUsingHazardRatioLimitsOfCI.R" ),
        AnalyzeUsingEastLogrankFormula = c( "2ArmTimeToEventOutcomeAnalysis", "AnalyzeUsingEastLogrankFormula.R" ),
        GenRespDiffOfMeansRepMeasures = c( "2ArmNormalRepeatedMeasuresResponseGeneration", "GenerateResponseDiffOfMeansRepeatedMeasures.R" ),
        Analyze.RepeatedMeasures = c( "2ArmNormalRepeatedMeasuresAnalysis", "Analyze.RepeatedMeasures.R" ),
        GenerateDropoutTimeForRM = c( "2ArmPatientDropout", "GenerateDropoutTimeForRM.R" ),
        GenerateDropoutTimeForSurvival = c( "2ArmPatientDropout", "GenerateDropoutTimeForSurvival.R" ),
        AnalyzeDEPUsingFisherExact = c( "DEPAnalysis", "AnalyzeDEPUsingFisherExact.R" ),
        AnalyzeDEPUsingModWtLogRank = c( "DEPAnalysis", "AnalyzeDEPUsingModWtLogRank.R" ),
        GetDEPDecisionsFSD = c( "DEPDecisionsUsingMCP", "GetDEPDecisionsFSD.R" ),
        SimulatePatientOutcomeDEPSurvBinSingleHazardPiece = c( "DEPPatientSimulation", "SimulatePatientOutcomeDEPSurvBinSingleHazardPiece.R" ),
        SimulatePatientOutcomeDEPSurvSurvSingleHazardPiece = c( "DEPPatientSimulation", "SimulatePatientOutcomeDEPSurvSurvSingleHazardPiece.R" ),
        GeneratePoissonArrivalMEP = c( "GeneratePoissonArrival", "GeneratePoissonArrivalMEP.R" ),
        GenerateMEPResponse = c( "MEPPatientSimulation", "GenerateMEPResponse.R" ),
        GetMEPDecision = c( "MEPDesign", "GetMEPDecision.R" )
    )

    expect_true( all( names( lFunctions ) %in% getNamespaceExports( "CyneRgy" ) ) )

    for( strFunction in names( lFunctions ) )
    {
        vLocation <- lFunctions[[ strFunction ]]
        fnExample <- .GetCommonExampleFunction( vLocation[ 1 ], vLocation[ 2 ], strFunction )
        expect_true( is.function( fnExample ), info = strFunction )
        expect_identical( formals( getExportedValue( "CyneRgy", strFunction ) ),
                          formals( fnExample ), info = strFunction )
    }
} )


test_that( "continuous, TTE, RM, and MEP wrappers run representative simulations", {
    set.seed( 7 )
    nQtyOfPatients <- 20
    lArrival       <- GeneratePoissonArrival( nQtyOfPatients, 1, 0, 5 )
    lTreatment     <- RandomizationSubjectsUsingUniformDistribution( nQtyOfPatients, 2, 1 )

    lContinuous <- SimulatePatientOutcomePercentAtZero(
        nQtyOfPatients, lArrival$ArrivalTime, lTreatment$TreatmentID,
        Mean = c( 0, 0.5 ), StdDev = c( 1, 1 )
    )
    expect_length( lContinuous$Response, nQtyOfPatients )
    expect_identical( lContinuous$ErrorCode, 0L )

    lTTE <- SimulatePatientSurvivalWeibull(
        nQtyOfPatients, 2, lArrival$ArrivalTime, lTreatment$TreatmentID,
        SurvMethod = 3, NumPrd = 1, PrdTime = 0, SurvParam = matrix( c( 1, 1 ), nrow = 1 )
    )
    expect_length( lTTE$SurvivalTime, nQtyOfPatients )
    expect_true( all( lTTE$SurvivalTime > 0 ) )

    skip_if_not_installed( "MASS" )
    lRM <- GenRespDiffOfMeansRepMeasures(
        nQtyOfPatients, NumVisit = 2, ArrivalTime = lArrival$ArrivalTime,
        TreatmentID = lTreatment$TreatmentID, Inputmethod = 1, VisitTime = c( 1, 2 ),
        MeanControl = c( 0, 0 ), MeanTrt = c( 0.5, 0.5 ),
        StdDevControl = c( 1, 1 ), StdDevTrt = c( 1, 1 ),
        CorrMat = matrix( c( 1, 0.5, 0.5, 1 ), nrow = 2 )
    )
    expect_length( lRM$Response, nQtyOfPatients )
    expect_length( lRM$Response2, nQtyOfPatients )

    lMEPArrival <- GeneratePoissonArrivalMEP( nQtyOfPatients, 1, 0, 5 )
    expect_length( lMEPArrival$ArrivalTime, nQtyOfPatients )
} )
