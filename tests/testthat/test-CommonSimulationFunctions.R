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
