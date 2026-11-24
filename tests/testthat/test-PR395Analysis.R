test_that( "selection rules retain actual arm IDs and constant binary outcomes", {
    vFunctions <- c( "SelectExpThatAreBetterThanCtrl", "SelectExpUsingBayesianRule",
        "SelectExpWithPValueLessThanSpecified", "SelectSpecifiedNumberOfExpWithHighestResponses" )
    for ( strFunction in vFunctions ) {
        fnSelect <- .GetCommonExampleFunction( "TreatmentSelection", paste0( strFunction, ".R" ), strFunction )
        for ( nResponse in 0:1 ) {
            dfData <- data.frame( TreatmentID = rep( c( 0L, 4L ), each = 20 ), Response = nResponse )
            lResult <- suppressWarnings( fnSelect( dfData, list( ), list( ) ) )
            expect_identical( lResult$TreatmentID, 4L )
            expect_identical( lResult$ErrorCode, 0L )
            expect_length( lResult$AllocRatio, 1 )
            expect_type( lResult$AllocRatio, "double" )
        }
    }

    fnRank <- .GetCommonExampleFunction( "TreatmentSelection",
        "SelectSpecifiedNumberOfExpWithHighestResponses.R", "SelectSpecifiedNumberOfExpWithHighestResponses" )
    dfData <- data.frame( TreatmentID = rep( c( 0L, 2L, 4L ), each = 20 ),
        Response = c( rep( 0:1, 10 ), rep( 0, 20 ), rep( 1, 20 ) ) )
    lResult <- fnRank( dfData, list( ), list( ),
        list( QtyOfArmsToSelect = 2, Rank1AllocationRatio = 3, Rank2AllocationRatio = 1 ) )
    expect_identical( lResult$TreatmentID, c( 4L, 2L ) )
    expect_identical( lResult$AllocRatio, c( 3, 1 ) )
} )

test_that( "ranked selection rejects fatal configuration errors with engine-compatible types", {
    fnSelect <- .GetCommonExampleFunction( "TreatmentSelection",
        "SelectSpecifiedNumberOfExpWithHighestResponses.R", "SelectSpecifiedNumberOfExpWithHighestResponses" )
    dfData <- data.frame( TreatmentID = rep( 0:1, each = 10 ), Response = rep( 0:1, 10 ) )
    for ( nCount in c( 0, 2, 1.5, Inf, NA_real_ ) ) {
        lResult <- fnSelect( dfData, list( ), list( ),
            list( QtyOfArmsToSelect = nCount, Rank1AllocationRatio = 1 ) )
        expect_identical( lResult, list( TreatmentID = integer( ), AllocRatio = numeric( ), ErrorCode = -1L ) )
    }
    for ( dRatio in c( 0, 0.0009, 100.001, Inf, NA_real_ ) ) {
        lResult <- fnSelect( dfData, list( ), list( ),
            list( QtyOfArmsToSelect = 1, Rank1AllocationRatio = dRatio ) )
        expect_identical( lResult, list( TreatmentID = integer( ), AllocRatio = numeric( ), ErrorCode = -2L ) )
    }
    expect_identical( fnSelect( dfData, list( ), list( ),
        list( QtyOfArmsToSelect = 1 ) )$ErrorCode, -2L )
    for ( dRatio in c( 0.001, 100 ) ) {
        lResult <- fnSelect( dfData, list( ), list( ),
            list( QtyOfArmsToSelect = 1, Rank1AllocationRatio = dRatio ) )
        expect_identical( lResult$ErrorCode, 0L )
        expect_identical( lResult$AllocRatio, dRatio )
    }
} )

test_that( "TTE SSR targets final efficacy rather than current efficacy or futility", {
    skip_if_not_installed( "survival" )
    fnAnalyze <- .GetCommonExampleFunction( "2ArmTimeToEventOutcomeAnalysis",
        "AnalyzeTTESSR.R", "AnalyzeTTESSR" )
    set.seed( 395 )
    dfData <- data.frame( ArrivalTime = 0, TreatmentID = rep( 0:1, each = 80 ),
        SurvivalTime = stats::rexp( 160, rate = rep( c( 1, 0.8 ), each = 80 ) ) )
    dTime <- sort( dfData$SurvivalTime )[ 40 ]
    dfData$ObservedTime <- pmin( dfData$SurvivalTime, dTime )
    dfData$Event <- as.integer( dfData$SurvivalTime <= dTime )
    cLogrank <- survival::survdiff( survival::Surv( ObservedTime, Event ) ~ TreatmentID, data = dfData )
    dZ <- sqrt( cLogrank$chisq ) * sign( cLogrank$obs[ 2 ] - cLogrank$exp[ 2 ] )

    for ( nTail in 0:1 ) {
        dTailSign <- if ( nTail == 0 ) -1 else 1
        # Final boundary chosen so the current-trend CP equals 0.4.
        dFinal <- dZ / sqrt( 0.5 ) + dTailSign * stats::qnorm( 0.6 ) * sqrt( 0.5 )
        lDesign <- list( MaxEvents = 80L, TailType = nTail, CriticalPoint = 100 * dTailSign )
        lLook <- list( NumLooks = 2L, CurrLookIndex = 1L, InfoFrac = c( 0.5, 1 ),
            RejType = if ( nTail == 0 ) 5L else 4L,
            EffBdry = c( dZ + dTailSign * 2, dFinal ),
            FutBdry = c( dZ - dTailSign * 3, dZ - dTailSign * 4 ) )
        lAdapt <- list( AdaptInterimNum = 1L, SSRFuncScale = 0, PromZoneMin = 0.39, PromZoneMax = 0.41,
            MaxSSMultInp = list( From = 0.39, To = 0.41, MaxSSMult = 7, MaxEventsMult = 1.5 ) )
        lResult <- fnAnalyze( dfData, lDesign, lLook, lAdapt )
        expect_equal( lResult$TestStat, unname( dZ ) )
        expect_identical( lResult$ReEstEvents, 120L )
        expect_identical( lResult$Decision, 0L )
        expect_identical( lResult$ErrorCode, 0L )
        expect_equal( lResult$AnalysisTime, dTime )

        dCP <- 1 - stats::pnorm( dTailSign * ( dFinal - lResult$TestStat / sqrt( 0.5 ) ) / sqrt( 1 - 0.5 ) )
        expect_equal( dCP, 0.4 )
        # Promising zone is PromZoneMin <= CP < PromZoneMax, as in the engine.
        lAdapt$PromZoneMin <- dCP
        expect_identical( fnAnalyze( dfData, lDesign, lLook, lAdapt )$ReEstEvents, 120L )
        lAdapt$PromZoneMin <- dCP - 0.1
        lAdapt$PromZoneMax <- dCP
        expect_identical( fnAnalyze( dfData, lDesign, lLook, lAdapt )$ReEstEvents, 80L )
        lAdapt$SSRFuncScale <- 1
        lAdapt$MaxSSMultInp <- list( From = c( 0.1, 0.39 ), To = c( 0.3, 0.41 ),
            MaxSSMult = c( 6, 7 ), MaxEventsMult = c( 1.2, 2 ) )
        expect_identical( fnAnalyze( dfData, lDesign, lLook, lAdapt )$ReEstEvents, 160L )
        # Steps cover From <= CP < To.
        lAdapt$MaxSSMultInp <- list( From = dCP - 0.1, To = dCP, MaxSSMult = 7, MaxEventsMult = 2 )
        expect_identical( fnAnalyze( dfData, lDesign, lLook, lAdapt )$ReEstEvents, 80L )
        lAdapt$MaxSSMultInp$From <- dCP
        lAdapt$MaxSSMultInp$To <- dCP + 0.1
        expect_identical( fnAnalyze( dfData, lDesign, lLook, lAdapt )$ReEstEvents, 160L )
        lLook$EffBdry[ 2 ] <- dZ / sqrt( 0.5 ) + dTailSign * 4
        expect_identical( fnAnalyze( dfData, lDesign, lLook, lAdapt )$ReEstEvents, 80L )
    }
} )

test_that( "subpopulation HR outputs use engine names and numeric missing values", {
    skip_if_not_installed( "survival" )
    skip_if_not_installed( "gMCPLite" )
    fnAnalyze <- .GetCommonExampleFunction( "2ArmTimeToEventOutcomeAnalysis",
        "AnalyzeSubpopulation.R", "AnalyzeSubpopulation" )
    set.seed( 396 )
    dfData <- data.frame( ArrivalTime = 0, TreatmentID = rep( 0:1, each = 60 ),
        SurvivalTime = stats::rexp( 120 ), Region = rep( c( "A", "B" ), 60 ) )
    lDesign <- list( MaxEvents = 80L, TailType = 0L, Alpha = 0.025, NumSubPops = 2L,
        SubpopName = c( "Overall", "Region A", "Empty" ),
        SubPops = list( "Region A" = list( Region = "A" ), Empty = list( Region = "C" ) ),
        PropAlpha = rep( 1 / 3, 3 ), TransitionMatrix = matrix( 0, 3, 3 ) )
    lResult <- fnAnalyze( dfData, lDesign )
    expect_identical( names( lResult$HR ), lDesign$SubpopName )
    expect_identical( names( lResult$TestStat ), lDesign$SubpopName )
    expect_identical( names( lResult$Decision ), lDesign$SubpopName )
    expect_identical( lResult$HR, lResult$dHR )
    expect_true( all( vapply( lResult$HR, is.double, logical( 1 ) ) ) )
    expect_identical( lResult$HR$Empty, NA_real_ )
    expect_identical( lResult$TestStat$Empty, NA_real_ )
    expect_identical( lResult$ErrorCode, 0L )
} )

test_that( "beta predictive helpers accept scalar analysis priors in both bundled forms", {
    lPriors <- list( dAlphaCtrl = 2, dBetaCtrl = 3, dAlphaExp = 4, dBetaExp = 5 )
    for ( strFile in c( "AnalyzeUsingBetaBinomial.R", "ProbExpGreaterCtrlBeta.R" ) ) {
        fnPosterior <- .GetCommonExampleFunction( "2ArmBinaryOutcomeAnalysis", strFile, "ProbExpGreaterCtrlBeta" )
        set.seed( 397 )
        lPosterior <- fnPosterior( c( 0, 1 ), c( 1, 1 ), 2, 3, 4, 5 )
        expect_equal( lPosterior$dDelta, 6 / 11 - 3 / 7 )
        fnPredict <- .GetCommonExampleFunction( "2ArmBinaryOutcomeAnalysis", strFile,
            "ComputeBayesianPredictiveProbabilityWithBayesianAnalysis" )
        lPrediction <- fnPredict( c( 0, 1 ), c( 1, 1 ), 2, 3, 4, 5, 3, 3, 3, 0.5, lPriors )
        expect_length( lPrediction$predictiveProbabilityS, 1 )
        expect_gte( lPrediction$predictiveProbabilityS, 0 )
        expect_lte( lPrediction$predictiveProbabilityS, 1 )
    }
} )

test_that( "repeated-measures pipeline selection uses native calendar visit times once", {
    skip_if_not_installed( "nlme" )
    skip_if_not_installed( "rpact" )
    fnAnalyze <- .GetCommonExampleFunction( "2ArmNormalRepeatedMeasuresAnalysis",
        "Analyze.RepeatedMeasures.R", "Analyze.RepeatedMeasures" )
    set.seed( 398 )
    dfData <- data.frame( ArrivalTime = 0:39, TreatmentID = rep( 0:1, 20 ) )
    for ( nVisit in 1:3 ) {
        dfData[[ paste0( "ArrTimeVisit", nVisit ) ]] <- dfData$ArrivalTime + 5 * ( nVisit - 1 )
        dfData[[ paste0( "Response", nVisit ) ]] <- stats::rnorm( 40 ) + 0.1 * dfData$TreatmentID
    }
    dfCaptured <- NULL
    fnGLS <- nlme::gls
    local_mocked_bindings( gls = function( model, data, ... ) {
        dfCaptured <<- data
        fnGLS( model = model, data = data, ... )
    }, .package = "nlme" )
    lLook <- list( NumLooks = 2L, CurrLookIndex = 1L, CumCompleters = c( 10L, 40L ),
        InterimVisit = 3L, IncludePipeline = 1L, RejType = 0L, InfoFrac = c( 0.5, 1 ) )
    lResult <- fnAnalyze( dfData, list( NumVisit = 3L, TailType = 1L, Alpha = 0.025 ), lLook )
    # The tenth completion is at calendar time 19, when twenty subjects have enrolled.
    expect_equal( length( unique( dfCaptured$id ) ), 20 )
    vExpectedTimes <- unlist( dfData[ 1:20, paste0( "ArrTimeVisit", 1:3 ) ], use.names = FALSE )
    expect_equal( sort( dfCaptured$CalendarVisitTime ), sort( vExpectedTimes ) )
    expect_identical( lResult$ErrorCode, 0L )
} )
