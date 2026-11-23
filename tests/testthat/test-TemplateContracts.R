######################################################################################################################## .
# Description: Regression checks for East Horizon integration-point template signatures.
######################################################################################################################## .

.GetTemplateFunction <- function( strTemplate ) {
    strPath <- system.file( "Templates", strTemplate, package = "CyneRgy" )
    strCode <- paste( readLines( strPath, warn = FALSE ), collapse = "\n" )
    strCode <- gsub( "{{FUNCTION_NAME}}", "TemplateFunction", strCode, fixed = TRUE )
    envTemplate <- new.env( parent = baseenv() )
    eval( parse( text = strCode ), envir = envTemplate )
    envTemplate$TemplateFunction
}

.GetTemplateText <- function( strTemplate ) {
    strPath <- system.file( "Templates", strTemplate, package = "CyneRgy" )
    paste( readLines( strPath, warn = FALSE ), collapse = "\n" )
}

.GetDocumentedMembers <- function( strTemplate, strParameter ) {
    vLines <- readLines(
        system.file( "Templates", strTemplate, package = "CyneRgy" ),
        warn = FALSE
    )
    nStart <- grep( paste0( "^#' @param ", strParameter, "\\b" ), vLines )
    expect_length( nStart, 1L )

    vEnds <- grep( "^#' @(param|return)\\b", vLines )
    vEnds <- vEnds[ vEnds > nStart ]
    nEnd <- if ( length( vEnds ) ) vEnds[[ 1 ]] - 1L else length( vLines )
    strDocumentation <- paste( vLines[ nStart:nEnd ], collapse = "\n" )
    vMatches <- regmatches( strDocumentation, gregexpr( "\\\\item\\{[^}]+\\}", strDocumentation ) )[[ 1 ]]
    sub( "\\}$", "", sub( "^\\\\item\\{", "", vMatches ) )
}

.ExpectDocumentedMembers <- function( strTemplate, strParameter, vExpected ) {
    vActual <- .GetDocumentedMembers( strTemplate, strParameter )
    # Source allocators include conditional/NA fields; documentation need not enumerate all of them.
    expect_equal( setdiff( vActual, vExpected ), character(),
        info = paste( strTemplate, strParameter, "contains a member absent from the source contract" ) )
}

test_that( "optional enrollment arguments preserve positional and native named calls", {
    vTemplates <- c( "SimulatePatientOutcome.Binary.MAMS.R", "SimulatePatientOutcome.Continuous.MAMS.R",
        "SimulatePatientOutcome.DEP.R", "SimulatePatientOutcome.RepeatedMeasures.R" )
    for ( strTemplate in vTemplates ) {
        fnTemplate <- .GetTemplateFunction( strTemplate )
        fnProbe <- as.function( c( formals( fnTemplate ), quote( {
            list( ArrivalTime = ArrivalTime, TreatmentID = TreatmentID )
        } ) ) )
        expect_identical( fnProbe( 2L, 3L, c( 10, 20 ), c( 0L, 1L ) ),
            list( ArrivalTime = c( 10, 20 ), TreatmentID = c( 0L, 1L ) ), info = strTemplate )
        expect_identical( fnProbe( TreatmentID = c( 0L, 1L ) ),
            list( ArrivalTime = NULL, TreatmentID = c( 0L, 1L ) ), info = strTemplate )
    }
} )

test_that( "standard response templates tolerate Design calls without enrollment", {
    fnBinary <- .GetTemplateFunction( "SimulatePatientOutcome.Binary.R" )
    expect_identical( fnBinary( NumSub = 2L, NumArm = 2L, TreatmentID = c( 0L, 1L ), PropResp = c( 0, 1 ) ),
        list( Response = c( 0, 0 ), ErrorCode = 0L ) )
    fnContinuous <- .GetTemplateFunction( "SimulatePatientOutcome.Continuous.R" )
    expect_identical( fnContinuous( NumSub = 2L, TreatmentID = c( 0L, 1L ),
        Mean = c( 10, 20 ), StdDev = c( 0, 0 ) ),
        list( Response = c( 10, 20 ), ErrorCode = 0L ) )
} )

test_that( "RM all-completer defaults contain valid equivalent dropout representations", {
    fnDropout <- .GetTemplateFunction( "Dropout.RepeatedMeasures.R" )
    expect_no_warning( lResult <- fnDropout( NumSub = 2L, NumArm = 2L, TreatmentID = c( 0L, 1L ),
        NumVisit = 3L, VisitTime = c( 1, 2, 4 ), DropMethod = 2L, ByTime = 4,
        DropParamControl = 0, DropParamTrt = 0 ) )
    expect_identical( lResult$DropoutVisitID, rep( 3L, 2L ) )
    expect_identical( lResult$DropOutTime, rep( Inf, 2L ) )
    expect_true( all( vapply( lResult[ paste0( "CensorInd", 1:3 ) ],
        identical, logical( 1 ), rep( 1L, 2L ) ) ) )
} )

test_that( "subpopulation decisions include full population and arbitrary engine names", {
    fnAnalyze <- .GetTemplateFunction( "Analyze.TimeToEvent.Stratification.Subpopulation.R" )
    vNames <- c( "All randomized subjects", "Marker positive", "Marker negative" )
    lResult <- fnAnalyze( SimData = data.frame(), DesignParam = list( SubpopName = vNames ) )
    expect_identical( lResult$Decision, stats::setNames( as.list( rep( 0L, 3L ) ), vNames ) )
} )

.ExpectTemplateContract <- function( strTemplate, vEngineArgs, vTemplateArgs = vEngineArgs ) {
    fnTemplate <- .GetTemplateFunction( strTemplate )
    expect_identical( names( formals( fnTemplate ) ), vTemplateArgs, info = strTemplate )

    fnProbe <- as.function( c( formals( fnTemplate ), quote( {
        mget( names( formals( sys.function() ) ), envir = environment(), inherits = FALSE )
        TRUE
    } ) ) )
    lArgs <- stats::setNames( rep( list( NULL ), length( vEngineArgs ) ), vEngineArgs )
    expect_true( do.call( fnProbe, lArgs ), info = strTemplate )
}

test_that( "all templates accept their engine-supplied arguments", {
    lContracts <- list(
        "Analyze.Binary.MAMS.R" = c( "SimData", "DesignParam", "LookInfo", "UserParam" ),
        "Analyze.Binary.R" = c( "SimData", "DesignParam", "LookInfo", "UserParam" ),
        "Analyze.Binary.SSR.R" = c( "SimData", "DesignParam", "LookInfo", "AdaptInfo", "UserParam" ),
        "Analyze.Continuous.MAMS.R" = c( "SimData", "DesignParam", "LookInfo", "UserParam" ),
        "Analyze.Continuous.R" = c( "SimData", "DesignParam", "LookInfo", "UserParam" ),
        "Analyze.Continuous.SSR.R" = c( "SimData", "DesignParam", "LookInfo", "AdaptInfo", "UserParam" ),
        "Analyze.DEP.R" = c( "SimData", "DesignParam", "LookInfo", "UserParam" ),
        "Analyze.DoseFinding.Continuous.R" = list(
            Engine = c( "SimData", "DesignParam", "LookInfo", "UserParam", "OutList" ),
            Template = c( "SimData", "DesignParam", "LookInfo", "OutList", "UserParam" )
        ),
        "Analyze.RepeatedMeasures.R" = c( "SimData", "DesignParam", "LookInfo", "UserParam" ),
        "Analyze.TimeToEvent.MAMS.R" =
            c( "SimData", "DesignParam", "LookInfo", "UserParam", "OutList" ),
        "Analyze.TimeToEvent.R" = c( "SimData", "DesignParam", "LookInfo", "UserParam" ),
        "Analyze.TimeToEvent.SSR.R" =
            c( "SimData", "DesignParam", "LookInfo", "AdaptInfo", "UserParam" ),
        "Analyze.TimeToEvent.Stratification.R" =
            c( "SimData", "DesignParam", "LookInfo", "UserParam" ),
        "Analyze.TimeToEvent.Stratification.Subpopulation.R" =
            c( "SimData", "DesignParam", "LookInfo", "UserParam" ),
        "Decision.DEP.R" = list(
            Engine = c( "SimData", "DesignParam", "TestStat", "LookInfo", "UserParam", "OutList" ),
            Template = c( "SimData", "DesignParam", "LookInfo", "TestStat", "OutList", "UserParam" )
        ),
        "Design.MEP.R" =
            c( "SimData", "AnalysisData", "DataSummary", "LookInfo", "DesignParam", "OutList", "UserParam" ),
        "Dropout.BinaryAndContinuous.MAMS.R" =
            c( "NumSub", "ProbDrop", "NumArm", "TreatmentID", "UserParam" ),
        "Dropout.BinaryAndContinuous.R" = c( "NumSub", "ProbDrop", "UserParam" ),
        "Dropout.RepeatedMeasures.R" = c( "NumSub", "NumArm", "NumVisit", "VisitTime", "TreatmentID",
            "DropMethod", "ByTime", "DropParamControl", "DropParamTrt", "UserParam" ),
        "Dropout.Survival.R" = c( "NumSub", "NumArm", "TreatmentID", "DropMethod", "NumPrd", "PrdTime",
            "DropParam", "UserParam" ),
        "GenerateArrivalTimes.MEP.R" = list(
            Engine = c( "NumPat", "NumPrd", "PrdStart", "AccrRate", "UserParam" ),
            Template = c( "NumPat", "NumPrd", "PrdStart", "AccrRate", "UserParam" )
        ),
        "GenerateArrivalTimes.R" = c( "NumSub", "NumPrd", "PrdStart", "AccrRate", "UserParam" ),
        "Initialize.R" = c( "Seed", "UserParam" ),
        "Randomization.R" = list(
            Engine = c( "NumSub", "NumArm", "AllocRatio", "UserParam" ),
            Template = c( "NumSub", "NumArms", "AllocRatio", "UserParam" )
        ),
        "SimulatePatientOutcome.Binary.MAMS.R" = list(
            Engine = c( "NumSub", "NumArm", "TreatmentID", "PropResp", "UserParam" ),
            Template = c( "NumSub", "NumArm", "ArrivalTime", "TreatmentID", "PropResp", "UserParam" )
        ),
        "SimulatePatientOutcome.Binary.R" =
            c( "NumSub", "NumArm", "ArrivalTime", "TreatmentID", "PropResp", "UserParam" ),
        "SimulatePatientOutcome.Continuous.MAMS.R" = list(
            Engine = c( "NumSub", "NumArms", "TreatmentID", "Mean", "StdDev", "UserParam" ),
            Template = c( "NumSub", "NumArms", "ArrivalTime", "TreatmentID", "Mean", "StdDev", "UserParam" )
        ),
        "SimulatePatientOutcome.Continuous.R" =
            c( "NumSub", "ArrivalTime", "TreatmentID", "Mean", "StdDev", "UserParam" ),
        "SimulatePatientOutcome.DEP.R" = list(
            Engine = c( "NumSub", "NumArm", "TreatmentID", "EndpointType", "EndpointName", "Correlation",
                "SurvMethod", "NumPrd", "PrdTime", "SurvParam", "PropResp", "UserParam" ),
            Template = c( "NumSub", "NumArm", "ArrivalTime", "TreatmentID", "EndpointType", "EndpointName",
                "Correlation", "SurvMethod", "NumPrd", "PrdTime", "SurvParam", "PropResp", "UserParam" )
        ),
        "SimulatePatientOutcome.MEP.R" = c( "NumPat", "NumArms", "TreatmentID", "ArrivalTime",
            "EndpointType", "EndpointName", "RespParams", "Correlation", "UserParam" ),
        "SimulatePatientOutcome.RepeatedMeasures.R" = list(
            Engine = c( "NumSub", "NumVisit", "TreatmentID", "Inputmethod", "VisitTime", "MeanControl",
                "MeanTrt", "StdDevControl", "StdDevTrt", "CorrMat", "UserParam" ),
            Template = c( "NumSub", "NumVisit", "ArrivalTime", "TreatmentID", "Inputmethod", "VisitTime",
                "MeanControl", "MeanTrt", "StdDevControl", "StdDevTrt", "CorrMat", "UserParam" )
        ),
        "SimulatePatientOutcome.TimeToEvent.MAMS.R" = c( "NumSub", "NumArm", "ArrivalTime", "TreatmentID",
            "SurvMethod", "NumPrd", "PrdTime", "SurvParam", "UserParam" ),
        "SimulatePatientOutcome.TimeToEvent.R" = c( "NumSub", "NumArm", "ArrivalTime", "TreatmentID",
            "SurvMethod", "NumPrd", "PrdTime", "SurvParam", "UserParam" ),
        "SimulatePatientOutcome.TimeToEvent.Stratification.R" =
            c( "NumSub", "NumArm", "ArrivalTime", "TreatmentID", "StratumID", "SurvMethod", "NumPrd",
                "PrdTime", "SurvParam", "UserParam" ),
        "TreatmentSelection.R" = c( "SimData", "DesignParam", "LookInfo", "UserParam" )
    )

    strTemplateDir <- system.file( "Templates", package = "CyneRgy" )
    expect_length( lContracts, length( list.files( strTemplateDir, pattern = "[.]R$" ) ) )
    for ( strTemplate in names( lContracts ) ) {
        lContract <- lContracts[[ strTemplate ]]
        if ( is.list( lContract ) ) {
            .ExpectTemplateContract( strTemplate, lContract$Engine, lContract$Template )
        } else {
            .ExpectTemplateContract( strTemplate, lContract )
        }
    }
} )

test_that( "documented engine list and data-frame members use source names", {
    vNativeAnalysisTemplates <- c(
        "Analyze.Binary.MAMS.R", "Analyze.Binary.R", "Analyze.Binary.SSR.R",
        "Analyze.Continuous.MAMS.R", "Analyze.Continuous.R", "Analyze.Continuous.SSR.R",
        "Analyze.DEP.R", "Analyze.DoseFinding.Continuous.R", "Analyze.RepeatedMeasures.R",
        "Analyze.TimeToEvent.MAMS.R", "Analyze.TimeToEvent.R", "Analyze.TimeToEvent.SSR.R",
        "Analyze.TimeToEvent.Stratification.R", "Analyze.TimeToEvent.Stratification.Subpopulation.R",
        "Decision.DEP.R", "TreatmentSelection.R"
    )
    for ( strTemplate in vNativeAnalysisTemplates ) {
        expect_match( .GetTemplateText( strTemplate ), "\\\\item\\{TestID\\}", info = strTemplate )
    }

    vSurvivalTemplates <- c(
        "Analyze.TimeToEvent.MAMS.R", "Analyze.TimeToEvent.R", "Analyze.TimeToEvent.SSR.R",
        "Analyze.TimeToEvent.Stratification.R", "Analyze.TimeToEvent.Stratification.Subpopulation.R"
    )
    for ( strTemplate in vSurvivalTemplates ) {
        strTemplateText <- .GetTemplateText( strTemplate )
        expect_match( strTemplateText, "\\\\item\\{ClndrRespTime\\}", info = strTemplate )
        expect_match( strTemplateText, "\\\\item\\{CensorInd\\}", info = strTemplate )
        expect_match( strTemplateText, "\\\\item\\{CensorIndOrg\\}", info = strTemplate )
    }

    for ( strTemplate in c( "Analyze.DEP.R", "Decision.DEP.R" ) ) {
        strTemplateText <- .GetTemplateText( strTemplate )
        for ( strMember in c( "EffBdryLwr", "EffBdryUpr", "FutBdryLwr", "FutBdryUpr" ) ) {
            expect_match( strTemplateText, paste0( "\\\\item\\{", strMember, "\\}" ), info = strTemplate )
        }
        expect_false( grepl( "\\\\item\\{(Eff|Fut)Bdry(Lower|Upper)\\}", strTemplateText ),
            info = strTemplate )
    }

    for ( strTemplate in c(
        "Analyze.Binary.SSR.R", "Analyze.Continuous.SSR.R", "Analyze.TimeToEvent.SSR.R"
    ) ) {
        strTemplateText <- .GetTemplateText( strTemplate )
        for ( strMember in c(
            "PromZonePiC", "PromZoneHR", "TestStat", "Delta", "StdError", "MaxSSMultInp"
        ) ) {
            expect_match( strTemplateText, paste0( "\\\\item\\{", strMember, "\\}" ), info = strTemplate )
        }
        expect_match( strTemplateText, "MaxEventsMult", fixed = TRUE, info = strTemplate )
    }

    strMEPTemplate <- .GetTemplateText( "Design.MEP.R" )
    for ( strMember in c( "PatID", "NumEndpointLooks", "ACI.EPNAME", "AFT.EPNAME", "ADI.EPNAME" ) ) {
        expect_match( strMEPTemplate, strMember, fixed = TRUE )
    }
    expect_false( grepl( "\\\\item\\{PatId\\}", strMEPTemplate ) )
} )

test_that( "native analysis documented members belong to source-defined lists", {
    vBaseDesign <- c(
        "TestID", "Alpha", "UpperAlpha", "LowerAlpha", "TrialType", "TestType", "TailType",
        "CriticalPoint", "UpperCriticalPoint", "LowerCriticalPoint", "SampleSize", "MaxCompleters",
        "RespLag", "MaxEvents", "FollowUpType", "FollowUpDur", "TestStatType", "VarType", "Sigma"
    )
    vNonMAMSDesign <- c(
        "AllocInfo", "LookFixOption", "MaxStudyDur", "SigmaD", "SigmaLogRatio", "CoeffVar",
        "TrtEffNull", "UpperEquiLimit", "LowerEquiLimit", "EquiMargin", "MuC", "PiC"
    )
    vMAMSDesign <- c(
        "InitialAllocInfo", "PValCombMethod", "w1", "w2", "MultAdjMethod", "NumTreatments",
        "AlphaProp", "TestSeq", "IsArmPresent", "UpdatedAllocInfo"
    )
    vSurvivalDesign <- c( "HFParam1", "HFParam2", "NumHzrdPrd", "PrdAt", "LambdaC" )
    vLookInfo <- c(
        "NumLooks", "CurrLookIndex", "InfoFrac", "CumAlpha", "CumAlphaUpper", "CumAlphaLower",
        "CumCompleters", "RejType", "EffBdryScale", "EffBdry", "EffBdryUpper", "EffBdryLower",
        "FutBdryScale", "FutBdry", "FutBdryUpper", "FutBdryLower", "BindingType"
    )

    lContracts <- list(
        "Analyze.Binary.R" = list( DesignParam = c( vBaseDesign, vNonMAMSDesign,
            "NumPop", "Scores", "NumCat", "NumGrp", "CatPropNull" ),
            LookInfo = c( vLookInfo, "CPDeltaOption" ) ),
        "Analyze.Binary.SSR.R" = list( DesignParam = c( vBaseDesign, vNonMAMSDesign,
            "NumPop", "Scores", "NumCat", "NumGrp", "CatPropNull" ),
            LookInfo = c( vLookInfo, "CPDeltaOption" ) ),
        "Analyze.Continuous.R" = list( DesignParam = c( vBaseDesign, vNonMAMSDesign ),
            LookInfo = c( vLookInfo, "CPDeltaOption" ) ),
        "Analyze.Continuous.SSR.R" = list( DesignParam = c( vBaseDesign, vNonMAMSDesign ),
            LookInfo = c( vLookInfo, "CPDeltaOption" ) ),
        "Analyze.TimeToEvent.R" = list(
            DesignParam = c( vBaseDesign, vNonMAMSDesign, vSurvivalDesign ),
            LookInfo = c( vLookInfo, "CumEvents", "LookTime", "CPDeltaOption" )
        ),
        "Analyze.TimeToEvent.SSR.R" = list(
            DesignParam = c( vBaseDesign, vNonMAMSDesign, vSurvivalDesign ),
            LookInfo = c( vLookInfo, "CumEvents", "LookTime", "CPDeltaOption" )
        ),
        "Analyze.TimeToEvent.Stratification.R" = list(
            DesignParam = c( vBaseDesign, vNonMAMSDesign, vSurvivalDesign, "NumStratFactors",
                "StratFactors", "TestStratFactors" ),
            LookInfo = c( vLookInfo, "CumEvents", "LookTime", "CPDeltaOption" )
        ),
        "Analyze.TimeToEvent.Stratification.Subpopulation.R" = list(
            DesignParam = c( vBaseDesign, vNonMAMSDesign, vSurvivalDesign, "NumStratFactors",
                "StratFactors", "TestStratFactors", "NumSubPops", "SubPops", "SubpopName", "WinCond",
                "PlanEndTrial", "TransitionMatrix", "PropAlpha" ),
            LookInfo = c( vLookInfo, "CumEvents", "LookTime", "CPDeltaOption" )
        ),
        "Analyze.RepeatedMeasures.R" = list(
            DesignParam = c( vBaseDesign, vNonMAMSDesign, "NumVisit", "VisitStatus", "VisitTime",
                "PrimContrastCoeff", "SecContrastCoeff", "DropImp" ),
            LookInfo = c( vLookInfo, "InterimVisit", "FutContrast", "IncludePipeline", "CPDeltaOption" )
        ),
        "Analyze.Binary.MAMS.R" = list(
            DesignParam = c( vBaseDesign, vMAMSDesign ),
            LookInfo = vLookInfo
        ),
        "Analyze.Continuous.MAMS.R" = list(
            DesignParam = c( vBaseDesign, vMAMSDesign ),
            LookInfo = vLookInfo
        ),
        "Analyze.TimeToEvent.MAMS.R" = list(
            DesignParam = c( vBaseDesign, vMAMSDesign, vSurvivalDesign ),
            LookInfo = c( vLookInfo, "CumEvents", "LookTime" )
        ),
        "Analyze.DoseFinding.Continuous.R" = list(
            DesignParam = c( vBaseDesign, vMAMSDesign ),
            LookInfo = c( vLookInfo, "PoCScale", "PoCThreshold" )
        ),
        "TreatmentSelection.R" = list(
            DesignParam = c( vBaseDesign, vMAMSDesign ),
            LookInfo = c( vLookInfo, "CumEvents", "LookTime" )
        )
    )

    for ( strTemplate in names( lContracts ) ) {
        for ( strParameter in names( lContracts[[ strTemplate ]] ) ) {
            .ExpectDocumentedMembers(
                strTemplate,
                strParameter,
                lContracts[[ strTemplate ]][[ strParameter ]]
            )
        }
    }
} )

test_that( "DEP documented members belong to source-defined lists", {
    vDesignParam <- c(
        "EndpointType", "EndpointName", "WinCond", "PlanEndTrial", "Alpha", "UpperAlpha",
        "LowerAlpha", "SampleSize", "TestID", "MultAdj", "TestOrder", "AllocInfo", "VarType",
        "TailType", "FollowUpType", "FollowUpDur", "TrialType", "CriticalPoint",
        "UpperCriticalPoint", "LowerCriticalPoint", "MaxEvents", "MaxCompleters", "TrtEffNull",
        "AlphaAlloc", "TargetSSFA", "TestStat"
    )
    vLookInfo <- c(
        "NumLooks", "CurrLookIndex", "SyncInterim", "NumEndpointLooks", "InputInfoFrac",
        "CumCompleters", "CumEvents", "RejType", "EffBdryScale", "EffBdry", "EffBdryUpr",
        "EffBdryLwr", "FutBdryScale", "FutBdry", "FutBdryUpr", "FutBdryLwr", "BindingType"
    )

    for ( strTemplate in c( "Analyze.DEP.R", "Decision.DEP.R" ) ) {
        .ExpectDocumentedMembers( strTemplate, "DesignParam", vDesignParam )
        .ExpectDocumentedMembers( strTemplate, "LookInfo", vLookInfo )
    }
    .ExpectDocumentedMembers( "Decision.DEP.R", "TestStat", "EndpointName" )
} )

test_that( "analysis templates document source-defined SimData columns", {
    vResponseData <- c(
        "ArrivalTime", "TreatmentID", "Response", "ClndrRespTime", "CensorInd", "CensorIndOrg",
        "LkRespTime", "LkCensorIndctr", "DropOutTime", "ArrvRandNum", "AllocRandNum",
        "RespRandNum", "DropOutPrdRandNum", "DropOutDORandNum", "CensorIndRandNum"
    )
    for ( strTemplate in c(
        "Analyze.Binary.MAMS.R", "Analyze.Binary.R", "Analyze.Binary.SSR.R",
        "Analyze.Continuous.MAMS.R", "Analyze.Continuous.R", "Analyze.Continuous.SSR.R"
    ) ) {
        .ExpectDocumentedMembers( strTemplate, "SimData", vResponseData )
    }

    vSurvivalData <- c(
        "ArrivalTime", "TreatmentID", "SurvivalTime", "ClndrRespTime", "CensorInd",
        "CensorIndOrg", "DropOutTime", "LkRespTime", "LkCensorIndctr", "ArrvRandNum",
        "AllocRandNum", "RespRandNum", "RespPrdRandNum", "DropOutPrdRandNum",
        "DropOutDORandNum", "CensorIndRandNum"
    )
    for ( strTemplate in c(
        "Analyze.TimeToEvent.MAMS.R", "Analyze.TimeToEvent.R", "Analyze.TimeToEvent.SSR.R"
    ) ) {
        .ExpectDocumentedMembers( strTemplate, "SimData", vSurvivalData )
    }
    for ( strTemplate in c(
        "Analyze.TimeToEvent.Stratification.R", "Analyze.TimeToEvent.Stratification.Subpopulation.R"
    ) ) {
        .ExpectDocumentedMembers(
            strTemplate,
            "SimData",
            c( vSurvivalData, "StratumIndex", "StratumID", "StratumRandNum" )
        )
    }

    vDEPData <- c(
        "ArrivalTime", "TreatmentID", "Response1", "Response2", "ClndrRespTime", "ClndrRespTime2",
        "LkCensorIndctr", "LkCensorIndctr2", "CensorIndOrg", "CensorIndOrg2", "DropOutTime",
        "NormalResponse1", "NormalResponse2", "LkRespTime", "LkRespTime2", "ArrvRandNum",
        "AllocRandNum", "DropOutPrdRandNum", "DropOutDORandNum", "CensorIndRandNum"
    )
    for ( strTemplate in c( "Analyze.DEP.R", "Decision.DEP.R" ) ) {
        .ExpectDocumentedMembers( strTemplate, "SimData", vDEPData )
    }

    .ExpectDocumentedMembers(
        "Analyze.DoseFinding.Continuous.R",
        "SimData",
        vResponseData
    )
    .ExpectDocumentedMembers(
        "TreatmentSelection.R",
        "SimData",
        c( "ArrivalTime", "TreatmentID", "Response", "SurvivalTime", "ClndrRespTime", "DropOutTime",
            "CensorInd", "CensorIndOrg", "LkRespTime", "LkCensorIndctr", "ArrvRandNum",
            "AllocRandNum", "RespRandNum", "RespPrdRandNum", "DropOutPrdRandNum",
            "DropOutDORandNum", "CensorIndRandNum" )
    )
    .ExpectDocumentedMembers(
        "Analyze.RepeatedMeasures.R",
        "SimData",
        c( "ArrivalTime", "TreatmentID", "Response1, ..., ResponseNumVisit",
            "CensorInd1, ..., CensorIndNumVisit", "DropOutTime", "DropoutVisitID",
            "ArrTimeVisit[VisitID]", "RespRandNum1, ..., RespRandNumNumVisit",
            "ClndrRespTimeVisit[VisitID]", "LkRespTime", "LkCensorIndctr", "ArrvRandNum",
            "AllocRandNum", "DropOutPrdRandNum", "DropOutDORandNum", "CensorIndRandNum" )
    )

    .ExpectDocumentedMembers(
        "Design.MEP.R",
        "SimData",
        c( "SimID", "PatID", "SiteID", "RegionID", "ArrivalTime", "TreatmentID", "RespTime",
            "EPRuleID", "DropoutTime", "TResponse.`EPNAME`", "CensorID.`EPNAME`",
            "Response.`EPNAME`", "CalRespT.`EPNAME`", "DropoutID.`EPNAME`" )
    )
    strMEPTemplate <- .GetTemplateText( "Design.MEP.R" )
    for ( strColumn in c( "ACI.EPNAME", "AFT.EPNAME", "ADI.EPNAME" ) ) {
        expect_match( strMEPTemplate, strColumn, fixed = TRUE )
    }
} )

test_that( "adaptive and MEP templates document source-defined nested members", {
    vAdaptInfo <- c(
        "AdaptMethod", "AdaptInterimScale", "AdaptInterimNum", "AdaptInterimPerc", "StudyDurationUL",
        "WaldCPThreshold", "EnrollAdaptScale", "EnrollAdaptMult", "EnrollAdaptRate", "PromZoneScale",
        "PromZoneSigma", "PromZonePiC", "PromZoneHR", "PromZoneDelta", "PromZoneMin", "PromZoneMax",
        "SSRFuncScale", "NumSteps", "MaxSSMultInp", "TargetCP", "OrigCP", "TestStat", "Delta",
        "StdError"
    )
    for ( strTemplate in c(
        "Analyze.Binary.SSR.R", "Analyze.Continuous.SSR.R", "Analyze.TimeToEvent.SSR.R"
    ) ) {
        .ExpectDocumentedMembers( strTemplate, "AdaptInfo", vAdaptInfo )
        strTemplateText <- .GetTemplateText( strTemplate )
        for ( strMember in c( "From", "To", "MaxSSMult", "MaxEventsMult" ) ) {
            expect_match( strTemplateText, strMember, fixed = TRUE )
        }
    }

    .ExpectDocumentedMembers(
        "Design.MEP.R",
        "DataSummary",
        c( "AvgFollowupTime", "MedianFollowupTime", "Dropouts", "Dropouts0", "Dropouts1", "Censored",
            "Censored0", "Censored1", "Pendings", "Pendings0", "Pendings1", "Events", "Events0",
            "Events1", "Completers", "Completers0", "Completers1", "HR", "HR0", "HR1", "Mean0",
            "Mean1", "SD0", "SD1", "Delta", "Prop0", "Prop1" )
    )
    .ExpectDocumentedMembers(
        "Design.MEP.R",
        "LookInfo",
        c( "AnalysisTime", "LookNum", "TestStatisticsOutputs", "InfoFrac", "LastLookDecision",
            "EfficacyBoundaryPScale", "EPStatus" )
    )
    .ExpectDocumentedMembers(
        "Design.MEP.R",
        "DesignParam",
        c( "TotalLooks", "EndpointName", "EndpointType", "TailType", "NumPat", "AllocRatio",
            "TrialType", "Alpha", "TestStatistics", "TargetInformation", "MultiplicityDetails",
            "EffFlg", "NumEndpointLooks", "FutFlg", "FutThrsld", "EffSpending", "WinCondCriteria" )
    )
} )
