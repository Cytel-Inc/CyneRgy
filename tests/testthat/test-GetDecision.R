test_that( "GetDecision maps enabled decisions for both tail directions", {
    lRightDesign <- list( TailType = 1 )
    lLeftDesign  <- list( TailType = 0 )
    lBoth        <- list( RejType = 4, CurrLookIndex = 1, NumLooks = 2 )

    expect_equal( GetDecision( "Efficacy", lRightDesign, lBoth ), 2L )
    expect_equal( GetDecision( "Efficacy", lLeftDesign, lBoth ), 1L )
    expect_equal( GetDecision( "Futility", lRightDesign, lBoth ), 3L )
    expect_equal( GetDecision( "Continue", lRightDesign, lBoth ), 0L )
} )

test_that( "GetDecision enforces enabled boundaries and final decisions", {
    lDesign      <- list( TailType = 1 )
    lEfficacyIA  <- list( RejType = 0, CurrLookIndex = 1, NumLooks = 2 )
    lFutilityIA  <- list( RejType = 1, CurrLookIndex = 1, NumLooks = 2 )
    lEfficacyFA  <- list( RejType = 0, CurrLookIndex = 2, NumLooks = 2 )
    lFutilityFA  <- list( RejType = 1, CurrLookIndex = 2, NumLooks = 2 )

    expect_error( GetDecision( "Futility", lDesign, lEfficacyIA ), "not enabled" )
    expect_error( GetDecision( "Efficacy", lDesign, lFutilityIA ), "not enabled" )
    expect_error( GetDecision( "Continue", lDesign, lEfficacyFA ), "not valid" )
    expect_equal( GetDecision( "Futility", lDesign, lEfficacyFA ), 0L )
    expect_equal( GetDecision( "Efficacy", lDesign, lFutilityFA ), 0L )
    expect_equal( GetDecision( "Efficacy", lDesign ), 2L )
} )

test_that( "GetDecision validates integration inputs", {
    expect_error( GetDecision( "Maybe", list( TailType = 1 ) ), "strDecision" )
    expect_error( GetDecision( "Efficacy", list( TailType = 2 ) ), "TailType" )
    expect_error(
        GetDecision( "Efficacy", list( TailType = 1 ), list( RejType = 8, CurrLookIndex = 1, NumLooks = 2 ) ),
        "RejType"
    )
} )

test_that( "GetDecisionString distinguishes interim and final looks", {
    lLookInfo <- list( RejType = 4 )

    expect_equal( GetDecisionString( lLookInfo, 1, 2, bIAEfficacyCondition = TRUE ), "Efficacy" )
    expect_equal( GetDecisionString( lLookInfo, 1, 2, bIAFutilityCondition = TRUE ), "Futility" )
    expect_equal( GetDecisionString( lLookInfo, 1, 2 ), "Continue" )
    expect_equal( GetDecisionString( lLookInfo, 2, 2, bFAEfficacyCondition = TRUE ), "Efficacy" )
    expect_equal( GetDecisionString( lLookInfo, 2, 2 ), "Futility" )
} )
