test_that( "AddUnrenderToList appends unknown template tags and keeps existing values", {
    lTags <- list( KNOWN = "value" )

    lResult <- CyneRgy:::AddUnrenderToList( "{{KNOWN}} {{NEW}} {{}} {{NEW}}", lTags )

    expect_identical( lResult$KNOWN, "value" )
    expect_identical( lResult$NEW, "{{NEW}}" )
    expect_equal( sum( names( lResult ) == "NEW" ), 1 )
} )


test_that( "WhiskerKeepUnrender preserves unknown tags while rendering known tags", {
    skip_if_not_installed( "whisker" )

    strRendered <- CyneRgy:::WhiskerKeepUnrender(
        "Hello {{name}} {{missing}}",
        list( name = "World" )
    )

    expect_identical( strRendered, "Hello World {{missing}}" )
} )


test_that( "SimulateAccrualTimesWithConstantRate stays within the requested period", {
    set.seed( 123 )

    vTimes <- CyneRgy:::SimulateAccrualTimesWithConstantRate(
        dPatsPerUnitTime = 8,
        dPeriodStartTime = 10,
        dQtyOfUnitsOfTime = 2
    )

    expect_type( vTimes, "double" )
    expect_true( all( diff( vTimes ) >= 0 ) )
    expect_true( all( vTimes >= 10 ) )
    expect_true( all( vTimes < 12 ) )
} )


test_that( "PlotExampleFlowchart returns ggplot objects for used and empty integration points", {
    skip_if_not_installed( "ggplot2" )

    pUsed <- PlotExampleFlowchart(
        lIntPoints = list( "Response" = c( "Simulate outcomes", "Analyze outcomes" ) )
    )
    pEmpty <- PlotExampleFlowchart()

    expect_s3_class( pUsed, "ggplot" )
    expect_s3_class( pEmpty, "ggplot" )
} )