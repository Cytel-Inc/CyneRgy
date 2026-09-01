test_that( "ReplaceTagsInFile replaces known tags and preserves unknown tags on every line", {
    strFile <- tempfile( fileext = ".R" )
    on.exit( unlink( strFile ), add = TRUE )
    writeLines( c( "{{KNOWN}}", "{{UNKNOWN}}" ), strFile )

    expect_true( CyneRgy:::ReplaceTagsInFile( strFile, "KNOWN", "value" ) )
    expect_equal( readLines( strFile, warn = FALSE ), c( "value", "{{UNKNOWN}}" ) )
    expect_error( CyneRgy:::ReplaceTagsInFile( strFile, c( "A", "B" ), "value" ), "same length" )
} )
