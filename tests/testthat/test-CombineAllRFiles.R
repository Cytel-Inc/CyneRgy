test_that( "CombineAllRFiles combines files in order and ignores its output", {
    strDirectory <- tempfile( "CyneRgy-combine-" )
    dir.create( strDirectory )
    on.exit( unlink( strDirectory, recursive = TRUE ), add = TRUE )

    writeLines( "FunctionB <- function() 2", file.path( strDirectory, "B.R" ) )
    writeLines( "FunctionA <- function() 1", file.path( strDirectory, "A.r" ) )
    writeLines( "ignore", file.path( strDirectory, "notes.txt" ) )

    lContents <- CombineAllRFiles( strDirectory = strDirectory )
    expect_equal( lContents$nQtyCombinedFiles, 2 )
    expect_lt( regexpr( "A.r", lContents$strCombinedContents ), regexpr( "B.R", lContents$strCombinedContents ) )
    expect_match( lContents$strCombinedContents, "FunctionA <- function\\(\\) 1" )

    strOutput <- file.path( strDirectory, "Combined.R" )
    lWritten  <- CombineAllRFiles( strOutput, strDirectory )
    expect_equal( lWritten$nQtyCombinedFiles, 2 )
    expect_true( file.exists( strOutput ) )

    lRewritten <- CombineAllRFiles( strOutput, strDirectory )
    expect_equal( lRewritten$nQtyCombinedFiles, 2 )
} )

test_that( "CombineAllRFiles handles missing directories", {
    lResult <- CombineAllRFiles( strDirectory = tempfile( "missing-" ) )
    expect_equal( lResult$nQtyCombinedFiles, 0L )
    expect_equal( lResult$strCombinedContents, "" )
} )
