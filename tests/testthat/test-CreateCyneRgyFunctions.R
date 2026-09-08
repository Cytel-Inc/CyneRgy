test_that( "CreateCyneRgyFunction lists templates and creates a tagged script", {
    vTemplates <- suppressMessages( CreateCyneRgyFunction() )
    expect_contains( vTemplates, "Analyze.Binary" )

    strDirectory <- tempfile( "CyneRgy-function-" )
    dir.create( strDirectory )
    on.exit( unlink( strDirectory, recursive = TRUE ), add = TRUE )

    expect_message(
        strFile <- CreateCyneRgyFunction( "Analyze.Binary", "MyAnalysis", strDirectory, bOpen = FALSE ),
        "Created CyneRgy function"
    )
    expect_true( file.exists( strFile ) )
    expect_match( paste( readLines( strFile, warn = FALSE ), collapse = "\n" ), "MyAnalysis" )
} )

test_that( "CreateCyneRgyExample creates a project by default and can omit it", {
    strDirectory <- tempfile( "CyneRgy-example-" )
    dir.create( strDirectory )
    on.exit( unlink( strDirectory, recursive = TRUE ), add = TRUE )

    strExample <- suppressMessages(
        CreateCyneRgyExample( "Analyze.Binary", "ExampleWithProject", strDirectory, bOpen = FALSE )
    )
    expect_true( file.exists( file.path( strExample, "Description.Rmd" ) ) )
    expect_true( file.exists( file.path( strExample, "R", "ExampleWithProject.R" ) ) )
    expect_true( file.exists( file.path( strExample, "ExampleWithProject.Rproj" ) ) )
    expect_match( paste( readLines( file.path( strExample, "Description.Rmd" ), warn = FALSE ), collapse = "\n" ),
                  "ExampleWithProject.Rproj", fixed = TRUE )

    strExampleWithoutProject <- suppressMessages(
        CreateCyneRgyExample( "Analyze.Binary", "ExampleWithoutProject", strDirectory,
                              bCreateProject = FALSE, bOpen = FALSE )
    )
    expect_length( list.files( strExampleWithoutProject, pattern = "\\.Rproj$" ), 0 )
} )
