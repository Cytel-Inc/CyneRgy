test_that( "RunExample lists, resolves and copies examples without opening an IDE", {
    strDefaultDirectory <- tempfile( "CyneRgy-default-examples-" )
    vOriginalOptions    <- options( CyneRgy.examples.path = strDefaultDirectory )
    on.exit( options( vOriginalOptions ), add = TRUE )
    on.exit( unlink( strDefaultDirectory, recursive = TRUE ), add = TRUE )

    vExamples <- suppressMessages( RunExample() )
    expect_contains( vExamples, "TreatmentSelection" )

    expect_message(
        strDefaultPath <- RunExample( "TreatmentSelection", bOpen = FALSE ),
        "CyneRgy example"
    )
    expect_true( dir.exists( strDefaultPath ) )

    strDirectory <- tempfile( "CyneRgy-run-example-" )
    dir.create( strDirectory )
    on.exit( unlink( strDirectory, recursive = TRUE ), add = TRUE )

    strCopiedPath <- suppressMessages(
        RunExample( "TreatmentSelection", strDirectory = strDirectory, bOpen = FALSE )
    )
    expect_true( dir.exists( strCopiedPath ) )
    expect_true( file.exists( file.path( strCopiedPath, "Description.Rmd" ) ) )
    expect_identical( suppressMessages( RunExample( strCopiedPath, bOpen = FALSE ) ), strCopiedPath )
    expect_error( RunExample( "NotAnExample", bOpen = FALSE ), "Unknown CyneRgy example" )
} )

test_that( "RunExample copies installed examples outside the package library and reuses them", {
    strSourceExample       <- file.path( system.file( "Examples", package = "CyneRgy" ), "TreatmentSelection" )
    strFakePackagePath     <- tempfile( "CyneRgy-installed-package-" )
    strFakeExamplesPath    <- file.path( strFakePackagePath, "Examples" )
    strDefaultDirectory    <- tempfile( "CyneRgy-user-examples-" )
    fOriginalSystemFile    <- base::system.file

    dir.create( strFakeExamplesPath, recursive = TRUE )
    expect_true( file.copy( strSourceExample, strFakeExamplesPath, recursive = TRUE ) )
    on.exit( unlink( c( strFakePackagePath, strDefaultDirectory ), recursive = TRUE ), add = TRUE )
    vOriginalOptions <- options( CyneRgy.examples.path = strDefaultDirectory )
    on.exit( options( vOriginalOptions ), add = TRUE )
    fSystemFile <- function( ..., package = "base", lib.loc = NULL, mustWork = FALSE ) {
        vPaths <- list( ... )
        if( identical( package, "CyneRgy" ) )
        {
            if( length( vPaths ) == 0 )
                return( strFakePackagePath )
            return( do.call( file.path, c( list( strFakePackagePath ), vPaths ) ) )
        }
        fOriginalSystemFile( ..., package = package, lib.loc = lib.loc, mustWork = mustWork )
    }
    fRunExample <- RunExample
    environment( fRunExample ) <- list2env( list( system.file = fSystemFile ), parent = environment( RunExample ) )

    expect_message(
        strCopiedPath <- fRunExample( "TreatmentSelection", bOpen = FALSE ),
        "Created writable CyneRgy example copy"
    )
    expect_true( dir.exists( strCopiedPath ) )
    expect_false( startsWith( strCopiedPath, normalizePath( strFakePackagePath, winslash = "/" ) ) )
    expect_message(
        strReusedPath <- fRunExample( "TreatmentSelection", bOpen = FALSE ),
        "Using existing CyneRgy example copy"
    )
    expect_identical( strReusedPath, strCopiedPath )
} )

test_that( "Every example has a matching project and concise introduction instructions", {
    strExamplesPath <- system.file( "Examples", package = "CyneRgy" )
    vExamples       <- list.dirs( strExamplesPath, recursive = FALSE, full.names = FALSE )

    for( strExample in vExamples )
    {
        strExamplePath   <- file.path( strExamplesPath, strExample )
        strProjectPath   <- file.path( strExamplePath, paste0( strExample, ".Rproj" ) )
        strDescription   <- paste( readLines( file.path( strExamplePath, "Description.Rmd" ), warn = FALSE ), collapse = "\n" )
        strRunCommand    <- paste0( 'CyneRgy::RunExample( "', strExample, '"' )
        nIntroduction    <- regexpr( "# Introduction", strDescription, fixed = TRUE )[ 1 ]
        vRunInstructions <- gregexpr( strRunCommand, strDescription, fixed = TRUE )[[ 1 ]]

        expect_true( file.exists( strProjectPath ), info = strExample )
        expect_false( grepl( "# Opening this example", strDescription, fixed = TRUE ), info = strExample )
        expect_true( nIntroduction > 0, info = strExample )
        expect_identical( sum( vRunInstructions > 0 ), 1L, info = strExample )
        expect_true( vRunInstructions[ 1 ] > nIntroduction, info = strExample )
    }
} )

test_that( "RunExample opens the matching project in RStudio", {
    strDefaultDirectory <- tempfile( "CyneRgy-rstudio-examples-" )
    vOriginalOptions    <- options( CyneRgy.examples.path = strDefaultDirectory )
    fOriginalGetenv     <- base::Sys.getenv
    vProjectCalls       <- list()

    on.exit( options( vOriginalOptions ), add = TRUE )
    on.exit( unlink( strDefaultDirectory, recursive = TRUE ), add = TRUE )

    local_mocked_bindings(
        Sys.getenv = function( x, unset = "", names = NA ) {
            if( length( x ) == 1 && x %in% c( "TERM_PROGRAM", "VSCODE_PID" ) )
                return( "" )
            fOriginalGetenv( x, unset = unset, names = names )
        },
        .package = "base"
    )
    local_mocked_bindings(
        isAvailable = function() TRUE,
        hasFun = function( strFunction ) strFunction %in% c( "openProject", "navigateToFile", "filesPaneNavigate" ),
        openProject = function( path, newSession ) {
            vProjectCalls[[ length( vProjectCalls ) + 1 ]] <<- list( path = path, newSession = newSession )
            invisible( NULL )
        },
        .package = "rstudioapi"
    )

    strExamplePath <- RunExample( "TreatmentSelection", bOpen = TRUE )
    strProjectPath <- file.path( strExamplePath, "TreatmentSelection.Rproj" )

    expect_length( vProjectCalls, 1 )
    expect_identical( normalizePath( vProjectCalls[[ 1 ]]$path, winslash = "/" ), strProjectPath )
    expect_true( vProjectCalls[[ 1 ]]$newSession )
} )

test_that( "RunExample uses the VS Code CLI before its rstudioapi emulation", {
    strDefaultDirectory <- tempfile( "CyneRgy-vscode-examples-" )
    vOriginalOptions    <- options( CyneRgy.examples.path = strDefaultDirectory )
    fOriginalGetenv     <- base::Sys.getenv
    vSystem2Calls       <- list()

    on.exit( options( vOriginalOptions ), add = TRUE )
    on.exit( unlink( strDefaultDirectory, recursive = TRUE ), add = TRUE )

    local_mocked_bindings(
        Sys.getenv = function( x, unset = "", names = NA ) {
            if( identical( x, "TERM_PROGRAM" ) )
                return( "vscode" )
            if( identical( x, "VSCODE_PID" ) )
                return( "123" )
            fOriginalGetenv( x, unset = unset, names = names )
        },
        Sys.which = function( x ) {
            vCommands        <- c( "code", "" )
            names( vCommands ) <- x
            vCommands
        },
        system2 = function( command, args, wait, stdout, stderr ) {
            vSystem2Calls[[ length( vSystem2Calls ) + 1 ]] <<- list( command = command, args = args )
            invisible( 0 )
        },
        .package = "base"
    )

    strExamplePath <- RunExample( "TreatmentSelection", bOpen = TRUE )
    vExpectedFiles <- c(
        sort( list.files( file.path( strExamplePath, "R" ), pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE ) ),
        file.path( strExamplePath, "Description.Rmd" )
    )

    expect_length( vSystem2Calls, 1 )
    expect_identical( vSystem2Calls[[ 1 ]]$command, "code" )
    expect_identical( vSystem2Calls[[ 1 ]]$args[[ 1 ]], "--new-window" )
    expect_length( vSystem2Calls[[ 1 ]]$args, length( vExpectedFiles ) + 2 )
} )
