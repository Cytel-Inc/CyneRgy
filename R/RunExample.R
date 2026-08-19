#################################################################################################### .
#' @name RunExample
#' @title Open a CyneRgy Example
#'
#' @description
#' Lists or opens an example stored in the CyneRgy package. Examples are self-contained folders in the repository; their functions
#' are not added to the CyneRgy package namespace. RStudio opens the example's project; VS Code opens its folder, R scripts, and
#' `Description.Rmd`. The R scripts do not require an RStudio project.
#'
#' When CyneRgy is installed normally, the example is copied to `~/CyneRgyExamples` and opened there; an existing copy is reused so
#' user changes are preserved. When CyneRgy is loaded from a development checkout with `pkgload`, the repository example is opened
#' directly. Supply `strDirectory` to choose another copy location. Calling `RunExample()` without an example name lists all
#' available examples. An existing example folder or file path can also be supplied.
#'
#' @param strExample Character string naming an included example, or an existing example folder or file path.
#' @param strDirectory Optional existing directory where an included example should be copied. The default, `NA`, uses the development
#' checkout directly or copies an installed example to the default user examples directory.
#' @param bOpen Logical value indicating whether to open the example in the active IDE. Defaults to `interactive()`.
#'
#' @return Invisibly returns the example path. When called without `strExample`, invisibly returns the available example names.
#'
#' @details
#' Set `options( CyneRgy.examples.path = "path" )` to change the default user examples directory from `~/CyneRgyExamples`.
#' RStudio Desktop and supported browser-based RStudio sessions use `rstudioapi` to open a matching `.Rproj` file. Positron can use
#' its supported `rstudioapi` hooks, and VS Code uses the `code` command when it is available on `PATH`. Other IDEs can register an opener with
#' `options( CyneRgy.path.opener = function( strPath ) ... )`. If no integration is available, the function returns and displays the
#' path so it can be opened manually.
#'
#' @examplesIf interactive()
#' RunExample()
#' RunExample( "TreatmentSelection" )
#' RunExample( "TreatmentSelection", strDirectory = getwd() )
#'
#' @export
#################################################################################################### .

RunExample <- function( strExample = "", strDirectory = NA, bOpen = interactive() )
{
    strPackage      <- "CyneRgy"
    strPackagePath  <- system.file( package = strPackage )
    strExamplesPath <- system.file( "Examples", package = strPackage )
    strPackagePath  <- normalizePath( strPackagePath, winslash = "/", mustWork = TRUE )
    strExamplesPath <- normalizePath( strExamplesPath, winslash = "/", mustWork = TRUE )
    vValidExamples  <- list.dirs( strExamplesPath, recursive = FALSE, full.names = FALSE )
    vValidExamples  <- sort( vValidExamples )

    strDevelopmentRoot         <- dirname( strPackagePath )
    strDevelopmentExamplesPath <- file.path( strPackagePath, "Examples" )
    bDevelopmentCheckout       <- identical( tolower( basename( strPackagePath ) ), "inst" ) &&
                                  file.exists( file.path( strDevelopmentRoot, "DESCRIPTION" ) ) &&
                                  dir.exists( strDevelopmentExamplesPath ) &&
                                  identical( normalizePath( strDevelopmentExamplesPath, winslash = "/", mustWork = TRUE ),
                                             strExamplesPath )

    if( length( strExample ) == 1 && !is.na( strExample ) && !nzchar( strExample ) )
    {
        message( "Available CyneRgy examples:\n", paste0( "- ", vValidExamples, collapse = "\n" ) )
        return( invisible( vValidExamples ) )
    }

    if( length( strExample ) != 1 || is.na( strExample ) )
        stop( "strExample must be one example name or an existing path.", call. = FALSE )
    if( length( strDirectory ) != 1 )
        stop( "strDirectory must be a single directory path or NA.", call. = FALSE )

    bExistingPath <- file.exists( strExample )
    if( bExistingPath )
    {
        if( !is.na( strDirectory ) )
            stop( "strDirectory cannot be used when strExample is already a path.", call. = FALSE )
        strExamplePath <- normalizePath( strExample, winslash = "/", mustWork = TRUE )
    }
    else
    {
        if( !strExample %in% vValidExamples )
        {
            stop( "Unknown CyneRgy example: '", strExample,
                  "'. Call RunExample() to list the available examples.", call. = FALSE )
        }

        strExamplePath <- file.path( strExamplesPath, strExample )
        bDefaultCopy   <- is.na( strDirectory ) && !bDevelopmentCheckout
        if( bDefaultCopy )
            strDirectory <- getOption( "CyneRgy.examples.path", file.path( path.expand( "~" ), "CyneRgyExamples" ) )

        if( length( strDirectory ) != 1 || ( !is.na( strDirectory ) && !nzchar( strDirectory ) ) )
            stop( "strDirectory must be a single non-empty directory path or NA.", call. = FALSE )
        if( bDefaultCopy && is.na( strDirectory ) )
            stop( "The CyneRgy.examples.path option must be a non-empty directory path.", call. = FALSE )

        if( !is.na( strDirectory ) )
        {
            if( bDefaultCopy && !dir.exists( strDirectory ) && !dir.create( strDirectory, recursive = TRUE ) )
                stop( "The default CyneRgy examples directory could not be created: ", strDirectory, call. = FALSE )
            if( !dir.exists( strDirectory ) )
                stop( "strDirectory does not exist: ", strDirectory, call. = FALSE )

            strDirectory   <- normalizePath( strDirectory, winslash = "/", mustWork = TRUE )
            strExamplePath <- file.path( strDirectory, strExample )
            if( file.exists( strExamplePath ) )
            {
                if( bDefaultCopy )
                    message( "Using existing CyneRgy example copy: ", strExamplePath )
                else
                    stop( "The destination already exists: ", strExamplePath, call. = FALSE )
            }
            else
            {
                bCopied <- file.copy( file.path( strExamplesPath, strExample ), strDirectory, recursive = TRUE )
                if( !bCopied || !dir.exists( strExamplePath ) )
                    stop( "The example could not be copied to: ", strDirectory, call. = FALSE )
                message( "Created writable CyneRgy example copy: ", strExamplePath )
            }
        }
        strExamplePath <- normalizePath( strExamplePath, winslash = "/", mustWork = TRUE )
    }

    if( !isTRUE( bOpen ) )
    {
        message( "CyneRgy example: ", strExamplePath )
        return( invisible( strExamplePath ) )
    }

    bIsDirectory   <- dir.exists( strExamplePath )
    vFilesToOpen   <- strExamplePath
    strProjectFile <- ""
    if( bIsDirectory )
    {
        strRDirectory   <- file.path( strExamplePath, "R" )
        vRFiles         <- character()
        if( dir.exists( strRDirectory ) )
            vRFiles <- sort( list.files( strRDirectory, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE ) )
        strDescription  <- file.path( strExamplePath, "Description.Rmd" )
        vFilesToOpen    <- c( vRFiles, strDescription )
        vFilesToOpen    <- unique( vFilesToOpen[ file.exists( vFilesToOpen ) ] )

        vProjectFiles          <- list.files( strExamplePath, pattern = "\\.[Rr]proj$", full.names = TRUE )
        strExpectedProjectFile <- file.path( strExamplePath, paste0( basename( strExamplePath ), ".Rproj" ) )
        if( file.exists( strExpectedProjectFile ) )
            strProjectFile <- strExpectedProjectFile
        else if( length( vProjectFiles ) == 1 )
            strProjectFile <- vProjectFiles[[ 1 ]]
    }

    fCustomOpener <- getOption( "CyneRgy.path.opener" )
    if( is.function( fCustomOpener ) )
    {
        fCustomOpener( strExamplePath )
        return( invisible( strExamplePath ) )
    }

    strTermProgram <- tolower( Sys.getenv( "TERM_PROGRAM" ) )
    bIsVSCode      <- identical( strTermProgram, "vscode" ) || nzchar( Sys.getenv( "VSCODE_PID" ) )

    # VS Code emulates rstudioapi, but navigateToFile reuses its preview editor. Its CLI opens all files as separate tabs.
    if( bIsVSCode )
    {
        vCommands <- c( "code", "code-insiders" )
        strCommand <- vCommands[ nzchar( Sys.which( vCommands ) ) ][ 1 ]
        if( !is.na( strCommand ) )
        {
            vPathsToOpen <- if( bIsDirectory ) c( strExamplePath, vFilesToOpen ) else strExamplePath
            system2( strCommand, c( "--new-window", shQuote( vPathsToOpen ) ), wait = FALSE, stdout = FALSE, stderr = FALSE )
            return( invisible( strExamplePath ) )
        }
    }

    # RStudio Desktop, RStudio Server, Posit Workbench and Positron expose supported IDE actions through rstudioapi.
    if( requireNamespace( "rstudioapi", quietly = TRUE ) && rstudioapi::isAvailable() )
    {
        if( nzchar( strProjectFile ) && rstudioapi::hasFun( "openProject" ) )
        {
            tryCatch(
                rstudioapi::openProject( strProjectFile, newSession = TRUE ),
                error = function( e ) rstudioapi::openProject( strProjectFile, newSession = FALSE )
            )
            return( invisible( strExamplePath ) )
        }

        if( bIsDirectory )
        {
            if( rstudioapi::hasFun( "filesPaneNavigate" ) )
                try( rstudioapi::filesPaneNavigate( strExamplePath ), silent = TRUE )

            if( length( vFilesToOpen ) > 0 && rstudioapi::hasFun( "navigateToFile" ) )
            {
                for( strFileToOpen in vFilesToOpen )
                    rstudioapi::navigateToFile( strFileToOpen )
                return( invisible( strExamplePath ) )
            }
        }
        else if( rstudioapi::hasFun( "navigateToFile" ) )
        {
            rstudioapi::navigateToFile( strExamplePath )
            return( invisible( strExamplePath ) )
        }
    }

    if( length( vFilesToOpen ) > 0 && all( file.exists( vFilesToOpen ) ) )
        file.show( vFilesToOpen )
    else
        warning( "No supported IDE opener was detected. The files are available at: ", strExamplePath, call. = FALSE )

    return( invisible( strExamplePath ) )
}
