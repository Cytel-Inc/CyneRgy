#################################################################################################### .
#   Program/Function Name: CreateCyneRgyExample
#   Author: Subhajit Sengupta
#   Description: Create a starter example folder from a CyneRgy integration-point template.
#################################################################################################### .
#' @name CreateCyneRgyExample
#' @title Create a CyneRgy Example Folder
#'
#' @description Creates an example folder containing `Description.Rmd`, an `R` directory with a selected integration-point
#' template, and a matching RStudio project by default. The R scripts do not depend on the project file.
#'
#' @param strFunctionType Character string naming the integration-point template to use.
#' @param strNewExampleName Character string naming the new example folder and starter function.
#' @param strDirectory Existing parent directory where the example should be created. Defaults to the current working directory.
#' @param bCreateProject Logical value indicating whether to include an RStudio project file. Defaults to `TRUE`.
#' @param bOpen Logical value indicating whether to open the new example in the active IDE. Defaults to `interactive()`.
#'
#' @return Invisibly returns the created example path. When called without `strFunctionType`, invisibly returns the available template names.
#'
#' @examplesIf interactive()
#' CreateCyneRgyExample()
#' CreateCyneRgyExample( "Analyze.Binary", "MyBinaryAnalysis" )
#' CreateCyneRgyExample( "Analyze.Binary", "MyBinaryFolder", bCreateProject = FALSE )
#'
#' @export
#################################################################################################### .

CreateCyneRgyExample <- function( strFunctionType = "", strNewExampleName = "", strDirectory = NA,
                                  bCreateProject = TRUE, bOpen = interactive() )
{
    strPackage          <- "CyneRgy"
    vTemplateFiles      <- list.files( system.file( "Templates", package = strPackage ), full.names = TRUE )
    vValidFunctionTypes <- tools::file_path_sans_ext( basename( vTemplateFiles ) )

    if( length( strFunctionType ) == 1 && !is.na( strFunctionType ) && !nzchar( strFunctionType ) )
    {
        message( "Available CyneRgy templates:\n", paste0( "- ", vValidFunctionTypes, collapse = "\n" ) )
        return( invisible( vValidFunctionTypes ) )
    }

    if( length( strFunctionType ) != 1 || is.na( strFunctionType ) || !strFunctionType %in% vValidFunctionTypes )
        stop( "Unknown CyneRgy template: '", paste( strFunctionType, collapse = "', '" ),
              "'. Call CreateCyneRgyExample() to list the available templates.", call. = FALSE )
    if( length( strNewExampleName ) != 1 || is.na( strNewExampleName ) || !nzchar( strNewExampleName ) )
        stop( "strNewExampleName is required.", call. = FALSE )
    if( grepl( "[/\\\\]", strNewExampleName ) )
        stop( "strNewExampleName must be a folder name, not a path.", call. = FALSE )

    if( length( strDirectory ) != 1 || is.na( strDirectory ) )
        strDirectory <- getwd()
    if( !dir.exists( strDirectory ) )
        stop( "strDirectory does not exist: ", strDirectory, call. = FALSE )

    strDirectory      <- normalizePath( strDirectory, winslash = "/", mustWork = TRUE )
    strNewExamplePath <- file.path( strDirectory, strNewExampleName )
    if( file.exists( strNewExamplePath ) )
        stop( "The destination already exists: ", strNewExamplePath, call. = FALSE )

    if( !dir.create( strNewExamplePath ) )
        stop( "The example directory could not be created: ", strNewExamplePath, call. = FALSE )

    bCreationComplete <- FALSE
    on.exit( if( !bCreationComplete ) unlink( strNewExamplePath, recursive = TRUE ), add = TRUE )

    strExampleTemplatePath <- system.file( "ExampleTemplate", package = strPackage )
    vExampleTemplateFiles  <- list.files( strExampleTemplatePath, all.files = TRUE, no.. = TRUE, full.names = TRUE )
    if( length( vExampleTemplateFiles ) > 0 && !all( file.copy( vExampleTemplateFiles, strNewExamplePath, recursive = TRUE ) ) )
        stop( "The example template could not be copied.", call. = FALSE )

    strRDirectory <- file.path( strNewExamplePath, "R" )
    if( !dir.create( strRDirectory ) )
        stop( "The example R directory could not be created.", call. = FALSE )

    strSelectedTemplate <- vTemplateFiles[ match( strFunctionType, vValidFunctionTypes ) ]
    strRCodeFileName    <- file.path( strRDirectory, paste0( strNewExampleName, ".R" ) )
    if( !file.copy( strSelectedTemplate, strRCodeFileName ) )
        stop( "The selected function template could not be copied.", call. = FALSE )

    strProjectTemplate <- file.path( strNewExamplePath, "Example.Rproj" )
    if( isTRUE( bCreateProject ) )
    {
        strProjectFile <- file.path( strNewExamplePath, paste0( strNewExampleName, ".Rproj" ) )
        if( !file.rename( strProjectTemplate, strProjectFile ) )
            stop( "The RStudio project file could not be created.", call. = FALSE )
    }
    else if( file.exists( strProjectTemplate ) )
    {
        unlink( strProjectTemplate )
    }

    vTags    <- c( "FUNCTION_NAME", "CREATION_DATE" )
    vReplace <- c( strNewExampleName, format( Sys.Date(), format = "%m/%d/%Y" ) )
    ReplaceTagsInFile( strRCodeFileName, vTags, vReplace )
    ReplaceTagsInFile( file.path( strNewExamplePath, "Description.Rmd" ), "EXAMPLE_NAME", strNewExampleName )

    bCreationComplete <- TRUE
    strNewExamplePath <- normalizePath( strNewExamplePath, winslash = "/", mustWork = TRUE )
    message( "Created CyneRgy example: ", strNewExamplePath )
    if( isTRUE( bOpen ) )
        RunExample( strNewExamplePath )

    return( invisible( strNewExamplePath ) )
}
