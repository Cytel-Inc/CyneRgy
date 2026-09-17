#################################################################################################### .
#   Program/Function Name: CreateCyneRgyFunction
#   Author: J. Kyle Wathen
#   Description: Create an R file from a CyneRgy integration-point template.
#################################################################################################### .
#' @name CreateCyneRgyFunction
#' @title Create a CyneRgy Function from a Template
#'
#' @description Creates an R script from one of the integration-point templates included with CyneRgy. Call the function without a
#' template name to list the available templates.
#'
#' @param strFunctionType Character string naming the template to use.
#' @param strNewFunctionName Character string used for the function and file name. Defaults to `strFunctionType`.
#' @param strDirectory Directory where the file should be created. Defaults to the current working directory.
#' @param bOpen Logical value indicating whether to open the new file in the active IDE. Defaults to `interactive()`.
#'
#' @return Invisibly returns the created file path. When called without `strFunctionType`, invisibly returns the available template names.
#'
#' @examplesIf interactive()
#' CreateCyneRgyFunction()
#' CreateCyneRgyFunction( "Analyze.Binary", "NewBinaryAnalysis" )
#'
#' @export
#################################################################################################### .

CreateCyneRgyFunction <- function( strFunctionType = "", strNewFunctionName = NA, strDirectory = NA, bOpen = interactive() )
{
    strPackage            <- "CyneRgy"
    vTemplateFiles        <- list.files( system.file( "Templates", package = strPackage ), full.names = TRUE )
    vValidFunctionTypes   <- tools::file_path_sans_ext( basename( vTemplateFiles ) )

    if( length( strFunctionType ) == 1 && !is.na( strFunctionType ) && !nzchar( strFunctionType ) )
    {
        message( "Available CyneRgy templates:\n", paste0( "- ", vValidFunctionTypes, collapse = "\n" ) )
        return( invisible( vValidFunctionTypes ) )
    }

    if( length( strFunctionType ) != 1 || is.na( strFunctionType ) || !strFunctionType %in% vValidFunctionTypes )
        stop( "Unknown CyneRgy template: '", paste( strFunctionType, collapse = "', '" ),
              "'. Call CreateCyneRgyFunction() to list the available templates.", call. = FALSE )

    if( length( strNewFunctionName ) != 1 || is.na( strNewFunctionName ) || !nzchar( strNewFunctionName ) )
        strNewFunctionName <- strFunctionType
    if( grepl( "[/\\\\]", strNewFunctionName ) )
        stop( "strNewFunctionName must be a file name, not a path.", call. = FALSE )

    if( length( strDirectory ) != 1 || is.na( strDirectory ) )
        strDirectory <- getwd()
    if( !dir.exists( strDirectory ) )
        stop( "strDirectory does not exist: ", strDirectory, call. = FALSE )

    strDirectory       <- normalizePath( strDirectory, winslash = "/", mustWork = TRUE )
    strSelectedTemplate <- vTemplateFiles[ match( strFunctionType, vValidFunctionTypes ) ]
    strNewFilePath      <- file.path( strDirectory, paste0( strNewFunctionName, ".R" ) )
    nFileIndex          <- 0
    while( file.exists( strNewFilePath ) )
    {
        nFileIndex     <- nFileIndex + 1
        strNewFilePath <- file.path( strDirectory, paste0( strNewFunctionName, nFileIndex, ".R" ) )
    }

    if( !file.copy( strSelectedTemplate, strNewFilePath ) )
        stop( "The template could not be copied to: ", strNewFilePath, call. = FALSE )

    vTags    <- c( "FUNCTION_NAME", "CREATION_DATE" )
    vReplace <- c( strNewFunctionName, format( Sys.Date(), format = "%m/%d/%Y" ) )
    ReplaceTagsInFile( strNewFilePath, vTags, vReplace )

    strNewFilePath <- normalizePath( strNewFilePath, winslash = "/", mustWork = TRUE )
    message( "Created CyneRgy function: ", strNewFilePath )
    if( isTRUE( bOpen ) )
        RunExample( strNewFilePath )

    return( invisible( strNewFilePath ) )
}
