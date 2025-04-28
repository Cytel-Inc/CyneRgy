#################################################################################################### .
#   Program/Function Name: CreateCyneRgyFuntion
#   Author: J. Kyle Wathen
#   Description: This function will create a new file containing the template for the desired CyneRgy function.
#   Change History:
#   Last Modified Date: 12/19/2023
#################################################################################################### .
#' @name CreateCyneRgyFunction
#' @title Create a CyneRgy Function Template
#'
#' @description This function generates a new R script file containing a template for a CyneRgy function, 
#' which can be integrated with Cytel products. The template is selected based on the provided function type.
#' 
#' @param strFunctionType A string specifying the type of function to create. This determines the integration 
#' template to use. Valid values for `strFunctionType` can be obtained from the available templates.
#' @param strNewFunctionName The name of the new function to be created. This also determines the name of the 
#' resulting file, which will be named `[strNewFunctionName].R`. If no value is provided, the default name 
#' will be derived from `strFunctionType`.
#' @param strDirectory The directory where the new file will be created. If not provided, the file will be created 
#' in the current working directory. A sub-directory with this name will also be created if it does not exist.
#' @param bOpen Logical value (TRUE/FALSE). When TRUE, the newly created file will be opened in RStudio using 
#' the RStudio API (works only in RStudio). When FALSE, the file will be created but not opened.
#' 
#' @examples 
#' \dontrun{
#' CreateCyneRgyFunction()  # Run without arguments to see valid options for `strFunctionType`.
#' 
#' # Example: Create a new file named `NewBinaryAnalysis.R` using the `Analyze.Binary` template.
#' CreateCyneRgyFunction("Analyze.Binary", "NewBinaryAnalysis")
#' }
#' 
#' @export
#################################################################################################### .

CreateCyneRgyFunction <- function( strFunctionType = "", strNewFunctionName = NA, strDirectory = NA, bOpen = TRUE )
{
    if( is.na( strNewFunctionName ) || strNewFunctionName == "" )
    {
        strNewFunctionName <- strFunctionType
    }
    strNewFileExt  <- ".R"
    strNewFileName <- strNewFunctionName
    
    
    
    #TODO: Make sure the strFunctionType is a valid type, eg PatientSimulator, Analysis ect
    # Make sure the file does not already exist
    # create it and open it
    strPackage <- "CyneRgy"
    
    # Existing template names, remove extensions
    vValidExamples         <- tools::file_path_sans_ext( list.files( system.file( "Templates", package = strPackage ) ) )
    vValidExamplesFullPath <- list.files( system.file( "Templates", package = strPackage ), full.names = TRUE ) 
    
    validExamplesMsg <-
        paste0(
            "Valid values for strFunctionType are: '",
            paste( vValidExamples, collapse = "', '" ),
            "'" )

    
    # Check if strFunctionType is a valid example
    if ( missing( strFunctionType ) || !nzchar( strFunctionType ) || !( strFunctionType %in% vValidExamples ) ) 
    {
        print( paste0( 
            'Please run `CreateCyneRgyFunction()` with a valid strFunctionType argument name.',
            validExamplesMsg ) )
        return()
    }
    
    # Find the full path of the selected example
    strSelectedExample <- vValidExamplesFullPath[ grep( strFunctionType, vValidExamples ) ]
    
    # Check if the file already exists in the destination directory
    if ( !is.na( strDirectory ) && file.exists( file.path( strDirectory, basename( strSelectedExample ) ) ) ) {
        stop( "File already exists in the destination directory." )
    }
    
    # Determine the destination directory
    if ( is.na( strDirectory ) ) {
         strDirectory <- getwd()  # Use the current working directory if not specified
    }
    
    # Create the full path for the new file
    strNewFilePath <- paste0( strDirectory, "/",ifelse( strNewFileName == "", basename( strSelectedExample ), strNewFileName ), strNewFileExt )
    
    # Check if the file name exists and if so update it
    # Create the file name
    bFileExists <- file.exists( strNewFilePath )
    # Need to find the file name since it already exists
    nIndex <- 0
    while( bFileExists )
    {
        nIndex      <- nIndex + 1
        #strFileName <- paste( strPkgDir, "/R/", strFunctionName, nIndex, ".R", sep ="" )
        strNewFilePath <- paste0( strDirectory, "/", ifelse( strNewFileName == "", basename( strSelectedExample ), strNewFileName ), nIndex, strNewFileExt )
        
        bFileExists <- file.exists( strNewFilePath )
    }
    
    # Copy the file to the destination directory
    file.copy( strSelectedExample, strNewFilePath )
    
    # Print a message indicating success
    cat( "File copied successfully to:", strNewFilePath, "\n" )
    
    strToday           <- format( Sys.Date(), format="%m/%d/%Y" )
    
    # Update the tags in the file that was copied  
    vTags    <- c( "FUNCTION_NAME",  "CREATION_DATE" )
    vReplace <- c( strNewFunctionName, strToday )
    ReplaceTagsInFile( strNewFilePath, vTags, vReplace )
    
    if( bOpen )
    {
        # Open the file in RStudio
        strIgnore <- rstudioapi::navigateToFile( strNewFilePath )
    }
}