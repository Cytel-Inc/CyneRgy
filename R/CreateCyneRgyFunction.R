#################################################################################################### .
#   Program/Function Name: CreateCyneRgyFuntion
#   Author: J. Kyle Wathen
#   Description: This function will create a new file containing the template for the desired CyneRgy function.
#   Change History:
#   Last Modified Date: 12/19/2023
#################################################################################################### .
#' Create a new CyneRgy function using provided templates that can be utilized with R integration points in Cytel products. 
#' @description {Create a new CyneRgy function using provided templates that can be utilized with R integration points in Cytel products.}
#' @param strFunctionType A string variable that provides the function type to create.   This argument is used to determine which integration template should be used. 
#' @param strNewFunctionName The name of the new function to create.   This argument is also used to name the file.  The resulting file is named [strNewFunctionName].R
#' @param strDirectory A sub-directory with this name is created in the current working directory.  If no value is provided, the file is created in the working directory. 
#' @param bOpen TRUE/FALSE value, when TRUE the file is opened and when FALSE the file is not opened, just created. Note, the R Studio API is used to open the file and only works in R Studio.
#' @examples \dontrun{
#' CreateCyneRgyFunction()  # A full list of options for strFunctionType is provided
#' 
#' # Using the Analyze.Binary function template a new file named NewBinaryAnalysis.R is created in the working directory with a function that is ready to
#' # be used in places where binary data is generated in simulations. 
#' CreateCyneRgyFunction( "Analyze.Binary", "NewBinaryAnalysis" )  
#' }
#' @export
CreateCyneRgyFunction <- function( strFunctionType = "", strNewFunctionName = NA, strDirectory = NA, bOpen = TRUE)
{
    if( is.na(strNewFunctionName) || strNewFunctionName == "" )
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
    vValidExamples         <- tools::file_path_sans_ext(list.files(system.file("Templates", package = strPackage)) )
    vValidExamplesFullPath <- list.files(system.file("Templates", package = strPackage), full.names = TRUE) 
    
    validExamplesMsg <-
        paste0(
            "Valid values for strFunctionType are: '",
            paste(vValidExamples, collapse = "', '"),
            "'")

    
    # Check if strFunctionType is a valid example
    if ( missing( strFunctionType ) || !nzchar(strFunctionType) || !(strFunctionType %in% vValidExamples) ) 
    {
        print( paste0( 
            'Please run `CreateCyneRgyFunction()` with a valid strFunctionType argument name.',
            validExamplesMsg ))
        return()
    }
    
    # Find the full path of the selected example
    strSelectedExample <- vValidExamplesFullPath[grep(strFunctionType, vValidExamples)]
    
    # Check if the file already exists in the destination directory
    if (!is.na(strDirectory) && file.exists(file.path(strDirectory, basename(strSelectedExample)))) {
        stop("File already exists in the destination directory.")
    }
    
    # Determine the destination directory
    if (is.na(strDirectory)) {
        strDirectory <- getwd()  # Use the current working directory if not specified
    }
    
    # Create the full path for the new file
    strNewFilePath <- paste0(strDirectory, "/",ifelse(strNewFileName == "", basename(strSelectedExample), strNewFileName), strNewFileExt)
    
    # Check if the file name exists and if so update it
    # Create the file name
    bFileExists <- file.exists( strNewFilePath )
    # Need to find the file name since it already exists
    nIndex <- 0
    while( bFileExists )
    {
        nIndex      <- nIndex + 1
        #strFileName <- paste( strPkgDir, "/R/", strFunctionName, nIndex, ".R", sep ="" )
        strNewFilePath <- paste0(strDirectory, "/",ifelse(strNewFileName == "", basename(strSelectedExample), strNewFileName), nIndex,  strNewFileExt)
        
        bFileExists <- file.exists( strNewFilePath )
    }
    
    # Copy the file to the destination directory
    file.copy(strSelectedExample, strNewFilePath )
    
    # Print a message indicating success
    cat("File copied successfully to:", strNewFilePath, "\n")
    
    strToday           <- format(Sys.Date(), format="%m/%d/%Y")
    
    # Update the tags in the file that was copied  
    vTags    <- c("FUNCTION_NAME",  "CREATION_DATE")
    vReplace <- c(strNewFunctionName, strToday)
    ReplaceTagsInFile( strNewFilePath, vTags, vReplace )
    
    if( bOpen )
    {
        # Open the file in RStudio
        strIgnore <- rstudioapi::navigateToFile( strNewFilePath )
    }
}